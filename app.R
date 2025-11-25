library(shiny)
library(bslib)
library(fpp3)
library(tidyverse)
library(gt)
library(shinyjs)
library(urca)

# Load and prepare data
aus_wine <- read_csv("AustralianWines.csv", na = "*", show_col_types = FALSE) |>
  fill(where(is.numeric), .direction = "down") |>
  mutate(Month = mdy(str_replace(Month, '-', '-01-')) |> yearmonth())

all_wine <- aus_wine |>
  pivot_longer(
    cols = c(Fortified, Red, Rose, sparkling, `Sweet white`, `Dry white`),
    names_to = "Varietal",
    values_to = "Sales"
  ) |>
  as_tsibble(index = Month, key = Varietal)

varietals <- unique(all_wine$Varietal)
date_range <- range(all_wine$Month)

ui <- page_sidebar(
  title = "Australian Wine Sales Analysis",
  useShinyjs(),
  sidebar = sidebar(
    div(
      id = "viz_controls",
      checkboxGroupInput(
        "viz_varietals",
        "Select Varietals:",
        choices = setNames(varietals, varietals),
        selected = varietals[1:2]
      ),
      dateRangeInput(
        "viz_date_range",
        "Date Range:",
        start = as_date(date_range[1]),
        end = as_date(date_range[2]),
        min = as_date(date_range[1]),
        max = as_date(date_range[2])
      )
    ),
    div(
      id = "model_controls",
      style = "display: none;",
      dateInput(
        "train_cutoff",
        "Training Cutoff Date:",
        value = as_date(yearmonth("1993 Dec")),
        min = as_date(date_range[1]),
        max = as_date(date_range[2])
      ),
      checkboxInput(
        "show_model_spec",
        "Show Model Specification",
        value = FALSE
      ),
      checkboxInput(
        "show_train_acc",
        "Show Training Accuracy",
        value = FALSE
      )
    ),
    div(
      id = "forecast_controls",
      style = "display: none;",
      selectInput(
        "forecast_model",
        "Select Model:",
        choices = c("arima", "ets", "tslm"),
        selected = "arima"
      ),
      numericInput(
        "forecast_months",
        "Forecast Horizon (months):",
        value = 12,
        min = 1,
        max = 36
      )
    )
  ),
  
  navset_tab(
    nav_panel(
      "Visualizations",
      card(
        card_header("Wine Sales Data"),
        plotOutput("viz_plot", height = "600px")
      )
    ),
    nav_panel(
      "Model Building",
      card(
        card_header("Model Specification"),
        uiOutput("model_spec_card")
      ),
      card(
        card_header("Training Accuracy"),
        uiOutput("train_accuracy_card")
      ),
      card(
        card_header("Forecast Accuracy"),
        uiOutput("forecast_accuracy_out")
      )
    ),
    nav_panel(
      "Forecast",
      card(
        card_header("Forecast Visualization"),
        plotOutput("forecast_plot", height = "600px")
      ),
      card(
        card_header("Forecast Table"),
        uiOutput("forecast_table_out")
      )
    ),
    id = "main_nav"
  )
)

server <- function(input, output, session) {
  
  # Show/hide sidebar controls based on active tab
  observe({
    tab <- input$main_nav
    shinyjs::hide("viz_controls")
    shinyjs::hide("model_controls")
    shinyjs::hide("forecast_controls")
    
    if (tab == "Visualizations") {
      shinyjs::show("viz_controls")
    } else if (tab == "Model Building") {
      shinyjs::show("model_controls")
    } else if (tab == "Forecast") {
      shinyjs::show("forecast_controls")
    }
  })
  
  # Reactive: Filtered data for visualization
  viz_data <- reactive({
    req(input$viz_varietals, input$viz_date_range)
    all_wine |>
      filter(
        Varietal %in% input$viz_varietals,
        Month >= yearmonth(input$viz_date_range[1]),
        Month <= yearmonth(input$viz_date_range[2])
      )
  })
  
  # Visualization plot
  output$viz_plot <- renderPlot({
    req(nrow(viz_data()) > 0)
    viz_data() |>
      autoplot(Sales) +
      labs(y = "Sales Volume", title = "Wine Sales Data") +
      facet_wrap(~Varietal, ncol = 2, scales = "free_y") +
      theme_minimal(base_size = 12) +
      theme(
        plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        strip.text = element_text(face = "bold", size = 12),
        panel.grid.minor = element_blank()
      )
  })
  
  # Reactive: Train/validation split
  train_data <- reactive({
    req(input$train_cutoff, input$viz_varietals)
    train_cutoff <- yearmonth(input$train_cutoff)
    all_wine |>
      filter(
        Month <= train_cutoff,
        Varietal %in% input$viz_varietals
      )
  })
  
  val_data <- reactive({
    req(input$train_cutoff, input$viz_varietals)
    train_cutoff <- yearmonth(input$train_cutoff)
    all_wine |>
      filter(
        Month > train_cutoff,
        Varietal %in% input$viz_varietals
      )
  })
  
  # Reactive: Model fits
  fit_models <- reactive({
    req(nrow(train_data()) > 0)
    train_data() |>
      model(
        arima = ARIMA(Sales),
        ets = ETS(Sales),
        tslm = TSLM(Sales ~ trend() + season())
      )
  })
  
  # Model specification (conditional)
  output$model_spec_card <- renderUI({
    if (input$show_model_spec) {
      verbatimTextOutput("model_spec_out")
    }
  })
  
  output$model_spec_out <- renderPrint({
    req(fit_models())
    fit_models()
  })
  
  # Training accuracy (conditional)
  output$train_accuracy_card <- renderUI({
    if (input$show_train_acc) {
      uiOutput("train_accuracy_out")
    }
  })
  
  # Training accuracy
  output$train_accuracy_out <- renderUI({
    req(fit_models())
    fit_models() |>
      accuracy() |>
      select(Varietal, .model, RMSE, MAE, MAPE) |>
      arrange(Varietal, RMSE) |>
      gt() |>
      as_raw_html() |>
      HTML()
  })
  
  # Forecast accuracy
  output$forecast_accuracy_out <- renderUI({
    req(forecasts(), nrow(val_data()) > 0)
    forecasts() |>
      accuracy(all_wine) |>
      select(Varietal, .model, RMSE, MAE, MAPE) |>
      arrange(Varietal, RMSE) |>
      gt() |>
      as_raw_html() |>
      HTML()
  })
  
  # Reactive: Forecasts
  forecasts <- reactive({
    req(fit_models(), input$forecast_months)
    fit_models() |>
      forecast(h = input$forecast_months)
  })
  
  # Forecast table
  output$forecast_table_out <- renderUI({
    req(forecasts(), input$forecast_model)
    forecasts() |>
      filter(.model == input$forecast_model) |>
      as_tibble() |>
      select(Varietal, Month, .mean) |>
      pivot_wider(names_from = Month, values_from = .mean) |>
      gt() |>
      fmt_number(decimals = 0) |>
      as_raw_html() |>
      HTML()
  })
  
  # Forecast visualization
  output$forecast_plot <- renderPlot({
    req(forecasts(), input$train_cutoff)
    train_cutoff <- yearmonth(input$train_cutoff)
    trn_start <- yearmonth(as_date(train_cutoff) - years(1))
    
    forecasts() |>
      autoplot(all_wine |> filter(Month >= trn_start)) +
      geom_vline(xintercept = as_date(train_cutoff), color = "red", linetype = "dashed") +
      labs(y = "Sales Volume", title = "Wine Sales Forecast") +
      facet_wrap(Varietal ~ .model, ncol = 3, scales = "free_y") +
      theme_minimal(base_size = 12) +
      theme(
        plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        strip.text = element_text(face = "bold", size = 10),
        panel.grid.minor = element_blank()
      )
  })
}

shinyApp(ui = ui, server = server)