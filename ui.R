css_vars <- sprintf("
  :root {
    --page: %s;
    --surface: %s;
    --grid: %s;
    --axis: %s;
    --text-pri: %s;
    --text-sec: %s;
    --text-mute: %s;
    --actual: %s;
    --warning: %s;
  }
", COL_PAGE, COL_SURFACE, COL_GRID, COL_AXIS, COL_TEXT_PRI, COL_TEXT_SEC,
   COL_TEXT_MUTE, COL_ACTUAL, COL_WARNING)

app_css <- "
  body {
    background-color: var(--page);
    color: var(--text-pri);
    font-family: system-ui, -apple-system, 'Segoe UI', Roboto, sans-serif;
  }
  .app { max-width: 1200px; margin: 0 auto; padding: 28px 16px 40px; }

  .app-header h1 { font-size: 24px; font-weight: 600; margin: 0 0 4px; }
  .app-header h1::before {
    content: ''; display: inline-block; width: 6px; height: 22px; border-radius: 3px;
    background-color: var(--actual); margin-right: 10px; vertical-align: -3px;
  }
  .app-header p { color: var(--text-sec); margin: 0 0 20px; }

  .card {
    background-color: var(--surface);
    border: 1px solid var(--grid);
    border-radius: 10px;
    box-shadow: 0 1px 2px rgba(15, 27, 45, 0.04);
    padding: 16px 20px;
    margin-bottom: 16px;
  }

  /* Filters: one row, model dropdown shown only for forecast dates */
  .filters-row { display: flex; flex-wrap: wrap; gap: 12px 24px; align-items: flex-start; }
  .filters-row .form-group { margin-bottom: 0; }
  .filters-row .shiny-input-container { width: 100% !important; }
  .filters-row label {
    display: block; margin: 0 0 6px; line-height: 18px;
    font-size: 13px; font-weight: 600; color: var(--text-sec);
  }
  .filters-row input.form-control,
  .filters-row .selectize-input {
    height: 36px; min-height: 36px; padding: 7px 12px; font-size: 14px; line-height: 20px;
    border-color: var(--axis); border-radius: 6px; box-shadow: none;
  }
  .filters-row .selectize-input > input { height: 20px; }
  .filter-date { width: 200px; }
  .filter-model { width: 240px; }
  .filters-note { margin: 12px 0 0; font-size: 13px; color: var(--text-mute); }

  /* Stat tiles */
  .stats {
    display: grid;
    grid-template-columns: repeat(auto-fit, minmax(220px, 1fr));
    gap: 16px;
    margin-bottom: 16px;
  }
  .stats .card { margin-bottom: 0; }
  .stat-label { font-size: 13px; color: var(--text-sec); }
  .stat-value { font-size: 28px; font-weight: 600; margin-top: 4px; line-height: 1.2; }
  .stat-unit { font-size: 15px; font-weight: 500; color: var(--text-sec); margin-left: 4px; }
  .stat-detail { font-size: 13px; color: var(--text-mute); margin-top: 4px; }

  /* Missing-data warning */
  .alert-warning-box {
    display: flex;
    gap: 12px;
    background-color: #fff8e6;
    border: 1px solid #f3dca0;
    border-left: 4px solid var(--warning);
    border-radius: 10px;
    padding: 12px 16px;
    margin-bottom: 16px;
    color: var(--text-pri);
  }
  .alert-icon { font-size: 18px; line-height: 1.3; }
  .alert-title { font-weight: 600; }
  .alert-warning-box ul { margin: 4px 0 0; padding-left: 18px; }

  /* Chart */
  .chart-title { font-size: 15px; font-weight: 600; margin: 0 0 8px; }
  .chart-note { margin: 8px 0 0; font-size: 13px; color: var(--text-mute); }
  .chart-note .swatch {
    display: inline-block; width: 10px; height: 10px; border-radius: 50%;
    margin-right: 6px; vertical-align: -1px;
  }
"

ui <- fluidPage(
  tags$head(tags$style(HTML(css_vars)), tags$style(HTML(app_css))),
  title = "Energy consumption",

  div(class = "app",
    div(class = "app-header",
      h1("Energy consumption"),
      p("Hourly consumption and purchase cost: actual data and forecast")
    ),

    div(class = "card",
      div(class = "filters-row",
        div(class = "filter-date",
          dateInput(
            "date",
            label = "Date",
            value = ACTUAL_END,
            min = ACTUAL_START,
            max = FORECAST_END,
            format = "yyyy-mm-dd",
            language = "en"
          )
        ),
        conditionalPanel(
          condition = sprintf("input.date >= '%s'", format(FORECAST_START, "%Y-%m-%d")),
          class = "filter-model",
          selectInput(
            "model_name",
            label = "Forecast model",
            choices = model_choices,
            selected = if ("SARIMA_daily" %in% model_choices) "SARIMA_daily" else model_choices[1]
          )
        )
      ),
      p(class = "filters-note", sprintf(
        "The data source is selected automatically based on the date: actual data for %s – %s, model prediction from %s.",
        format(ACTUAL_START, "%Y-%m-%d"), format(ACTUAL_END, "%Y-%m-%d"), format(FORECAST_START, "%Y-%m-%d")
      ))
    ),

    uiOutput("stat_tiles"),
    uiOutput("data_warning"),

    div(class = "card",
      p(class = "chart-title", "Hourly consumption [MWh]"),
      plotlyOutput("consumption_plot", height = "480px"),
      uiOutput("chart_note")
    )
  )
)
