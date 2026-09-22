server <- function(input, output, session) {

  selected_data <- reactive({
    req(input$date)
    sel_date <- as.Date(input$date)

    actual <- consumption_raw %>%
      filter(date == sel_date)

    fcst <- forecast_raw %>%
      filter(model_name == input$model_name, date == sel_date)

    list(
      actual = actual,
      forecast = fcst,
      has_actual = nrow(actual) > 0,
      has_forecast = nrow(fcst) > 0
    )
  })

  # Daily summary: total consumption and purchase cost (consumption [MWh] *
  # hourly price). Uses actual consumption where available, otherwise the
  # selected model's forecast. Hours with a missing value (NA) are excluded
  # and reported in the warning box.
  day_summary <- reactive({
    d <- selected_data()

    usage <- if (d$has_actual) {
      d$actual %>% select(datetime, mwh = consumption_mwh)
    } else if (d$has_forecast) {
      d$forecast %>% select(datetime, mwh = forecast)
    } else {
      return(NULL)
    }

    joined <- usage %>% left_join(prices_raw, by = "datetime")
    priced <- joined %>% filter(!is.na(mwh), !is.na(price))
    has_price <- nrow(priced) > 0

    list(
      is_actual     = d$has_actual,
      energy        = sum(joined$mwh, na.rm = TRUE),
      hours         = sum(!is.na(joined$mwh)),
      cost          = if (has_price) sum(priced$mwh * priced$price) else NA_real_,
      priced_hours  = nrow(priced),
      missing_price = joined$datetime[is.na(joined$price)],
      missing_mwh   = joined$datetime[is.na(joined$mwh)],
      missing_rows  = max(0, 24 - nrow(joined))
    )
  })

  stat_tile <- function(label, value, unit, detail) {
    div(class = "card",
      div(class = "stat-label", label),
      div(class = "stat-value", value, if (!is.null(unit)) span(class = "stat-unit", unit)),
      div(class = "stat-detail", detail)
    )
  }

  output$stat_tiles <- renderUI({
    s <- day_summary()
    if (is.null(s)) return(NULL)

    source_label <- if (s$is_actual) "Actual data" else paste0("Forecast (", input$model_name, ")")

    div(class = "stats",
      stat_tile(
        "Total consumption",
        comma(s$energy, accuracy = 0.1), "MWh",
        sprintf("%s · %d hours", source_label, s$hours)
      ),
      if (is.na(s$cost)) {
        stat_tile("Purchase cost", "—", NULL, "No price data for this date")
      } else {
        stat_tile(
          "Purchase cost",
          comma(s$cost, accuracy = 1), "PLN",
          sprintf("Consumption × hourly price, %d of 24 hours", s$priced_hours)
        )
      }
    )
  })

  # Missing-data warning is shown for actual data only; forecast dates have no
  # prices by design.
  output$data_warning <- renderUI({
    s <- day_summary()
    if (is.null(s) || !s$is_actual) return(NULL)

    fmt_hours <- function(x) paste(format(x, "%H:%M", tz = "UTC"), collapse = ", ")
    n_price <- length(s$missing_price)
    n_mwh <- length(s$missing_mwh)

    issues <- list(
      if (n_price > 0 && s$priced_hours == 0) {
        "No price data for this date, so the purchase cost can't be calculated."
      } else if (n_price > 0) {
        sprintf("No price data for %d hour(s): %s. These hours are excluded from the purchase cost.",
                n_price, fmt_hours(s$missing_price))
      },
      if (n_mwh > 0) {
        sprintf("Consumption value missing for %d hour(s): %s.", n_mwh, fmt_hours(s$missing_mwh))
      },
      if (s$missing_rows > 0) {
        sprintf("Only %d of 24 hours are available for this date.", 24 - s$missing_rows)
      }
    )
    issues <- Filter(Negate(is.null), issues)
    if (length(issues) == 0) return(NULL)

    div(class = "alert-warning-box", role = "alert",
      span(class = "alert-icon", HTML("&#9888;")),
      div(
        div(class = "alert-title", "Missing data"),
        tags$ul(lapply(issues, tags$li))
      )
    )
  })

  output$chart_note <- renderUI({
    d <- selected_data()

    note <- if (d$has_actual && d$has_forecast) {
      tagList(
        span(class = "swatch", style = sprintf("background:%s", COL_ACTUAL)), "Source: actual data and ",
        span(class = "swatch", style = sprintf("background:%s", COL_FORECAST)),
        sprintf("prediction of the %s model. The shaded band is the 95%% confidence interval.", input$model_name)
      )
    } else if (d$has_actual) {
      tagList(span(class = "swatch", style = sprintf("background:%s", COL_ACTUAL)),
              "Source: actual consumption data.")
    } else if (d$has_forecast) {
      tagList(
        span(class = "swatch", style = sprintf("background:%s", COL_FORECAST)),
        sprintf("Source: prediction of the %s model. The shaded band is the 95%% confidence interval.", input$model_name)
      )
    }

    if (!is.null(note)) p(class = "chart-note", note)
  })

  output$consumption_plot <- renderPlotly({
    d <- selected_data()

    if (!d$has_actual && !d$has_forecast) {
      return(
        plot_ly(type = "scatter", mode = "markers", x = numeric(0), y = numeric(0)) %>%
          layout(
            xaxis = list(visible = FALSE),
            yaxis = list(visible = FALSE),
            plot_bgcolor = COL_SURFACE,
            paper_bgcolor = COL_SURFACE,
            annotations = list(list(
              text = "No data available for the selected date",
              showarrow = FALSE,
              font = list(size = 16, color = COL_TEXT_MUTE)
            ))
          ) %>%
          config(displaylogo = FALSE)
      )
    }

    p <- plot_ly()

    if (d$has_forecast) {
      p <- p %>%
        add_ribbons(
          data = d$forecast, x = ~datetime, ymin = ~lower_95, ymax = ~upper_95,
          name = "95% confidence interval",
          fillcolor = toRGB(COL_FORECAST, 0.15),
          line = list(color = "rgba(0,0,0,0)"),
          hoverinfo = "skip",
          showlegend = TRUE
        )
    }

    if (d$has_actual) {
      p <- p %>%
        add_lines(
          data = d$actual, x = ~datetime, y = ~consumption_mwh,
          name = "Actual data",
          line = list(color = COL_ACTUAL, width = 2),
          hovertemplate = "%{x}<br>%{y:.1f} MWh<extra>Actual</extra>"
        )
    }

    if (d$has_forecast) {
      p <- p %>%
        add_lines(
          data = d$forecast, x = ~datetime, y = ~forecast,
          name = paste0("Forecast (", input$model_name, ")"),
          line = list(color = COL_FORECAST, width = 2, dash = "dash"),
          hovertemplate = "%{x}<br>%{y:.1f} MWh<extra>Forecast</extra>"
        )
    }

    p %>%
      layout(
        xaxis = list(title = "Delivery hour", gridcolor = COL_GRID, linecolor = COL_AXIS),
        yaxis = list(title = "Consumption [MWh]", gridcolor = COL_GRID, linecolor = COL_AXIS),
        plot_bgcolor = COL_SURFACE,
        paper_bgcolor = COL_SURFACE,
        font = list(family = "system-ui, -apple-system, Segoe UI, Roboto, sans-serif", color = COL_TEXT_SEC),
        showlegend = d$has_forecast,
        legend = list(orientation = "h", y = -0.2),
        hovermode = "x unified",
        margin = list(t = 10)
      ) %>%
      config(displaylogo = FALSE)
  })
}
