library(shiny)
library(DBI)
library(RSQLite)
library(dplyr)
library(lubridate)
library(plotly)
library(scales)

# --- Configuration ---------------------------------------------------------

DB_PATH <- "energy_data.sqlite"

# --- Palette (validated, see dataviz skill references/palette.md) ---------

COL_ACTUAL    <- "#2a78d6"  # blue  - series 1
COL_FORECAST  <- "#eb6834"  # orange - series 2
COL_GRID      <- "#e1e8f2"
COL_AXIS      <- "#c2cddc"
COL_TEXT_MUTE <- "#6b778a"
COL_TEXT_SEC  <- "#44526a"
COL_TEXT_PRI  <- "#0f1b2d"
COL_SURFACE   <- "#ffffff"
COL_PAGE      <- "#edf3fb"  # light blue page background behind the cards
COL_WARNING   <- "#fab219"  # status: warning

# --- Data loading -----------------------------------------------------------
# Data is read once at app start as the dataset is relatively small.

con <- dbConnect(SQLite(), DB_PATH)

consumption_raw <- dbGetQuery(con, "SELECT * FROM consumption") %>%
  rename(consumption_mwh = `Consumption [MWh]`) %>%
  mutate(
    datetime = as_datetime(Delivery_hour),
    date = as.Date(datetime)
  ) %>%
  select(datetime, date, consumption_mwh)

forecast_raw <- dbGetQuery(con, "SELECT * FROM forecasts") %>%
  mutate(
    datetime = as_datetime(Delivery_hour),
    date = as.Date(datetime)
  ) %>%
  select(datetime, date, forecast, lower_95, upper_95, model_name)

# Prices are joined on Delivery_hour as in report.Rmd; the last two hours of the
# consumption period have no price and are handled as NA in the cost calculation.
prices_raw <- dbGetQuery(con, "SELECT * FROM prices") %>%
  mutate(
    datetime = as_datetime(Delivery_hour),
    price = average_hourly_price
  ) %>%
  select(datetime, price)

dbDisconnect(con)

# Period boundaries are derived from the data.
ACTUAL_START   <- min(consumption_raw$date)
ACTUAL_END     <- max(consumption_raw$date)
FORECAST_START <- min(forecast_raw$date)
FORECAST_END   <- max(forecast_raw$date)

model_choices <- sort(unique(forecast_raw$model_name))
