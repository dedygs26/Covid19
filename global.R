library(shiny)
library(shinydashboard)
library(rvest)
library(tidyverse)
library(gganimate)
library(shinythemes)
library(highcharter)
library(formattable)
library(lubridate)
library(shinyWidgets)
library(shinyjs)
library(DT)
library(sever)
library(jsonlite)
library(httr)
library(textrank)
library(shinyalert)
library(tidytext)
library(shinycssloaders)
library(googlesheets4)
library(leaflet)
#library(later)

options(scipen = 9999)

# get data------
gs4_auth(email = "dedysianipar68@gmail.com",cache=".secrets", token="gs4auth.rds")


sheet_id <- "17WEiC7-xbKHHAELd59HeBRRRTPXdUCNjNAYnygXWyoc"
sheet <- gs4_get(sheet_id)


rawdat <- read_sheet(sheet)#defaultnya bakal baca yang pertama

rawdat <-  rawdat %>%
  setNames(c("ObjectID","Country","Last_Update","Lat","Long","Confirmed","Deaths","Recovered","Active")) %>%
    select(Country, Confirmed, Recovered,Active,Deaths,Lat, Long)

kawalcoronaprovinsi <- GET("https://api.kawalcorona.com/indonesia/provinsi/")

rawdatindo <- fromJSON(rawToChar(kawalcoronaprovinsi$content)) %>%
    pull(attributes)



# confirmed ----

selected <- top_n(rawdat, n=5 , wt = Confirmed) %>%
    pull(Country)

unique(rawdat$Country)
new <- c("Indonesia", "US","Russia","Australia","Sudan")

t_confirmed <- rawdat %>%
    select(Country, Confirmed) %>%
    mutate(datetime = Sys.Date() %>% as.character(),
           Country = as.factor(Country))

colnames(t_confirmed) <-
    colnames(t_confirmed) %>% str_to_lower()

ts_confirmed <-
    read.csv(
        url(
            "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv"
        )
    )
date_sequal <- seq(from = ymd("2020-01-22"),
                   length.out = ncol(ts_confirmed) - 4,
                   by = "day"
)

colnames(ts_confirmed) <-
    c("province_state",
      "country",
      "lat",
      "long",
      as.character(date_sequal))

ts_confirmed_long <- pivot_longer(
  ts_confirmed,
  cols = -c(province_state, country, lat, long),
  names_to = "datetime",
  values_to = "confirmed"
) %>%
  select(country, confirmed, datetime) %>%
  bind_rows(t_confirmed) %>%
  arrange(country)


#recovered ----------

t_recovered <- rawdat %>%
    select(Country, Recovered) %>%
    mutate(datetime = Sys.Date() %>% as.character(),
           Country = as.factor(Country))

colnames(t_recovered) <-
    colnames(t_recovered) %>% str_to_lower()

ts_recovered <-
    read.csv(
        url(
            "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv"
        )
    )

date_seq_recov <- seq(
    from = ymd("2020-01-22"),
    length.out = ncol(ts_recovered) - 4,
    by = "day"
)

colnames(ts_recovered) <-
    c("province_state",
      "country",
      "lat",
      "long",
      as.character(date_seq_recov))

ts_recovered_long <- pivot_longer(
    ts_recovered,
    cols = -c(province_state, country, lat, long),
    names_to = "datetime",
    values_to = "recovered") %>%
    select(country, recovered, datetime) %>%
    bind_rows(t_recovered) %>%
    arrange(country)

# deaths -------------

t_deaths <- rawdat %>%
    select(Country, Deaths) %>%
    mutate(datetime = Sys.Date() %>% as.character(),
           Country = as.factor(Country))

colnames(t_deaths) <-
    colnames(t_deaths) %>% str_to_lower()

ts_deaths <-
    read.csv(
        url(
            "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv"
        )
    )

date_seq_deaths <- seq(
    from = ymd("2020-01-22"),
    length.out = ncol(ts_deaths) - 4,
    by = "day"
)

colnames(ts_deaths) <-
    c("province_state",
      "country",
      "lat",
      "long",
      as.character(date_seq_deaths))


ts_deaths_long <- pivot_longer(
    ts_deaths,
    cols = -c(province_state, country, lat, long),
    names_to = "datetime",
    values_to = "deaths"
) %>%
    select(country, deaths, datetime) %>%
    bind_rows(t_deaths) %>%
    arrange(country)

#map data
## map leaflet
maxLong = max(rawdat$Long)
maxLat = max(rawdat$Lat)
minLong = min(rawdat$Long)
minLat = min(rawdat$Lat)

## map highchart
# map_indo <- read.csv("map-indo.csv") %>%
#   left_join(Persen,by ="Provinsi") %>%
#   select(woe_name, kasus = Total.Cases)

#map highchart
mapdata <- read_csv("mapdata.csv")

data_map <- rawdat %>%
  mutate(
    Country = case_when(
      Country == "US" ~ "United States of America",
      Country == "Korea, South" ~ "South Korea",
      Country == "Czechia" ~ "Czech Republic",
      Country == "Serbia" ~ "Republic of Serbia",
      Country == "Taiwan*" ~ "Taiwan",
      TRUE ~ Country
    )
  ) %>%
  left_join(mapdata, by = c("Country" = "name"))
