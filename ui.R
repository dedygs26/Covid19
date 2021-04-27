shinyUI(fluidPage(
  tags$head(tags$style(
    HTML(
      "
       table.dataTable tbody th, table.dataTable tbody td {
    padding: 8px 10px;
    background: black;
    font-size: 12px;
      }"
    )
  )),
#
#   use_sever(),
#   useShinyalert(),

  navbarPage(
    "Novel Coronavirus (COVID-19) Situation",
    theme = shinytheme("slate"),

    tabPanel("Global",

             sidebarLayout(
               sidebarPanel(


                 leafletOutput("leaflet")%>% withSpinner(type = 6, color = "#e00ba0", size = 0.5),

                 br(),
                 hr(),
                 p("Updated just now"),

                 p("Source: api.kawalcorona.com"),


                 p(
                   "Created by",
                   a("Dedy Gusnadi Sianipar", href = "https://github.com/dedygs26")
                 ),

               ),

               mainPanel(
                 width = 8,

                  infoBox(value = tags$p(style= "font-size: 20px;", comma(sum(
                    rawdat %>%
                      pull(Confirmed), na.rm = T
                  ), digits = 0)),
                  title = tags$p(style = "font-size: 30px; text-transform: capitalize;", "Cases"),
                  icon = icon("user-check"),
                  color= "blue",
                  fill = TRUE
                  ),

                infoBox(
                 value = tags$p(style= "font-size: 20px;", comma(sum(
                   rawdat %>%
                     pull(Deaths), na.rm = T
                 ), digits = 0)),
                 title = tags$p(style = "font-size: 30px; text-transform: capitalize;", "Deaths"),
                 icon = icon("user-slash"),
                 color= "green",
                 fill = TRUE
                 ),

               infoBox(
                 value = tags$p(style= "font-size: 20px;", comma(sum(
                   rawdat %>%
                     pull(Recovered), na.rm = T
                 ), digits = 0)),
                 title = tags$p(style = "font-size: 30px; text-transform: capitalize;", "Recovered"),
                 icon = icon("user-shield"),
                 color= "red",
                 fill = TRUE
                 ),

               # infoBox(
               #   value = tags$p(style= "font-size: 20px;", comma(sum(
               #     rawdat %>%
               #       pull(Active), na.rm = T
               #   ), digits = 0)),
               #   title = tags$p(style = "font-size: 30px; text-transform: capitalize;", "Active"),
               #   icon = icon("user-shield"),
               #   color= "red",
               #   fill = TRUE
               #   ),

               br(),
               br(),

              column(
                 width = 4,
                 selectInput(
                   inputId = "country",
                   label = "Select Country",
                   choices = unique(ts_deaths_long$country),
                   multiple = TRUE,
                   selected = selected
                 )
               ),


              column(
                width = 4,
                dateRangeInput(
                  "date_selector",
                  label = "Choose the date",
                  start = ymd("2020-01-01"),
                  end = ymd(Sys.Date()+1),
                  separator = "to"
                )
              ),

              column(
                width = 4,

                selectInput(
                  inputId = "option",
                  label = "Choose Graph",
                  selected = "confirmed",
                  choices = c("confirmed","death","recovered")
                )

              ),

              br(),
              hr(),

              highchartOutput(outputId = "graph",width = 860)%>% withSpinner(type = 6, color = "#e00ba0", size = 0.5),

              hr()

               ))
            ),

    tabPanel("Indonesia",





















             )


            ),


))

