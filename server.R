shinyServer(function(input, output) {
  # server()

  shinyInput <- function(FUN, len, id, ...) {
    inputs <- character(len)
    for (i in seq_len(len)) {
      inputs[i] <- as.character(FUN(paste0(id, i), ...))
    }
    inputs
  }

    #showNotification("Credit = Dedy Gusnadi Sianipar", duration = NULL, type = "message",closeButton = TRUE)
    # personal tab -----------------------------------------------------------------
    output$graph <- renderHighchart({
      if (input$option == "death") {
        ts_deaths_long %>%
          mutate(datetime = ymd(datetime)) %>%
          filter(country %in% input$country) %>%
          group_by(country, datetime) %>%
          summarise(deaths = sum(deaths)) %>%
          filter(datetime >= input$date_selector[1] &
                   datetime < input$date_selector[2]) %>%
          hchart(.,
                 type = "line",
                 hcaes(x = datetime,
                       y = deaths,
                       group = country)) %>%
          hc_title(text = "The Total Number of Deaths") %>%
          hc_subtitle(text = "The data is compiled by the Johns Hopkins University Center") %>%
          hc_plotOptions(line = list(
            lineWidth = 4,
            allowPointSelect = TRUE,
            marker = list(
              enabled = FALSE,
              radius = 1,
              symbol = "line"
            )
          )) %>%
          hc_add_theme(hc_theme_smpl()) %>%
          hc_exporting(enabled = TRUE)
      }

        else if (input$option == "recovered") {
            ts_recovered_long %>%
                mutate(datetime = ymd(datetime)) %>%
                filter(country %in% input$country) %>%
                group_by(country, datetime) %>%
                summarise(recovered = sum(recovered)) %>%
                filter(datetime >= input$date_selector[1] &
                           datetime < input$date_selector[2]) %>%
                hchart(.,
                       type = "line",
                       hcaes(x = datetime,
                             y = recovered,
                             group = country)) %>%
                hc_title(text = "The Total Number of Recovered") %>%
                hc_subtitle(text = "The data is compiled by the Johns Hopkins University Center ") %>%
                hc_plotOptions(line = list(
                    lineWidth = 4,
                    allowPointSelect = TRUE,
                    marker = list(
                        enabled = FALSE,
                        radius = 1,
                        symbol = "circle"
                    )
                )) %>%
                hc_add_theme(hc_theme_smpl()) %>%
                hc_exporting(enabled = TRUE)

        }

        else {
            ts_confirmed_long %>%
                mutate(datetime = ymd(datetime)) %>%
                filter(country %in% input$country) %>%
                group_by(country, datetime) %>%
                summarise(confirmed = sum(confirmed)) %>%
                filter(datetime >= input$date_selector[1] &
                           datetime < input$date_selector[2]) %>%
                hchart(.,
                       type = "line",
                       hcaes(x = datetime,
                             y = confirmed,
                             group = country)) %>%
                hc_title(text = "The Total Number of Confirmed Cases") %>%
                hc_subtitle(text = "The data is compiled by the Johns Hopkins University Center ") %>%
                hc_plotOptions(line = list(
                    lineWidth = 4,
                    allowPointSelect = TRUE,
                    marker = list(
                        enabled = FALSE,
                        radius = 1,
                        symbol = "circle"
                    )
                )) %>%
                hc_add_theme(hc_theme_smpl())%>%
                hc_exporting(enabled = TRUE)

        }


    })


    labels <- paste0(rawdat$Country,
                     "<br> Total Cases : " , rawdat$Confirmed,
                     "<br> TotalDeaths : " , rawdat$Deaths,
                     "<br> Total Recovered: ",rawdat$Recovered,
                     "<br> Total Active Cases: ",rawdat$Active)




    output$leaflet <- renderLeaflet({
      rawdat %>%
        leaflet() %>%
        addTiles() %>%
        addCircleMarkers(
          lng = rawdat$Long,
          lat =rawdat$Lat,
          popup = labels,
          fill = TRUE,
          color = "#03F",
          radius = 20,
          opacity = 0.5) %>%
        setView(100,10,4)
    })


})
