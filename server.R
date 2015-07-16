shinyServer(function(input, output, session) {
  
  #####################
  #### Fetch data  ####
  #####################
  
  parameter <- eventReactive(input$fetchdata, {
    
    list(input$api_key,
         input$api_code,
         input$lat,
         input$lng,
         "2008-01-01", 
         if(input$forecast == "Yes") {as.character(Sys.Date()+6)} else {as.character(Sys.Date())}, 
         if (input$attribute == "precip_1") {
           c("accPrecip", "accPrecipPriorYear")
         } else if (input$attribute == "precip_3") {
           c("accPrecip", "accPrecip3YearAverage")
         } else if (input$attribute == "precip_10") {
           c("accPrecip", "accPrecipLongTermAverage")
         } else {
           c("accPrecip", "accPet", "ppet")
         }
         
    )
    
  })
  
  query <- reactive({
    # fetch data if fetch data button is pressed
    if(input$fetchdata) {
      
      # set up days
      day <- date_range(parameter()[[5]], parameter()[[6]])
      
      strquery <- lapply(1:length(day), function(time){
        paste0("latitude=", parameter()[[3]], 
               "&longitude=", parameter()[[4]], 
               "&startDate=", day[[time]][1],
               "&endDate=", day[[time]][2], 
               Reduce(paste0, lapply(1:length(parameter()[[7]]), function(i){paste0("&attribute=", parameter()[[7]][i])})))
      })
      
      # adding progress bar
      withProgress(message = "Querying data", detail = "Batch 0", value = 0, {
        
        
        Reduce(rbind, lapply(1:length(strquery), function(time){
          
          # Each time through the loop, add another year of data to query. This a stand-in
          incProgress(0.1, detail = paste("Batch", time))
          request <- GET(url, query=strquery[[time]], authenticate(parameter()[[1]], parameter()[[2]]))
          data <- jsonlite::fromJSON(content(request, as = "text"))
          data.frame(date = as.Date(data$date), latitude = data$latitude, longitude = data$longitude, data$dailyAttributes, stringsAsFactors=FALSE)
        }))
        
      })
    } else {
      
      if (input$attribute == "precip_1") {
        data[, c(1,2,3,4,5)]
      } else if (input$attribute == "precip_3") {
        data[, c(1,2,3,4,6)]
      } else if (input$attribute == "precip_10") {
        data[, c(1,2,3,4,7)]
      } else {
        data[, c(1,2,3,4,8,9)]
      }
      
    }
    
  })
  
  weatherData <- reactive({
    
    data_daily <- calculate_daily(query(), 2) 
    data_daily[data_daily < 0] = 0
    colnames(data_daily) <- c("date", "V1", "V2")
    
    data_daily
    
  })
  
  
  aggregate <- reactive({
    
    if(input$accumulate == "No") {
      DT <- weatherData() 
      DT <- data.table(DT)
      DT <- setNames(DT, c("date", "V1", "V2"))
      
      if(input$aggregation == "By Day") {
        
        temp <- as.data.frame(DT)
        temp[,c(1,2,3)]
        
      } else if (input$aggregation == "By Week") {
        DT$group <- week(DT$date)
        DT$year <- year(DT$date)
        
        temp <- as.data.frame(DT[, list(sum(V1, na.rm = TRUE), sum(V2, na.rm = TRUE)), by = list(group,year)])
        temp$date <- as.Date(Reduce(rbind, lapply(1:dim(temp)[1], function(i){
          as.character(if(is.na(as.POSIXlt(paste(temp[i,2,drop = TRUE], temp[i,1,drop = TRUE],"7", sep = " "), format = "%Y %U %u", tz = "UTC") == TRUE)) {
            as.Date(paste0(temp[i,2,drop = TRUE], "-12-31"), tz = "UTC")
          } else {
            as.POSIXlt(paste(temp[i,2,drop = TRUE], temp[i,1,drop = TRUE],"7", sep = " "), format = "%Y %U %u", tz = "UTC")
          })
        })))
        
        temp[, c(5,3,4)]
        
        
      } else {
        DT$group <- month(DT$date)
        DT$year <- year(DT$date)
        
        temp <- as.data.frame(DT[, list(sum(V1, na.rm = TRUE), sum(V2, na.rm = TRUE)), by = list(group,year)])
        temp$date <- as.Date(as.POSIXlt(paste(temp[,2,drop = TRUE], temp[,1,drop = TRUE], "1", sep = " "), format = "%Y %m %d", tz = "UTC"))
        
        temp[, c(5,3,4)]
        
      }
    } else {
      
      DT <- query()[, c(1,4,5)]
      DT <- data.table(DT)
      DT <- setNames(DT, c("date", "V1", "V2"))
      
      if(input$aggregation == "By Day") {
        
        as.data.frame(DT)
        
      } else if (input$aggregation == "By Week") {
        DT$group <- weekdays(DT$date)
        
        temp <- as.data.frame(DT[DT[, group == "Sunday"] == TRUE,])
        temp[, c(1,2,3)]
        
      } else {
        DT$group <- month(DT$date)
        DT$year <- year(DT$date)
        
        temp <- as.data.frame(DT[, list(max(V1, na.rm = TRUE), max(V2, na.rm = TRUE)), by = list(group,year)])
        temp$date <- as.Date(as.POSIXlt(paste(temp[,2,drop = TRUE], temp[,1,drop = TRUE], "1", sep = " "), format = "%Y %m %d", tz = "UTC"))
        
        temp[, c(5,3,4)]
        
      }
    }
    
  })
  
  column_names <- reactive({
    if (input$attribute == "precip_1") {
      c("Date", "Precipitation", "Precipitation of Prior Year")
    } else if (input$attribute == "precip_3") {
      c("Date", "Precipitation", "Precipitation of 3 Year Average")
    } else if (input$attribute == "precip_10") {
      c("Date", "Precipitation", "Precipitation of Long Term Average")
    } else {
      c("Date", "Precipitation", "Potential Evapotranspiration")
    }
  })
  
  ####################
  #### Map Output ####
  ####################
  
  output$map <- renderLeaflet({
    
    map <- leaflet::leaflet() %>% 
      addTiles() %>%
      addMarkers(lng = input$lng,
                 lat = input$lat, 
                 icon = aWhereIcon,
                 popup = as.character(tagList(
                   sprintf(paste0("Latitude: ", as.numeric(input$lat))), tags$br(),
                   sprintf(paste0("Longitude: ", as.numeric(input$lng))), tags$br()
                 ))) %>%
      setView(lng = input$lng, 
              lat = input$lat,
              zoom = 14)
    
  })
  
  observe({
    
    event <- input$map_click
    if (is.null(event))
      return()
    
    leafletProxy("map") %>% clearMarkers()
    isolate({
      leafletProxy("map") %>% addMarkers(lng = event[[2]], 
                                         lat = event[[1]], 
                                         icon = aWhereIcon, 
                                         popup = as.character(tagList(
                                           sprintf(paste0("Latitude: ", as.numeric(event[[1]]))), tags$br(),
                                           sprintf(paste0("Longitude: ", as.numeric(event[[2]]))), tags$br()
                                         )))
    })
    
  })
  
  ###########################
  #### Data Table Output ####
  ###########################
  
  # Display data query
  output$table <- DT::renderDataTable(aggregate(), rownames = FALSE, colnames = column_names(),
                                      caption = "Table 1: This is a simple caption for the table.", 
                                      options = list(
                                        search = list(caseInsensitive = TRUE),
                                        pageLength = 10))
  
  ########################
  #### Graphic Output ####
  ########################
  
  # establish time series
  aggregate_ts <- reactive({
    xts(x = aggregate()[, c(2,3)], 
        order.by = aggregate()[, 1, drop = TRUE])    
  })
  
  cut_off <- reactive({
    as.numeric(quantile(aggregate()[, c(2,3)], probs = input$quantile, na.rm = TRUE))
  })
  
  main <- reactive({
    part_1 <- if (input$accumulate == "Yes") {"Accumulative"}
    part_2 <- if (input$attribute == "precip_1") {
      "Precipitation of Current Year Compare to Prior Year (mm)"
    } else if (input$attribute == "precip_3") {
      "Precipitation of Current Year Compare to 3 Year Average (mm)"
    } else if (input$attribute == "precip_10") {
      "Precipitation of Current Year Compare to Long Term Average (mm)"
    } else {
      "Precipitation of Current Year Compare to Potential Evapotranspiration (mm)"
    }
    paste(part_1, part_2)
  })
  
  label_v2 <- reactive({
    if (input$attribute == "precip_1") {
      "Prior Year (mm)"
    } else if (input$attribute == "precip_3") {
      "3 Year Average (mm)"
    } else if (input$attribute == "precip_10") {
      "Long Term Average (mm)"
    } else {
      "Potential Evapotranspiration (mm)"
    }
  })
  
  # construct dygraphs
  output$dygraph <- renderDygraph({
    dygraph(aggregate_ts(), main = main()) %>%
      dySeries("V1", label = "Precipitation, current year (mm)", color = "steelblue", fillGraph = TRUE, stepPlot = TRUE) %>%
      dySeries("V2", label = paste("Precipitation,", label_v2()), color = "darkgrey", fillGraph = TRUE, stepPlot = TRUE) %>%
      dyAxis("x", label = "Date", drawGrid = FALSE) %>%
      dyAxis("y", label = "Precipitation (mm)", gridLineColor = "lightblue", valueRange = c(0, cut_off())) %>%
      dyLegend(show = "always", hideOnMouseOut = FALSE) %>%
      dyRangeSelector(dateWindow = c("2014-07-14", "2015-07-15"), 
                      height = 100)
  })
  
  
  output$ppet_dygraph <- renderDygraph({
    if(input$attribute == "p_pet") {
      
      DT <- data.table(query()[,c(1,6)])
      DT <- setNames(DT, c("date", "V1"))
      temp <- as.data.frame(DT)
      data_ts <- xts(x = temp[, c(2)]*100, 
                     order.by = temp[, 1])
      
      dygraph(data_ts, main = "Precipitation over Potential Evapotranspiration (%)") %>%
        dySeries("V1", label = "P/PET (%)", color = "steelblue", fillGraph = TRUE) %>%
        dyLimit(50, color = "coral") %>%
        dyAxis("x", label = "Date", drawGrid = FALSE) %>%
        dyAxis("y", label = "P/PET (%)", gridLineColor = "lightblue") %>%
        dyLegend(show = "always", hideOnMouseOut = FALSE) %>%
        dyRangeSelector(dateWindow = c("2014-07-14", "2015-07-15"))
      
    } else {
      
      return(NULL)
      
    }
    
  })
  
  # set up dynamic ui component
  
  output$ppet_ui <- renderUI({
    
    if (input$attribute != "p_pet")
      return()
    
    dygraphOutput("ppet_dygraph", height = "800px")
    
  })
  
})

