library(data.table)
library(dplyr)
library(leaflet)
library(leaflet.extras)
library(zipcode)
library(htmltools)
library(tidyverse)
library(scales)
library(sf)
library(RColorBrewer)
library(shiny)


shinyServer(function(input, output, session) {
  #scatter plot
  observe({ 
    if (input$dataEdit == "Property Price < $5000"){
      output$myQScatterChart <- renderPlotly({
        pdf(NULL)
        plot_ly(c1, x = ~year,y = ~price, color = "tomato", type = "scatter" ) %>%
          layout(title = "Criteria 1: Prperty price less than $5000")
      })
    }
    
    else if (input$dataEdit == "Property price > $1.5M") {
      output$myQScatterChart <- renderPlotly({
        pdf(NULL)
        plot_ly(c2, y = ~price,type = "box",color = "tomato", name = "criteria2", boxpoints = "all", jitter = 0.1,alpha = 0.08, size = 0.2, pointpos = -1.8) %>%    
          layout(title = "Crteria 2: Price larger than $1.5M \n Box Plot",
                 yaxis = list(type = "log"))
      })
    }  
    
    else if (input$dataEdit == "Property type not in S,C,P") {
      output$myQScatterChart <- renderPlotly({
        pdf(NULL)
        plot_ly(c3, y = ~price,group=~prop_typ,color = ~prop_typ, type = "box") %>%
          layout(title = "Crteria 3: Property Type not in S,C,P  \n Box Plot",
                 xaxis = list(type = 'category'),
                 yaxis = list(type = "log"))
        
      })
    }  
    else if (input$dataEdit == "Number of Unit not equal to 1") {
      pdf(NULL)
      output$myQScatterChart <- renderPlotly({
        plot_ly(c4, y = ~price,group=~nunits,color = ~nunits, type = "box") %>%
          layout(title = "Crteria 4: nunits not equal to 1 \n Box Plot",
                 xaxis = list(type = 'category'),
                 yaxis = list(type = "log"))
      })
    }  
  })
  ##function for min, Q1,median, Q3, Max
  manualQuartile <- function(x){
    x <- sort(x)
    n <- length(x)
    m <- (n+1)/2
    if (floor(m)!=m){l <- m-1/2; u <- m+1/2
    }else {l <- m-1; u <- m+1}
    c(Min=min(x), Q1=median(x[1:l]), Median=median(x[1:n]),Mean=mean(x), Q3=median(x[u:n]), Max=max(x))
  }
  
  observe({ 
    #Value Box
    if (input$dataEdit == "Property Price < $5000"){
      res_mq <-manualQuartile(c1$price)
      output$minBoxInScatterSummary <- renderValueBox({
        valueBox(
          format(res_mq['Min'],format='d',big.mark=','),
          paste("Min Price"),
          icon = icon("stats",lib='glyphicon'),
          color = "purple"
        )
      })
      output$meanBoxInScatterSummary <- renderValueBox({
        valueBox(
          format(res_mq['Mean'],format='d',big.mark=','),
          paste("Mean Price"),
          icon = icon("fas fa-dollar-sign"),
          color = "green"
        )
      })
      output$maxBoxInScatterSummary <- renderValueBox({
        valueBox(
          format(res_mq['Max'],format='d',big.mark=','),
          paste("Max Price"),
          icon = icon("menu-hamburger",lib='glyphicon'),
          color = "yellow"
        )
      })
      output$q1BoxInScatterSummary <- renderValueBox({
        valueBox(
          format(res_mq['Q1'],format='d',big.mark=','),
          paste("Q1 Price"),
          icon = icon("stats",lib='glyphicon'),
          color = "purple"
        )
      })
      output$medBoxInScatterSummary <- renderValueBox({
        valueBox(
          format(res_mq['Median'],format='d',big.mark=','),
          paste("Median Price"),
          icon = icon("fas fa-dollar-sign"),
          color = "green"
        )
      })
      output$q3BoxInScatterSummary <- renderValueBox({
        valueBox(
          format(res_mq['Q3'],format='d',big.mark=','),
          paste("Q3 Price"),
          icon = icon("menu-hamburger",lib='glyphicon'),
          color = "yellow"
        )
      })

    }
    
    else if (input$dataEdit == "Property price > $1.5M") {
      res_mq <-manualQuartile(c2$price)
      output$minBoxInScatterSummary <- renderValueBox({
        valueBox(
          format(res_mq['Min'],format='d',big.mark=','),
          paste("Min Price"),
          icon = icon("stats",lib='glyphicon'),
          color = "purple"
        )
      })
      output$meanBoxInScatterSummary <- renderValueBox({
        valueBox(
          format(res_mq['Mean'],format='d',big.mark=','),
          paste("Mean Price"),
          icon = icon("fas fa-dollar-sign"),
          color = "green"
        )
      })
      output$maxBoxInScatterSummary <- renderValueBox({
        valueBox(
          format(res_mq['Max'],format='d',big.mark=','),
          paste("Max Price"),
          icon = icon("menu-hamburger",lib='glyphicon'),
          color = "yellow"
        )
      })
      output$q1BoxInScatterSummary <- renderValueBox({
        valueBox(
          format(res_mq['Q1'],format='d',big.mark=','),
          paste("Q1 Price"),
          icon = icon("stats",lib='glyphicon'),
          color = "purple"
        )
      })
      output$medBoxInScatterSummary <- renderValueBox({
        valueBox(
          format(res_mq['Median'],format='d',big.mark=','),
          paste("Median Price"),
          icon = icon("fas fa-dollar-sign"),
          color = "green"
        )
      })
      output$q3BoxInScatterSummary <- renderValueBox({
        valueBox(
          format(res_mq['Q3'],format='d',big.mark=','),
          paste("Q3 Price"),
          icon = icon("menu-hamburger",lib='glyphicon'),
          color = "yellow"
        )
      })
    }  
    
    else if (input$dataEdit == "Property type not in S,C,P") {
      res_mq <-manualQuartile(c3$price)
      output$minBoxInScatterSummary <- renderValueBox({
        valueBox(
          format(res_mq['Min'],format='d',big.mark=','),
          paste("Min Price"),
          icon = icon("stats",lib='glyphicon'),
          color = "purple"
        )
      })
      output$meanBoxInScatterSummary <- renderValueBox({
        valueBox(
          format(res_mq['Mean'],format='d',big.mark=','),
          paste("Mean Price"),
          icon = icon("fas fa-dollar-sign"),
          color = "green"
        )
      })
      output$maxBoxInScatterSummary <- renderValueBox({
        valueBox(
          format(res_mq['Max'],format='d',big.mark=','),
          paste("Max Price"),
          icon = icon("menu-hamburger",lib='glyphicon'),
          color = "yellow"
        )
      })
      output$q1BoxInScatterSummary <- renderValueBox({
        valueBox(
          format(res_mq['Q1'],format='d',big.mark=','),
          paste("Q1 Price"),
          icon = icon("stats",lib='glyphicon'),
          color = "purple"
        )
      })
      output$medBoxInScatterSummary <- renderValueBox({
        valueBox(
          format(res_mq['Median'],format='d',big.mark=','),
          paste("Median Price"),
          icon = icon("fas fa-dollar-sign"),
          color = "green"
        )
      })
      output$q3BoxInScatterSummary <- renderValueBox({
        valueBox(
          format(res_mq['Q3'],format='d',big.mark=','),
          paste("Q3 Price"),
          icon = icon("menu-hamburger",lib='glyphicon'),
          color = "yellow"
        )
      })
    }  
    else if (input$dataEdit == "Number of Unit not equal to 1") {
      res_mq <-manualQuartile(c4$price)
      output$minBoxInScatterSummary <- renderValueBox({
        valueBox(
          format(res_mq['Min'],format='d',big.mark=','),
          paste("Min Price"),
          icon = icon("stats",lib='glyphicon'),
          color = "purple"
        )
      })
      output$meanBoxInScatterSummary <- renderValueBox({
        valueBox(
          format(res_mq['Mean'],format='d',big.mark=','),
          paste("Mean Price"),
          icon = icon("fas fa-dollar-sign"),
          color = "green"
        )
      })
      output$maxBoxInScatterSummary <- renderValueBox({
        valueBox(
          format(res_mq['Max'],format='d',big.mark=','),
          paste("Max Price"),
          icon = icon("menu-hamburger",lib='glyphicon'),
          color = "yellow"
        )
      })
      output$q1BoxInScatterSummary <- renderValueBox({
        valueBox(
          format(res_mq['Q1'],format='d',big.mark=','),
          paste("Q1 Price"),
          icon = icon("stats",lib='glyphicon'),
          color = "purple"
        )
      })
      output$medBoxInScatterSummary <- renderValueBox({
        valueBox(
          format(res_mq['Median'],format='d',big.mark=','),
          paste("Median Price"),
          icon = icon("fas fa-dollar-sign"),
          color = "green"
        )
      })
      output$q3BoxInScatterSummary <- renderValueBox({
        valueBox(
          format(res_mq['Q3'],format='d',big.mark=','),
          paste("Q3 Price"),
          icon = icon("menu-hamburger",lib='glyphicon'),
          color = "yellow"
        )
      })
    }  
  })
  #MAPS menu item 
  observe({ 
  #cluster map
  if (input$cluster == 'c1'){
    output$cluster_map <- renderLeaflet({
      leaflet() %>%  
        addProviderTiles('Esri') %>% 
        addCircleMarkers(data = c1, lng = ~longitude, lat=~latitude, radius = 4, opacity = 0.7,
                         label = ~paste0("Price:",dollar(price),'\n',state), clusterOptions = markerClusterOptions()) %>%
        setView(lat = 39.8282, lng = -98.5795, zoom = 3) 
    })
  }
  
  else if (input$cluster == 'c2') {
    output$cluster_map <- renderLeaflet({
      leaflet() %>%  
        addProviderTiles('Esri') %>% 
        addCircleMarkers(data = c2, lng = ~longitude, lat=~latitude, radius = 4, 
                          opacity = 0.7,label = ~paste0("Price:",dollar(price),'\n',state), clusterOptions = markerClusterOptions()) %>%
        setView(lat = 39.8282, lng = -98.5795, zoom = 3) 
    })
  }  
  
  else if (input$cluster == 'c3') {
    output$cluster_map <- renderLeaflet({
      pal <- colorFactor(palette = c('royalblue','violetred','tomato3','purple'),
                          levels = c("4","5","6","9"))
      leaflet()%>%
        addProviderTiles('Esri') %>%
        addCircleMarkers(data = typ4,lng = ~longitude, lat=~latitude, color = ~pal(prop_typ),label = ~ paste0("Price:$",price,'\n',state), clusterOptions = markerClusterOptions(), radius = 1.5, group = '4') %>% 
        addCircleMarkers(data = typ5,lng = ~longitude, lat=~latitude,color =  ~pal(prop_typ), label = ~ paste0("Price:",dollar(price),'\n',state), clusterOptions = markerClusterOptions(), radius = 1.5, group = '5') %>%
        addCircleMarkers(data = typ6,lng = ~longitude, lat=~latitude,color =  ~pal(prop_typ), label = ~ paste0("Price:",dollar(price),'\n',state),clusterOptions = markerClusterOptions(), radius = 1.5,group = '6') %>%
        addCircleMarkers(data = typ9,lng = ~longitude, lat=~latitude,color =  ~pal(prop_typ), label = ~ paste0("Price:",dollar(price),'\n',state),clusterOptions = markerClusterOptions(),  radius = 1.5,group = '9') %>%
        addLayersControl(overlayGroups = c('4','5','6','9')) %>%
        setView(lat = 39.8282, lng = -98.5795, zoom = 3)
     
    })
  }  
  
  else if (input$cluster == 'c4') {
    output$cluster_map <- renderLeaflet({
      pal1 <- colorFactor(palette = c('royalblue','violetred','tomato3','purple'),
                          levels = c("2","3","4","."))
      leaflet()%>%
        addProviderTiles('Esri') %>%
        addCircleMarkers(data = nunits2,lng = ~longitude, lat=~latitude, color = ~pal1(nunits),label = ~ paste0("Price:$",price,'\n',state),clusterOptions = markerClusterOptions(),radius = 1.5, group = '2') %>%
        addCircleMarkers(data = nunits3,lng = ~longitude, lat=~latitude,color =  ~pal1(nunits), label = ~ paste0("Price:",dollar(price),'\n',state), clusterOptions = markerClusterOptions(), radius = 1.5, group = '3') %>%
        addCircleMarkers(data = nunits4,lng = ~longitude, lat=~latitude,color =  ~pal1(nunits), label = ~ paste0("Price:",dollar(price),'\n',state),clusterOptions = markerClusterOptions(), radius = 1.5,group = '4') %>%
        addCircleMarkers(data = nunitsdot,lng = ~longitude, lat=~latitude,color =  ~pal1(nunits), label = ~ paste0("Price:",dollar(price),'\n',state), clusterOptions = markerClusterOptions(), radius = 1.5,group = '.') %>%
        addLayersControl(overlayGroups = c('2','3','4','.')) %>%
        setView(lat = 39.8282, lng = -98.5795, zoom = 4)
    })
  }  
  
  #choropleth map 
  if (input$choropleth == 'c1'){
    pal2 <- colorNumeric("PiYG", domain=state_c1$total)
    label_c1 <- paste0(state_c1$NAME,"\n","Total: ",as.character(state_c1$total))
    output$choropleth_map <- renderLeaflet({
      leaflet() %>%
        addProviderTiles("Esri") %>%
        setView(-98.483330, 38.712046, zoom = 4) %>% 
        addPolygons(data = state_c1 , 
                    fillColor = ~pal2(state_c1$total), 
                    fillOpacity = 0.4, 
                    weight = 0.2, 
                    smoothFactor = 0.2, 
                    label = ~label_c1) %>%
        addLegend(pal = pal2, 
                  values = state_c1$total, 
                  position = "bottomright", 
                  title = "criteria1 : less than $5000")
    })
  }
  
  else if (input$choropleth == 'c2'){
    
    output$choropleth_map <- renderLeaflet({
      pal3 <- colorNumeric("OrRd", domain=state_c2$total)
      label_c2 <- paste0(state_c2$NAME,"\n","Total: ",as.character(state_c2$total))
      leaflet() %>%
        addProviderTiles("Esri") %>%
        setView(-98.483330, 38.712046, zoom = 4) %>% 
        addPolygons(data = state_c2 , 
                    fillColor = ~pal3(state_c2$total), 
                    fillOpacity = 0.8, 
                    weight = 0.2, 
                    smoothFactor = 0.2, 
                    label = ~label_c2) %>%
        addLegend(pal = pal3, 
                  values = state_c2$total, 
                  position = "bottomright", 
                  title = "criteria2 : 1.5 million")
    })
  }
  
  else if (input$choropleth == 'c3'){
    
    output$choropleth_map <- renderLeaflet({
      pal4 <- colorNumeric("BuPu", domain=state_c3$total)
      label_c3 <- paste0(state_c3$NAME,"\n","Total: ",as.character(state_c3$total))
      leaflet() %>%
        addProviderTiles("Esri") %>%
        setView(-98.483330, 38.712046, zoom = 4) %>% 
        addPolygons(data = state_c3 , 
                    fillColor = ~pal4(state_c3$total), 
                    fillOpacity = 0.8, 
                    weight = 0.2, 
                    smoothFactor = 0.2, 
                    label = ~label_c3) %>%
        addLegend(pal = pal4, 
                  values = state_c3$total, 
                  position = "bottomright", 
                  title = "criteria3 : prop_type not in s,c,p")
    })
  }
  
  else if (input$choropleth == 'c4'){
    
    output$choropleth_map <- renderLeaflet({
      pal5 <- colorNumeric("Spectral", domain=state_c4$total)
      label_c4 <- paste0(state_c4$NAME,"\n","Total: ",as.character(state_c4$total))
      leaflet() %>%
        addProviderTiles("Esri") %>%
        setView(-98.483330, 38.712046, zoom = 4) %>% 
        addPolygons(data = state_c4 , 
                    fillColor = ~pal5(state_c4$total), 
                    fillOpacity = 0.4, 
                    weight = 0.2, 
                    smoothFactor = 0.2, 
                    label = ~label_c4) %>%
        addLegend(pal = pal5, 
                  values = state_c4$total, 
                  position = "bottomright", 
                  title = "criteria4 : nunits not equal to 1")
    })
  }
  }) 
  #Time Series 
  observe({ 
    if (input$TSName == 'c1'){
      output$myTimeSeriesPlot <- renderPlotly({
        p1 <- ggplot(yearly_count1, aes(x=year, y=count)) + geom_line(size=0.7) +geom_point(aes(text=year), color = 'red', alpha = 0.3) 
        p1 <- p1 + ggtitle("Time Series Plot \n Criteria 1: Price less than $5000")    
        gg1 <- ggplotly(p1)
        gg1 <- style(gg1, line = list(color = 'gold'), hoverinfo = "y", traces = 1)
      })
      output$myTableForSummary <- DT::renderDataTable({
        yearly_count1
      })
      output$myTableForTimeSeries <- DT::renderDataTable({
        c1
      })
    }
    
    else if (input$TSName == 'c2') {
      output$myTimeSeriesPlot <- renderPlotly({
        p2 <- ggplot(yearly_count2, aes(x=year, y=count)) + geom_line(size=0.7) +geom_point(aes(text=year), color = 'red', alpha = 0.3) 
        p2 <- p2 + ggtitle("Time Series Plot \n Criteria 2: Price large than $1.5M")          
        gg2 <- ggplotly(p2)
        gg2 <- style(gg2, line = list(color = 'gold'), hoverinfo = "y", traces = 1)
      })
      output$myTableForSummary <- DT::renderDataTable({
        yearly_count2
      })
      output$myTableForTimeSeries <- DT::renderDataTable({
        c2
      })
    }  
    
    else if (input$TSName == 'c3') {
      output$myTimeSeriesPlot <- renderPlotly({
        p3 <- ggplot(yearly_count3, aes(x=year, y=count,group=prop_typ, color=prop_typ)) + geom_line(size=0.7) 
        p3 <- p3 + ggtitle("Time Series Plot \n Criteria 3: property type not in S,C,P") 
        gg <- ggplotly(p3)
        gg <- style(gg, line = list(color = 'gold'), hoverinfo = "y", traces = 1)
      })
      output$myTableForSummary <- DT::renderDataTable({
        yearly_count3
      })
      output$myTableForTimeSeries <- DT::renderDataTable({
        c3
      })
    }  
    
    else if (input$TSName == 'c4') {
      output$myTimeSeriesPlot <- renderPlotly({
        p4 <- ggplot(yearly_count4, aes(x=year, y=count, group=nunits, color=nunits)) + geom_line(size=0.7) 
        p4 <- p4 + ggtitle("Time Series Plot \n Criteria 4: nunits not equal to 1") 
        gg4 <- ggplotly(p4)
        gg4 <- style(gg4, line = list(color = 'gold'), hoverinfo = "y", traces = 1)
      })
      output$myTableForSummary <- DT::renderDataTable({
        yearly_count4
      })
      output$myTableForTimeSeries <- DT::renderDataTable({
        c4
      })
    }  
  })
  ## Data Explorer ###########################################
  excl_fun <- reactive({
    req(input$types)
    if (input$types == "Property Price  < $5000") {
      excl <- c1
    }
    else if(input$types == "Property price > $1.5M")
    {
      excl <-c2
    }
    else if(input$types == "Property type not in S,C,P")
    {
      excl <-c3
    }
    else if(input$types == "Number of Unit not equal to 1")
    {
      excl <-c4
    }
    else
    {
      excl <-excl
    }
  })
  observe({
    counties <- if (is.null(input$states)) character(0) else {
      excl = excl_fun()
      excl %>%
        filter(state %in% input$states) %>%
        `$`('county') %>%
        unique() %>%
        sort()
    }
    stillSelected <- isolate(input$counties[input$counties %in% counties])
    updateSelectInput(session, "counties", choices = counties,
                      selected = stillSelected)
  })
  
  observe({
    zipcodes <- if (is.null(input$states)) character(0) else {
      excl = excl_fun()
      excl %>%
        filter(state %in% input$states,
               is.null(input$counties) | county %in% input$counties) %>%
        `$`('zip') %>%
        unique() %>%
        sort()
    }
    stillSelected <- isolate(input$zipcodes[input$zipcodes %in% zipcodes])
    updateSelectInput(session, "zipcodes", choices = zipcodes,
                      selected = stillSelected)
  })
  
  
  
  output$datatable <- DT::renderDataTable({
      df <-excl_fun() %>%
        filter(
          is.null(input$states) | state %in% input$states,
          is.null(input$counties) | county %in% input$counties,
          is.null(input$zipcodes) | zip %in% input$zipcodes
        )
  })
  output$download_data <- downloadHandler(
    filename = function() { paste("excluded_", input$types,"_", input$states,"_",input$counties,'.csv', sep = '') },
    content = function(file) {
      df <-excl_fun() %>%
        filter(
          is.null(input$states) | state %in% input$states,
          is.null(input$counties) | county %in% input$counties,
          is.null(input$zipcodes) | zip %in% input$zipcodes)
      write.csv(df, file)
    }
  )
})
