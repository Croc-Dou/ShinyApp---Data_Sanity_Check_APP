cat('\014')
rm(list = ls())

library(leaflet)
library(shiny)
library(shinydashboard)
library(maps)
library(plotly)

shinyUI(dashboardPage(
  #header + skin  
  skin = 'black', 
  dashboardHeader(title = 'WRSI: DATA SANITY CHECK',
                  titleWidth = 450,
                  tags$li(a(href = 'https://sf.freddiemac.com/',
                            img(src = 'company_logo.png',
                                title = "Company Home", height = "30px"),
                            style = "padding-top:10px; padding-bottom:10px;"),
                          class = "dropdown"),
                  tags$li(a(href = 'http://www.freddiemac.com/',
                            icon("power-off"),
                            title = "Back to Apps Home"),
                          class = "dropdown")),
  
  #sidebar panel 
  ##https://shiny.rstudio.com/reference/shiny/1.0.1/icon.html (choose icon)
  dashboardSidebar(
    sidebarUserPanel("Single Family Modeling MEDA"),
    sidebarMenu(
      menuItem("Dashboard Overview", tabName = "myTabForIntroduction", icon = icon("fa fa-compass")),
      menuItem("Data Edit Box Plot", tabName = "myTabForBoxPlot", icon = icon("bar-chart-o")),
      menuItem("Price Distribution Map", tabName = 'maps', icon = icon('globe-americas')),
      menuItem("Time Series Plot", tabName = 'myTabForTimeSeries', icon = icon('fas fa-angle-double-right')),
      menuItem("Data Explorer", tabName = 'myTabForDataExplorer', icon = icon('far fa-eye'))
    )
  ), 
  
  # body
  
  dashboardBody(
    #header font
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
    ),
    tags$style(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
    ),
    tags$head(
      tags$style(HTML(".my_style_1{ 
                      background-image: url(https://tropki.com/sites/default/files/styles/large/public/previews/9038/tromsyo.jpg?itok=d_3iILRh);
                      height: 1055px;}"))),
    tabItems(
      tabItem(tabName = 'myTabForIntroduction',
              class="my_style_1",
              fluidRow(
                column(
                width = 10,
                box(
                  #title = strong('Dashboard Overview'),
                  solidHeader = T,
                  width = NULL,
                  HTML(paste(h3("Purpose of This Dashboard"),
                             '<br/>',
                             h4("This Data Sanity Check Dashboard aims to help users monitor the data quality of the WRSI model through visualizations. Based on R and Shiny, this dashboard consists of five tabs.  
                                The first tab gives an overview of features, a brief introduction to the WRSI, the Dataset, and the data selection rules. 
                                The second tab enables users to explore the statistical distribution of each data edit, and their five-number summaries. 
                                The third tab is for interactive map, which provides users with two type of geographical distributions for better comparison.
                                The fourth tab, 'time series plot,' displays the trend of each data edit starting from 1975.
                                The fifth tab provides users an opportunity to filter, sort, and download data for further analysis. "),
                             '<br/>',
                             h3("WRSI"),
                             '<br/>',
                             h4("WRSI stands for Weighted Repeated Sales Index. It is an index used for tracking house price appreciation at a given geographic level. The model is estimated using sales and refinances information for properties that have transacted more than once.  WRSI is estimated at the zip, county, and state levels. It is a multiplier that reflects the growth between any given time point and the current month."),
                             '<br/>',
                             h3("Data Selection Rules"),
                             '<br/>',
                             h4("The WRSI index is constructed using individual sale prices of Freddie and Fannie mortgage loans. As the objective of the index is to get an accurate representation of the average home price in the United States, observations that do not satisfy the criteria rules are eliminated from the estimation sample. The data used to create the index is filtered using the following criteria:"))
                  ),
                  br(),
                  img(src = 'table.png', height=450, width=700, align = "center")
                ))
              )),
      #2 box plot
      tabItem("myTabForBoxPlot",
              fluidRow(
                box(
                  title = "How to Use", solidHeader = TRUE,
                  status = "warning", width = 12, collapsible = TRUE, collapsed = TRUE,
                  h5("This 'Data Edit Box Plot' panel shows the property price distribution by four data edits. It comprises of three sections: an opton input section, a box plot section, and an aggregate box section.")
                )
              ),
              fluidRow(
                column(4,
                       selectizeInput("dataEdit", "Select a data edit", c("Property Price < $5000","Property price > $1.5M","Property type not in S,C,P","Number of Unit not equal to 1"), 
                                      multiple = F)
                ),
                column(4,br(),checkboxInput("checkboxForShowDataPoint",label = "Show Data Points"))
             ),
             br(),
             fluidRow(column(12,plotlyOutput("myQScatterChart"))),
             br(),
             fluidRow(
               valueBoxOutput("minBoxInScatterSummary"),
               valueBoxOutput("meanBoxInScatterSummary"),
               valueBoxOutput("maxBoxInScatterSummary")
             ),
             fluidRow(
               valueBoxOutput("q1BoxInScatterSummary"),
               valueBoxOutput("medBoxInScatterSummary"),
               valueBoxOutput("q3BoxInScatterSummary")
             )
      ), 
      #3 Interactive Maps
      tabItem(tabName ='maps',
              tabsetPanel(type = 'tabs',
                          tabPanel('Cluster Map',     
                                   radioButtons(
                                     "cluster",
                                     label = ("Select Data Edit:"),
                                     choices = list("Property Price < $5000"="c1","Property price > $1.5M"="c2","Property type not in S,C,P"="c3","Number of Unit not equal to 1"="c4"), 
                                     selected = "c1"),     
                                   fluidRow(box(width = 16, height = "80%", leafletOutput("cluster_map")))),
                          
                          tabPanel('Choropleth Map',     
                                   radioButtons(
                                     "choropleth",
                                     label = ("Select map content:"),
                                     choices = list("Property Price < $5000"="c1","Property price > $1.5M"="c2","Property type not in S,C,P"="c3","Number of Unit not equal to 1"="c4"), 
                                     selected = "c1"),     
                                   fluidRow(box(width = 16, height = "80%", leafletOutput("choropleth_map"))))
              ) #tabsetpanel
      ),#tabitem
      tabItem("myTabForTimeSeries",
              fluidRow(
                box(
                  title = "How to Use", solidHeader = TRUE,
                  status = "warning", width = 12, collapsible = TRUE, collapsed = TRUE,
                  h5("This 'Time Series Plot' panel shows the property price distribution by four data edits. It comprises of three sections: an opton input section, a box plot section, and an aggregate box section.")
                )
              ),
              fluidRow(
                column(4,
                       selectizeInput("TSName", "Select a data edit", list("Property Price < $5000"="c1","Property price > $1.5M"="c2","Property type not in S,C,P"="c3","Number of Unit not equal to 1"="c4"), 
                                      multiple = F)
                )
              ),
              br(),
              fluidRow(column(12,plotlyOutput("myTimeSeriesPlot"))),
              br(),
              fluidRow(
                box(
                  title = "Data Table for aggregate summary", solidHeader = TRUE,
                  collapsible = TRUE,collapsed = TRUE,
                  DT::dataTableOutput("myTableForSummary")
                )
              ),
              fluidRow(
                box(
                  title = "Observations used in this plot", solidHeader = TRUE, 
                  width = 12, collapsible = TRUE,collapsed = TRUE,
                  DT::dataTableOutput("myTableForTimeSeries")
                )
              )
      ),
      tabItem("myTabForDataExplorer",
              fluidRow(
                box(
                  title = "How to Use", solidHeader = TRUE,
                  status = "warning", width = 12, collapsible = TRUE, collapsed = TRUE,
                  h5("This 'Data Explorer' is a data table having features of filtering, paginating, searching, sorting and downloading to explore the data of your interests.
                     You can interactively choose the options, then the table shows the updated result. The data of the table can be filtered by data edits, states, county and zipcode.
                     Then, you can download the selected data table for your further analysis.")
                )
              ),
              fluidRow(
                column(3,
                       selectInput("types", "Data Edit", c("All","Property Price  < $5000","Property price > $1.5M","Property type not in S,C,P","Number of Unit not equal to 1"),
                                   selected= "All", multiple=TRUE)
                )
              ),
              fluidRow(
                column(3,
                       selectInput("states", "States", c("All states"="", structure(state.abb, names=state.name), "Washington, DC"="DC", "Puerto Rico"="PR"), multiple=TRUE)
                ),
                column(3,
                       conditionalPanel("input.states",
                                        selectInput("counties", "Counties", c("All counties"=""), multiple=TRUE)
                       )
                ),
                column(3,
                       conditionalPanel("input.states",
                                        selectInput("zipcodes", "Zipcodes", c("All zipcodes"=""), multiple=TRUE)
                       )
                )
              ),
              fluidRow(
                column(3,
                       downloadButton(outputId = "download_data", label = "Download")
                )
              ),
              hr(),
              DT::dataTableOutput("datatable"),style = "overflow-y: scroll;overflow-x: scroll;"
      ) #tabitem
    ) #tabitems 
  ) ##dashboardbody
) #dashboard page 
) #shinyUI


