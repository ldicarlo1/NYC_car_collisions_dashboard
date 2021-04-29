## app.R ##
library(shinydashboard)
library(plotly)
library(reshape2)
library(plyr)
library(shiny)

# load in data
data <- read.csv('data/Motor_Vehicle_Collisions_NYC_Crashes_processed.csv')


# create dashboard UI
ui <- dashboardPage(
  dashboardHeader(title = span("New York City Car Collisions: Interactive Dashboard",style="font-size: 30px;"),titleWidth = 1300),skin="red",
  dashboardSidebar(disable = TRUE),
  dashboardBody(
    # Boxes need to be put in a row (or column)
    fluidRow(
      box(sliderInput("slider1", label = h3("Select Time Period"), min = as.Date("20150101",format="%Y%m%d"),max = as.Date("20210101",format="%Y%m%d"), 
                      value = c(as.Date("20200101",format="%Y%m%d"),as.Date("20210101",format="%Y%m%d")),
                      timeFormat = "%b-%Y", step = 31),width=6,height=150,solidHeader = TRUE),
      box(title="Select Accident Cause",width=3,solidHeader = TRUE,
          selectInput("cause", "Cause:", choices=unique(data$CONTRIBUTING.FACTOR.VEHICLE.1)))),
    fluidRow(
      box(title="Map of New York City Car Collisions",plotlyOutput("map",height=600),width=6,solidHeader = TRUE),
      #box(title="Diurnal Cycle of Accidents", plotlyOutput("barchart",height=200),width=4),
      box(title="Accident Timing and Frequency", plotlyOutput("heatmap",height=600),width=6,solidHeader = TRUE)
    ),
    fluidRow(
      box(title="Overall Statistics", width = 12, background = "red")
    ),
    fluidRow(
      box(title="Pie Chart of Major Accident Causes (2015-2020)", width = 5,plotlyOutput("pie")),
      box(title="Total Accidents (2015-2020)", width=7, plotlyOutput("barchart2"))
    )
  )
)

server <- function(input, output) {
  # filter by collision cause
  data_cause <- reactive({subset(data,CONTRIBUTING.FACTOR.VEHICLE.1 == input$cause)})
  
  # filter data by year
  data_time <- reactive({subset(data_cause(),(CRASH.DATE>as.Date(input$slider1[1])) & (CRASH.DATE<as.Date(input$slider1[2])))})
  
  
  # Create Map of New York City Accidents
  output$map <- renderPlotly({
    fig <- plot_ly(data=data_time(),
                   lat = ~LATITUDE,
                   lon = ~LONGITUDE,
                   marker = list(color = "red",size=2),
                   type = 'scattermapbox',mode="markers",
                   hovertext = paste("Cause :", data_time()[,'CONTRIBUTING.FACTOR.VEHICLE.1'],
                                     "<br> Injured :", data_time()[,'NUMBER.OF.PERSONS.INJURED'],
                                     "<br> Killed",data_time()[,'NUMBER.OF.PERSONS.KILLED'])) 
    fig <- fig %>%
      layout(title=list(text=paste0("Accident Cause: ",input$cause),font=list(size=10)),
        mapbox = list(
          style = 'carto-positron',
          zoom = 9.2,
          center = list(lon = -74.0060, lat = 40.7128))) 
  })
  
  
  output$heatmap <- renderPlotly({
    
    z <-dcast(data_time(), WEEKDAY ~ CRASH.TIME)
    z <- z[c(2,6,7,5,1,3,4),]
    z <- as.matrix(z)
    
    times <- c("0:00","1:00","2:00","3:00","4:00","5:00","6:00","7:00","8:00","9:00","10:00","11:00","12:00","13:00","14:00","15:00","16:00",
               "17:00","18:00","19:00","20:00","21:00","22:00","23:00")
    dow <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
    
    fig <- plot_ly(z =z,type = "heatmap",zsmooth = 'best',showscale = FALSE)
    fig <- fig %>% layout(title=list(text = paste0("Accident Count by Weekday & Time : ", input$cause),showscale = FALSE,font=list(size=10)),
                          xaxis = list(titlefont = list(size = 10),title = "Hour",fixedrange=TRUE,  tickmode = "array",nticks = 24,
                                       tickvals = c(0:23), ticktext = times, tickfont = list(size = 8)),
                          yaxis = list(domain=c(0.6, 1),titlefont = list(size = 10),title = "Day of Week", tickmode = "array",
                                       nticks = 7,tickvals = c(0:6), ticktext = dow,fixedrange=TRUE, tickfont = list(size = 8)))
    
    # create dataframe for pie chart
    data_time <- data_time()
    Counts <- c(sum(data_time$NUMBER.OF.PERSONS.INJURED, na.rm=TRUE),sum(data_time$NUMBER.OF.PERSONS.KILLED, na.rm=TRUE))
    Cause <- c("Injured","Killed")
    df <- data.frame(Cause,Counts)
    
    fig %>% add_trace(df, labels = ~Cause, values = ~Counts,type="pie",domain = list(y = c(0, 0.4)),showlegend = FALSE,
                      textinfo = 'label+value',marker=list(colors=c("royalblue",'red')),hole=0.4) %>% style(hoverinfo = 'z')
    
  })
  
  output$pie<- renderPlotly({
    df <- count(data$CONTRIBUTING.FACTOR.VEHICLE.1)
    colnames(df) <- c("Cause","Count")
    
    # order causes by descending order
    df_order <- df[order(df$Count,decreasing=TRUE),]
    
    # sum the last 35 causes and create "other" category
    other <- sum(df_order$Count[12:55])
    
    # create new dataframe by combining "other" with first 20 rows
    df_new<- df_order[1:11,]
    df_new[12,1] = "Other"
    df_new[12,2] = other
    
    gradient <- colorRampPalette(c("tomato",'darkred'))
    df_new$colors <- gradient(dim(df_new)[1])[as.numeric(cut(df_new$Count, breaks = dim(df_new)[1]))]
    
    pie <- plot_ly(df_new,labels=~Cause,values=~Count,type="pie",showlegend = FALSE,textinfo = "percent", marker=list(colors=~colors))
  })
  
  output$barchart2<- renderPlotly({
    dates = as.Date(data$CRASH.DATE)
    a <- min(dates)
    b <- max(dates)
    
    plot_ly(x =dates , type = "histogram",marker=list(color="orangered")) %>% layout(xaxis = list(title = "Month-Year",
                                          range = c(a, b),type = 'date',tickformat = "%b-%y"),yaxis=list(title="Count"))
    
    
  })
}

shinyApp(ui, server)


        