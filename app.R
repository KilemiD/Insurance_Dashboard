library(shiny)
library(shinyWidgets)
library(shinydashboard)
library(ggplot2)
library(purrr)
library(highcharter)
library(dplyr)
library(scales)
library(tidyverse)
library(countrycode)
library(ggimage)
library(tidyr)
library(DBI)
library(RMySQL)
library(plotly)

PARS <- list(
  debug = FALSE,
  classcol = "col-lg-offset-1 col-lg-10 col-md-offset-0 col-md-12 col-sm-offset-0 col-sm-12",
  sparkline_color = "#333333",
  font = '-apple-system, BlinkMacSystemFont, "Segoe UI", Roboto, Helvetica, Arial, sans-serif, "Apple Color Emoji", "Segoe UI Emoji", "Segoe UI Symbol"'
)

options(
  highcharter.google_fonts = FALSE,
  highcharter.debug = PARS$debug,
  # shiny.launch.browser = PARS$debug,
  highcharter.theme = 
    hc_theme_smpl(
      title = list(style = list(fontSize = "1.2em", fontFamily = PARS$font)),
      subtitle = list(style = list(fontFamily = PARS$font, fontSize = "0.95em")),
      chart = list(
        backgroundColor = "transparent",
        style = list(fontFamily = PARS$font, fontSize = "1.0em")
      ),
      plotOptions = list(
        series = list(
          dataLabels = list(color = "#222d32", style = list(fontWeight = "normal", textShadow = FALSE, textOutline = FALSE)),
          animation = list(duration = 3000)
        )
      ),
      legend = list(
        itemStyle =  list(
          fontWeight = "normal"
        )
      )
    )
)

dropdownButtonp <- purrr::partial(
  dropdownButton,
  status = "customstatus",
  size = "sm",
  right = TRUE,
  status = "info",
  width = "400px",
  inline = TRUE,
)
#spark

#theme
hc_theme_sparkline_vb <- function(...) {
  
  theme <- list(
    chart = list(
      backgroundColor = NULL,
      margins = c(0, 0, 0, 0),
      spacingTop = 0,
      spacingRight = 0,
      spacingBottom = 0,
      spacingLeft = 0,
      plotBorderWidth = 0,
      borderWidth = 0,
      style = list(overflow = "visible")
    ),
    xAxis = list(
      visible = F, 
      endOnTick = FALSE, 
      startOnTick = FALSE
    ),
    yAxis = list(
      visible = F,
      endOnTick = FALSE, 
      startOnTick = FALSE
    ),
    tooltip = list(
      outside = FALSE,
      shadow = FALSE,
      borderColor = "transparent",
      botderWidth = 0,
      backgroundColor = "transparent",
      style = list(textOutline = "5px white")
    ),
    plotOptions = list(
      series = list(
        marker = list(enabled = FALSE),
        lineWidth = 2,
        shadow = FALSE,
        fillOpacity = 0.0,
        color = "#FFFFFFBF",
        fillColor = list(
          linearGradient = list(x1 = 0, y1 = 1, x2 = 0, y2 = 0),
          stops = list(
            list(0.00, "#FFFFFF00"),
            list(0.50, "#FFFFFF7F"),
            list(1.00, "#FFFFFFFF")
          )
        )
      )
    ),
    credits = list(
      enabled = FALSE,
      text = ""
    )
  )
  theme <- structure(theme, class = "hc_theme")
  
  if (length(list(...)) > 0) {
    theme <- hc_theme_merge(
      theme,
      hc_theme(...)
    )
  }
  
  theme
}

valueBoxSpark <- function(value, subtitle, icon = NULL, color = "aqua", 
                          width = 4, href = NULL, spark = NULL, height_spark = "150px",minititle = NULL) {
  
  shinydashboard:::validateColor(color)
  
  if (!is.null(icon)) 
    shinydashboard:::tagAssert(icon, type = "i")
  
  boxContent <- div(
    class = paste0("small-box bg-", color),
    div(
      class = "inner",
      if(!is.null(minititle)) tags$small(minititle),
      h3(value),
      # tags$span(style = paste0("height:", height_spark), hc_size(spark, height = "100vh")),
      tags$span(hc_size(spark, height = height_spark)),
      if (!is.null(subtitle)) p(subtitle)
    ),
    if (!is.null(icon)) div(class = "icon-large", icon)
  )
  
  if (!is.null(href)) 
    boxContent <- a(href = href, boxContent)
  
  div(class = if (!is.null(width)) 
    paste0("col-sm-", width), boxContent)
}

#reading the data
overview=read.csv("overview.csv")
general=read.csv("general_class.csv")
market_share=read.csv("market_share.csv")

# ui
ui <- navbarPage(
  title = tags$div(HTML('<img src="ira2.jpg" alt="Logo" height="30"/> KE: INSURANCE SECTOR')),
  #title = "KE: INSURANCE SECTOR",
  theme = "cerulean",
  #inverse = TRUE,
  tags$style(HTML('.navbar { background-color: #9ABB50; }')),
  selected = "Industry Overview",
  ###### Here : insert shinydashboard dependencies ######
  header = tagList(
    useShinydashboard()
  ),
  #######################################################
  tabPanel("Industry Overview",
           fluidRow(    
             column(width=12,
                    # Metrics Filters
                    fluidRow(tags$head(tags$style(HTML('.box{-webkit-box-shadow: none; border-top: none; -moz-box-shadow: none;box-shadow: none;}'))),
                             column(width =12,
                                 valueBoxOutput("vb_gdp", 3),
                                 valueBoxOutput("penetration", 3),
                                 valueBoxOutput("lives", 3),
                                 valueBoxOutput("policy", 3)
                             )
                    )

             )
           ),
           
           fluidRow(
             column(
               width = 6,
               selectInput("year","Select Year:",
                           choices = unique(as.character(market_share$Year)),
                           selected="2021")
             )
           ),
           
           fluidRow(
             column(
               width = 3,
               plotlyOutput("donut")
             ),
             column(
               width = 3,
               plotlyOutput("donut_longterm")
             ),
             
             column(
               width = 3,
               plotlyOutput("donut_reinsurance")
             ),
             
             column(
               width = 3,
               highchartOutput("hc")
             )
             
           )
           
           
  ),
  tabPanel("General Insurance",
           fluidRow(tags$head(tags$style(HTML('.box{-webkit-box-shadow: none; border-top: none;-moz-box-shadow: none;box-shadow: none;}'))),
                    box(width = 6,
                        plotOutput("plot")
                    )
           )
  ),
  tabPanel("Long Term Insurance",
           verbatimTextOutput("long_term")
  ),
  tabPanel("Claims Analysis",
           verbatimTextOutput("claims")
  ),
  tabPanel("Find An Insurer",
           verbatimTextOutput("location")
  )
)

# server
server <- function(input, output) {
  
  
  output$vb_gdp <- renderValueBox({
    
    d <- overview %>%
      #filter(Industry==input$Industry)%>%
      select(Year, Gross_Direct_Premium) %>% 
      mutate(gdp_value = round(Gross_Direct_Premium/1e9, 2)) %>% 
      select(x = Year, y = gdp_value)
    
    lbl <- d %>% pull(y) %>% last() %>% comma() %>% paste0("Ksh ", .," B") 
    
    hc <- hchart(d, "areaspline") %>% #, color = PARS$sparkline_color
      hc_add_theme(hc_theme_sparkline_vb()) %>% 
      hc_tooltip(valuePrefix = "Ksh ", valueSuffix = " B") 
    
    valueBoxSpark(
      value = lbl,
      subtitle = "GDP",
      color = "teal",
      spark = hc,
      minititle = "Gross Direct Premium in 2021"
    )
    
  })
  
  output$penetration <- renderValueBox({
    
    d <- overview %>%
      #filter(Industry==input$Industry)%>%
      select(Year, Penetration) %>% 
      #mutate(gdp_value = round(Gross_Direct_Premium/1e9, 2)) %>% 
      select(x = Year, y = Penetration)
    
    lbl <- d %>% pull(y) %>% last() %>% 
      #comma() %>% 
      paste0(" %") 
    
    hc <- hchart(d, "areaspline") %>% #, color = PARS$sparkline_color
      hc_add_theme(hc_theme_sparkline_vb()) %>% 
      hc_tooltip(valuePrefix = " ", valueSuffix = "") 
    
    valueBoxSpark(
      value = lbl,
      subtitle = "Penetration",
      color = "teal",
      spark = hc,
      minititle = "Insurance Penetration in 2021"
    )
    
  })
  
  output$lives <- renderValueBox({
    
    d <- overview %>%
      #filter(Industry==input$Industry)%>%
      select(Year, Lives_Covered) %>% 
      mutate(lives = round(Lives_Covered/1e6, 2)) %>% 
      select(x = Year, y = lives)
    
    lbl <- d %>% pull(y) %>% last() %>%
      #comma() %>% 
      paste0(" ", .," M") 
    
    hc <- hchart(d, "areaspline") %>% #, color = PARS$sparkline_color
      hc_add_theme(hc_theme_sparkline_vb()) %>% 
      hc_tooltip(valuePrefix = " ", valueSuffix = " M") 
    
    valueBoxSpark(
      value = lbl,
      subtitle = "Lives",
      color = "teal",
      spark = hc,
      minititle = "Lives Covered in 2021"
    )
    
  })
  
  output$policy <- renderValueBox({
    
    d <- overview %>%
      #filter(Industry==input$Industry)%>%
      select(Year, Policies) %>% 
      mutate(policies = round(Policies/1e6, 2)) %>% 
      select(x = Year, y = policies)
    
    lbl <- d %>% pull(y) %>% last() %>%
      #comma() %>% 
      paste0(" ", .," M") 
    
    hc <- hchart(d, "areaspline") %>% #, color = PARS$sparkline_color
      hc_add_theme(hc_theme_sparkline_vb()) %>% 
      hc_tooltip(valuePrefix = " ", valueSuffix = " M") 
    
    valueBoxSpark(
      value = lbl,
      subtitle = "Policies",
      color = "teal",
      spark = hc,
      minititle = "Total Insurance Policies in 2021"
    )
    
  })
  
  #next here put the market shares for various insurance
  #classes, you know, those donuts/pie charts
  
  filteredData <- reactive({
    filter(market_share, Year == input$year)
  })
  
  #share of market for longterm/general/reinsurance
  # Create a donut chart
  output$donut <- renderPlotly({
    
    # Get the donut chart data
    data <- filteredData() %>%
      select(Type,Amount) %>%
      group_by(Type) %>% 
      mutate(total=sum(Amount))
    
    # Create a donut chart
    donut <- plot_ly(data, labels = data$Type, values = data$total, type = "pie", hole = .4)
    
    # Update the donut chart layout
    donut <- donut %>%
      layout(
        title = "Insurance Market Share",
        xaxis = list(
          title = "Variable"
        ),
        yaxis = list(
          title = "Value"
        )
      )
    
    # Return the donut chart
    donut
  })
  
  #chart for longterm
  output$donut_longterm <- renderPlotly({
    
    # Get the donut chart data
    data3 <- filteredData() %>%
      filter(Type=="Long Term") %>% 
      select(Indicator,Amount) #%>%
      #group_by(Type) %>% 
      #mutate(total=sum(Amount))
    
    # Create a donut chart
    donut <- plot_ly(data3, labels = data3$Indicator, values = data3$Amount, type = "pie", hole = .4)
    
    # Update the donut chart layout
    donut <- donut %>%
      layout(
        title = "Longterm Insurance",
        xaxis = list(
          title = "Variable"
        ),
        yaxis = list(
          title = "Value"
        )
      )
    
    # Return the donut chart
    donut
  })
  
  #chart for reinsurance
  output$donut_reinsurance <- renderPlotly({
    
    # Get the donut chart data
    data2 <- filteredData() %>%
      filter(Type=="Reinsurance") %>% 
      select(Indicator,Amount) #%>%
    #group_by(Type) %>% 
    #mutate(total=sum(Amount))
    
    
    colors <- c("red", "blue", "green", "orange", "purple","yellow")
    # Create a donut chart
    donut <- plot_ly(data2, labels = data2$Indicator, values = data2$Amount, type = "pie", hole = .4) %>%
      add_trace(marker = list(colors = colors))
      
    
    # Update the donut chart layout
    donut <- donut %>%
      layout(
        title = "Share: Reinsurance",
        xaxis = list(
          title = "Variable"
        ),
        yaxis = list(
          title = "Value"
        )
      )
    
    # Return the donut chart
    donut
  })
  
  #chart for general insurance
  
  output$hc<-renderHighchart ({
    
    data4 <- filteredData() %>%
      filter(Type=="General Insurance") %>% 
      select(Indicator,Amount) %>% 
      arrange(desc(Amount))
      
      #avg_price_brand$avg_price<-round(avg_price_brand$avg_price,0)
      
      hc <- data4 %>% 
        hchart(
          'bar', hcaes(x = Indicator, y = Amount, color = Amount)
        )%>%
        hc_xAxis(title=list(text=NULL),
                 categories = as.list(toupper(data4$Indicator))) %>%
        hc_tooltip(valuePrefix = "Ksh ",pointFormat="Amount: KSh {point.y:.0f}") #%>%
        #hc_add_theme(hc_theme_sparkline_vb())
      hc
      
    })
  
  #then put the revenue/profit/and stuff next
  #add reactive elements
  #add meaning of series in hc_tooltip
  #add a table of market share afterwards
  #showing insurance company, market share, percent, stuff like that
  #i may have to put the year as an input finally
  
  
  #maybe add things that help you compare between companies say on claims performance
  #we are speaking analysis at this point
  #output for general business
  output$plot <- renderPlot({
    ggplot(data = general, aes(x=reorder(Class,GDP), y=GDP)) +
      geom_bar(stat="identity", alpha = 0.8)
  })
  
}

shinyApp(ui = ui, server = server)