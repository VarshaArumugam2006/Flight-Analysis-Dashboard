library(shiny)
library(shinydashboard)
library(tidyverse)
library(plotly)
library(DT)
set.seed(123)
flights <- data.frame(
    Flight_ID = 1:1000,
    Airline = sample(
    c(
      "Air India",
      "IndiGo",
      "Vistara",
      "SpiceJet",
      "Go First"
    ),
    1000,
    replace = TRUE
  ),
  
  Origin = sample(
    c(
      "Chennai",
      "Mumbai",
      "Delhi",
      "Bangalore",
      "Hyderabad"
    ),
    1000,
    replace = TRUE
  ),
  
  Destination = sample(
    c(
      "Chennai",
      "Mumbai",
      "Delhi",
      "Bangalore",
      "Hyderabad"
    ),
    1000,
    replace = TRUE
  ),
  
  Month = sample(
    month.name,
    1000,
    replace = TRUE
  ),
  
  Distance = sample(
    300:3000,
    1000,
    replace = TRUE
  ),
  
  Delay = sample(
    0:180,
    1000,
    replace = TRUE
  )
)
flights <- flights %>%
  mutate(
    Delay_Status = ifelse(
      Delay > 15,
      "Delayed",
      "On Time"
    )
  )
ui <- dashboardPage(
  dashboardHeader(
    title = "Flight Analytics"
  ),
  dashboardSidebar(
    
    sidebarMenu(
      
      menuItem(
        "Dashboard",
        tabName = "dashboard",
        icon = icon("dashboard")
      ),
      
      menuItem(
        "Flight Analysis",
        tabName = "analysis",
        icon = icon("plane")
      ),
      
      menuItem(
        "Delay Analysis",
        tabName = "delay",
        icon = icon("clock")
      ),
      
      menuItem(
        "Flight Data",
        tabName = "data",
        icon = icon("table")
      )
      
    )
    
  ),
  dashboardBody(
    
    tabItems(
      tabItem(
        
        tabName = "dashboard",
        
        h2("Flight Analytics Dashboard"),
        fluidRow(
          
          valueBoxOutput(
            "total_flights",
            width = 3
          ),
          
          valueBoxOutput(
            "avg_delay",
            width = 3
          ),
          
          valueBoxOutput(
            "delayed_flights",
            width = 3
          ),
          
          valueBoxOutput(
            "airlines",
            width = 3
          )
          
        ),
        fluidRow(
          
          box(
            title = "Flights by Airline",
            width = 6,
            status = "primary",
            solidHeader = TRUE,
            
            plotlyOutput(
              "airline_plot",
              height = "400px"
            )
            
          ),
          box(
            title = "Average Delay by Airline",
            width = 6,
            status = "warning",
            solidHeader = TRUE,
            
            plotlyOutput(
              "delay_airline_plot",
              height = "400px"
            )
            
          )
          
        ),
        fluidRow(
          
          box(
            title = "Flights by Month",
            width = 12,
            status = "success",
            solidHeader = TRUE,
            
            plotlyOutput(
              "month_plot",
              height = "400px"
            )
            
          )
          
        )
        
      ),
      tabItem(
        
        tabName = "analysis",
        
        h2("Flight Analysis"),
        fluidRow(
          
          box(
            title = "Flights by Origin",
            width = 6,
            status = "primary",
            solidHeader = TRUE,
            
            plotlyOutput(
              "origin_plot",
              height = "400px"
            )
            
          ),
          box(
            title = "Flights by Destination",
            width = 6,
            status = "primary",
            solidHeader = TRUE,
            
            plotlyOutput(
              "destination_plot",
              height = "400px"
            )
            
          )
          
        ),
        fluidRow(
          
          box(
            title = "Flight Distance Distribution",
            width = 12,
            status = "success",
            solidHeader = TRUE,
            
            plotlyOutput(
              "distance_plot",
              height = "400px"
            )
            
          )
          
        )
        
      ),
      tabItem(
        
        tabName = "delay",
        
        h2("Flight Delay Analysis"),
        fluidRow(
          
          box(
            title = "Delay Distribution",
            width = 6,
            status = "warning",
            solidHeader = TRUE,
            
            plotlyOutput(
              "delay_distribution",
              height = "400px"
            )
            
          ),
          box(
            title = "Flight Delay Status",
            width = 6,
            status = "danger",
            solidHeader = TRUE,
            
            plotlyOutput(
              "delay_status",
              height = "400px"
            )
            
          )
          
        )
        
      ),
      tabItem(
        
        tabName = "data",
        
        h2("Flight Dataset"),
        
        fluidRow(
          
          box(
            title = "Flight Records",
            width = 12,
            status = "primary",
            solidHeader = TRUE,
            
            DTOutput(
              "flight_table"
            )
            
          )
          
        )
        
      )
      
    )
    
  )
  
)
server <- function(input, output, session) {
  output$total_flights <- renderValueBox({
    
    valueBox(
      value = nrow(flights),
      subtitle = "Total Flights",
      icon = icon("plane"),
      color = "blue"
    )
    
  })
  output$avg_delay <- renderValueBox({
    
    average_delay <- round(
      mean(
        flights$Delay,
        na.rm = TRUE
      ),
      1
    )
    
    valueBox(
      value = paste0(
        average_delay,
        " min"
      ),
      subtitle = "Average Delay",
      icon = icon("clock"),
      color = "yellow"
    )
    
  })
  output$delayed_flights <- renderValueBox({
    
    delayed <- sum(
      flights$Delay_Status == "Delayed",
      na.rm = TRUE
    )
    
    valueBox(
      value = delayed,
      subtitle = "Delayed Flights",
      icon = icon("exclamation-triangle"),
      color = "red"
    )
    
  })
  output$airlines <- renderValueBox({
    
    number_airlines <- length(
      unique(flights$Airline)
    )
    
    valueBox(
      value = number_airlines,
      subtitle = "Airlines",
      icon = icon("building"),
      color = "green"
    )
    
  })
  output$airline_plot <- renderPlotly({
    
    airline_data <- flights %>%
      count(Airline)
    
    p <- ggplot(
      airline_data,
      aes(
        x = Airline,
        y = n,
        text = paste(
          "Airline:",
          Airline,
          "<br>Flights:",
          n
        )
      )
    ) +
      
      geom_col() +
      
      labs(
        title = "Number of Flights by Airline",
        x = "Airline",
        y = "Number of Flights"
      ) +
      
      theme_minimal()
    
    ggplotly(
      p,
      tooltip = "text"
    )
    
  })
  output$delay_airline_plot <- renderPlotly({
    
    delay_data <- flights %>%
      group_by(Airline) %>%
      summarise(
        Average_Delay = mean(
          Delay,
          na.rm = TRUE
        )
      )
    
    p <- ggplot(
      delay_data,
      aes(
        x = Airline,
        y = Average_Delay,
        text = paste(
          "Airline:",
          Airline,
          "<br>Average Delay:",
          round(Average_Delay, 1),
          "minutes"
        )
      )
    ) +
      
      geom_col() +
      
      labs(
        title = "Average Delay by Airline",
        x = "Airline",
        y = "Average Delay (minutes)"
      ) +
      
      theme_minimal()
    
    ggplotly(
      p,
      tooltip = "text"
    )
    
  })
  output$month_plot <- renderPlotly({
    
    month_data <- flights %>%
      count(Month)
    
    month_data$Month <- factor(
      month_data$Month,
      levels = month.name
    )
    
    p <- ggplot(
      month_data,
      aes(
        x = Month,
        y = n,
        text = paste(
          "Month:",
          Month,
          "<br>Flights:",
          n
        )
      )
    ) +
      
      geom_col() +
      
      labs(
        title = "Number of Flights by Month",
        x = "Month",
        y = "Number of Flights"
      ) +
      
      theme_minimal() +
      
      theme(
        axis.text.x = element_text(
          angle = 45,
          hjust = 1
        )
      )
    
    ggplotly(
      p,
      tooltip = "text"
    )
    
  })
  output$origin_plot <- renderPlotly({
    
    origin_data <- flights %>%
      count(Origin)
    
    p <- ggplot(
      origin_data,
      aes(
        x = reorder(Origin, n),
        y = n,
        text = paste(
          "Origin:",
          Origin,
          "<br>Flights:",
          n
        )
      )
    ) +
      
      geom_col() +
      
      coord_flip() +
      
      labs(
        title = "Flights by Origin Airport",
        x = "Origin",
        y = "Number of Flights"
      ) +
      
      theme_minimal()
    
    ggplotly(
      p,
      tooltip = "text"
    )
    
  })
  output$destination_plot <- renderPlotly({
    
    destination_data <- flights %>%
      count(Destination)
    
    p <- ggplot(
      destination_data,
      aes(
        x = reorder(Destination, n),
        y = n,
        text = paste(
          "Destination:",
          Destination,
          "<br>Flights:",
          n
        )
      )
    ) +
      
      geom_col() +
      
      coord_flip() +
      
      labs(
        title = "Flights by Destination Airport",
        x = "Destination",
        y = "Number of Flights"
      ) +
      
      theme_minimal()
    
    ggplotly(
      p,
      tooltip = "text"
    )
    
  })
  output$distance_plot <- renderPlotly({
    
    p <- ggplot(
      flights,
      aes(
        x = Distance
      )
    ) +
      
      geom_histogram(
        bins = 30
      ) +
      
      labs(
        title = "Flight Distance Distribution",
        x = "Distance",
        y = "Number of Flights"
      ) +
      
      theme_minimal()
    
    ggplotly(p)
    
  })
  output$delay_distribution <- renderPlotly({
    
    p <- ggplot(
      flights,
      aes(
        x = Delay
      )
    ) +
      
      geom_histogram(
        bins = 30
      ) +
      
      labs(
        title = "Distribution of Flight Delays",
        x = "Delay (minutes)",
        y = "Number of Flights"
      ) +
      
      theme_minimal()
    
    ggplotly(p)
    
  })
  output$delay_status <- renderPlotly({
    
    status_data <- flights %>%
      count(Delay_Status)
    
    p <- ggplot(
      status_data,
      aes(
        x = Delay_Status,
        y = n,
        text = paste(
          "Status:",
          Delay_Status,
          "<br>Flights:",
          n
        )
      )
    ) +
      
      geom_col() +
      
      labs(
        title = "On-Time vs Delayed Flights",
        x = "Flight Status",
        y = "Number of Flights"
      ) +
      
      theme_minimal()
    
    ggplotly(
      p,
      tooltip = "text"
    )
    
  })
  output$flight_table <- renderDT({
    
    datatable(
      flights,
      options = list(
        pageLength = 10,
        scrollX = TRUE
      ),
      filter = "top"
    )
    
  })
  
  
}
shinyApp(
  ui = ui,
  server = server
)