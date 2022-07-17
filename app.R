library(shiny)
library(dplyr)
require(readr)

bikes_data <- read_csv("https://firebasestorage.googleapis.com/v0/b/citybike-alan-velazquez.appspot.com/o/citibike-tripdata.csv?alt=media&token=f3729fa9-e954-4f12-b961-c08faa53d989")

number_of_users <- bikes_data %>%
  group_by(start_station_name) %>%
  summarize(Number_of_users_by_station = n())

number_of_users$Number_of_users_by_station <- as.numeric(number_of_users$Number_of_users_by_station)

number_of_users

max_value = max(number_of_users$Number_of_users_by_station)

min_value = min(number_of_users$Number_of_users_by_station)

median_value = median(number_of_users$Number_of_users_by_station)

data_bar <- number_of_users$Number_of_users_by_station
names(data_bar) <- number_of_users$start_station_name
data_bar

# Define UI for application that draws a scatterplot
ui <- fluidPage(
  
  # Application title
  titlePanel("Bike Project"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      sliderInput("numbers",
                  "Number of users",
                  min_value,
                  max_value,
                  median_value
      )
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("countryPlot"),
      dataTableOutput("dataTable")
    )
  )
)

# Define server logic required to draw a scatterplot
server <- function(input, output) {
  
  output$countryPlot <- renderPlot({
    numbers = input$numbers
    barplot(data_bar, col=ifelse(number_of_users$start_station_name >= numbers, "red","black"),
         main = "Number of users by station", xlab = "Station name", ylab = "Number of users")
    options(scipen=999)
  })
  
  output$dataTable <- renderDataTable({
    number_of_users
  })
}
# Run the application 
shinyApp(ui = ui, server = server)