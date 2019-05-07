#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(tidyverse)
library(ggthemes)
library(readxl)
library(lubridate)
theme_set(theme_minimal())
library(shiny)
library(data.table)
morrison <- read_excel("morrison.xlsx", 
                       sheet = "analysis", col_types = c("text", 
                                                         "numeric", "numeric", "numeric"))

morrison <- morrison %>% 
  mutate(
    Date_Time = mdy_hm(Date_Time)
  )

morrison <- morrison %>%
  group_by(Hour = hour(Date_Time)) %>%
  gather(key = "metric", value = "kW", demand:difference)

morrison_by_hour <- morrison %>%
  separate(Date_Time, into = c("Date", "Time"), sep = " ") %>%
  select(-Time) %>%
  mutate(
    Date = ymd(Date)
  )

morrison_by_hour_dt <- as.data.table(morrison_by_hour)

ui <- fluidPage(
  sidebarLayout(
    sliderInput("DatesMerge",
                "Dates:",
                min = ymd("2018-06-10"),
                max = ymd("2019-02-15"),
                value = ymd("2018-06-10"),
                timeFormat = "%F"),
  mainPanel(
    plotOutput("lineChart")
  )
 )
)

# Define server logic required to draw a linechart
server <- function(input, output) {
  output$lineChart <- renderPlot({
    DatesMerge <- input$DatesMerge
    
    ggplot(morrison_by_hour_dt[ymd(Date) == ymd(DatesMerge)], aes(Hour, kW, color = metric)) +
      geom_line() +
      scale_color_colorblind("Legend",
                             labels = c("Demand", "Net Demand", "Solar Generation"))
  })
}



# Run the application 
shinyApp(ui = ui, server = server)
