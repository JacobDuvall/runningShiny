library(shiny)
library(shinythemes)
library(DT)

shinyUI(fluidPage(theme = shinytheme("sandstone"),
  
  # Application title
  h2("Running Analytics (Road Running Records by Distance)", align = "center"),
  h4("Jacob Duvall", align = "center"),
  h4("Originally Published on February 4, 2019", align = "center"),
  sidebarLayout(
    sidebarPanel(
    selectInput("plotg", "Choose Race Option:",
                list(`options` = c("Marathon", "Half Marathon", "10,000 Meters", "5,000 Meters", "1 Mile")
                )
    ),
    selectInput("rd1", "Select Gender:",
                list(`options` = c("Male", "Female", "Both")
                )
    ),
    checkboxInput("table", label = "Show Data Table", value = TRUE),
    selectInput("additional", "Show Additional Analytics:",
                list(`options` = c("Nationality Bar Graph", "Nationality Pie Chart")))
  ),
  mainPanel(
    plotOutput("distPlot"),
    plotOutput("pie"),
    dataTableOutput("data")
  )
)))
