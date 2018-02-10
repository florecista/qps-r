library(shiny)
library(shinydashboard)

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Charts", icon = icon("bar-chart-o"), startExpanded = TRUE,
             menuSubItem("Pie", tabName = "pieplot", selected = TRUE),
             menuSubItem("Filled", tabName = "filledplot"),
             menuSubItem("Stacked", tabName = "stackedplot")
    )
  )
)

body <- dashboardBody(
  tabItems(
    tabItem(tabName = "pieplot",
            plotOutput("pieplot")
    ),
    
    tabItem(tabName = "filledplot",
            plotOutput("filledplot")
    ),
    
    tabItem(tabName = "stackedplot",
            plotOutput("stackedplot")
    )
  )
)

dashboardPage(
  dashboardHeader(title="QPS Data in R"),
  sidebar,
  body
)