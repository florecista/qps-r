library(shiny)
library(shinydashboard)

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Offences", startExpanded = TRUE,
      menuItem("Charts", startExpanded = TRUE, icon = icon("bar-chart-o"), tabName="charts",
         menuSubItem("Filled", tabName = "filledplot", selected = TRUE),
         menuSubItem("Stacked", tabName = "stackedplot")
      )
    ),
    menuItem("2017 Summary", startExpanded = FALSE,
             menuItem("Charts", icon = icon("bar-chart-o"), tabName="charts",
                      menuSubItem("Offences", tabName = "pieplot"),
                      menuSubItem("Adults", tabName = "adultplot"),
                      menuSubItem("Juveniles", tabName = "juvenileplot")
             )
    )
  )
)

body <- dashboardBody(
  tabItems(
    tabItem(tabName = "filledplot",
            plotOutput("filledplot")
    ),
    
    tabItem(tabName = "stackedplot",
            plotOutput("stackedplot")
    ),
    tabItem(tabName = "pieplot",
            plotOutput("pieplot")
    ),
    
    tabItem(tabName = "adultplot",
            plotOutput("adultplot")
    ),
    tabItem(tabName = "juvenileplot",
            plotOutput("juvenileplot")
    )
  )
)

dashboardPage(
  dashboardHeader(title="QPS Data in R"),
  sidebar,
  body
)