library(shinydashboard)
library(leaflet)
shinyUI(dashboardPage(
  dashboardHeader(), 
  dashboardSidebar(
    sidebarUserPanel(name = "Xiao Jia",
                     image = "https://www.finance-monthly.com/Finance-Monthly/wp-content/uploads/2017/08/patients-slider.jpg"),
    sidebarMenu(
      menuItem("Introduction", tabName = "introduction", icon = icon("database")),
      menuItem("World map 1", tabName = "map1", icon = icon("map")),
      menuItem("World map 2", tabName = "map2", icon = icon("map")),
      menuItem("Histogram", tabName = "hist", icon = icon("database")),
      menuItem("Boxplot", tabName = "boxplot", icon = icon("database")))
  ),
  
dashboardBody(
  tabItems(
    tabItem(tabName = "introduction", 
            h2("Ttile"),
            br(),
            h4("This is the intro of breast and prostate cancer.")),
    
    tabItem(tabName = "map1", 
            fluidPage(
              leafletOutput("map1")
            )), 
    
    tabItem(tabName = "map2", 
            fluidPage(
              leafletOutput("map2")
            )),
    
    tabItem(tabName = "hist",
            selectInput(inputId = "ct",
                        label = 'select trial information',
                        choices = information,
                        selected = "Recruitment"), #select shows at the top
             fluidRow(
               plotOutput('hist1'),
               plotOutput('hist2'))
            ),
    
    tabItem(tabName = "boxplot",
            selectInput(inputId  = "cy",
                        label = 'select trial information',
                        choices = information,
                        selected = "Sponsor.Collaborators"), #select shows at the top
            fluidRow(
              plotOutput('boxplot1')
              )
            )
    )
  ))
)

