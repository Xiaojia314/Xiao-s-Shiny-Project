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
      menuItem("4 Phases", tabName = "phases", icon = icon("database")),
      menuItem("Histogram", tabName = "hist", icon = icon("database")),
      menuItem("Boxplot", tabName = "boxplot", icon = icon("database")))
  ),
  
dashboardBody(
  tabItems(
    tabItem(tabName = "introduction", 
            h2("Background"),
            br(),
            h4("Clinical trials are experiments or observations done in clinical research.The research studies on human participants are designed to participate in the biomedical or behavioral interventions, including new treatments such as novel vaccines, drugs,  dietary supplements, and medical devices that warrant further study and comparison."),
            h4("Clinical trials contains 4 phases, and they are only conducted after they have received health authority committee approval in the country."),
            h2("Business Motivation"),
            h4("The pharmaceutical, biotech and medical device companies, even some governmental organizations spend millions on clinical trials every year.Depending on product types and development stages, investigators initially enroll volunteers or patients into small pilot studies, and conduct larger scale comparative studies progressively."),
            br(),
            h2("Clinial Trials(Breast cancer vs Prostate cancer)"),
            h4("Breast cancer rates the maxially in female and prostate cancer rates the maxially in male.")
            ),
    tabItem(tabName = "map1", 
            fluidPage(
              leafletOutput("map1")
            )), 
    
    tabItem(tabName = "map2", 
            fluidPage(
              leafletOutput("map2")
            )),
    tabItem(tabName = "phases", 
            h2("Four Phases in Clinial Trials"),
            br(),
            h4("Phase I  assess the safety of a drug or device."),
            br(),
            h4("Phase II  test the efficacy of a drug or device."),
            br(),
            h4("Phase III involves randomized and blind testing in hundreds to thousands patients."),
            br(),
            h4("Phase IV is often called Post Marketing Surveillance Trials, are conducted after a drug or device has been approved for consumer sale.")
            ),
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

