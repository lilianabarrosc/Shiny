#install.packages('shiny')
#install.packages("shinydashboard")
library('shinydashboard')
library('shiny')

header <- dashboardHeader(title = ":)")

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Dashboard", tabName = "Dsh", icon = icon("dashboard")),
    menuItem("Ten fold cross", tabName = "tenfc"),
    menuItem("Test/traning", tabName = "test"),
    menuItem("5X2CV", tabName = "5x2")
  )
)

body <- dashboardBody(
  tabItems( 
    #Tab del Dashboard
    tabItem(tabName = "Dsh",
            h2("Dashboard tab content"),
            navbarPage("App Title",
                       tabPanel("Plot"),
                       tabPanel("Summary"),
                       tabPanel("Table"))),
    # Tab de Ten fold cross
    tabItem(tabName="tenfc",
            fluidRow(
              column(width = 12,
                     box(width = 12,
                         title = "Ten fold cross", status = "primary",
                         verbatimTextOutput("value")
                     )
              ),
              column(width = 12,
                     box(width = 12, title = "Grafics", solidHeader = TRUE,
                         collapsible = TRUE
                         
                     )
                     #plotOutput("plot1", height = 300)
              )
            )
    ),
    # Tab de Test/traning
    tabItem(tabName="test",
            fluidRow(
              column(width = 12,
                     box(width = 12,
                         title = "Ten fold cross", status = "primary",
                         numericInput("separacion", label = "Percentage removal", value = 1),
                         #cargar archivo csv o txt
                         fileInput("file", label = "File upload csv/txt",
                                   accept=c('text/csv', 
                                            'text/comma-separated-values,text/plain', 
                                            '.csv')
                         ),
                         h4("Results"),
                         verbatimTextOutput("value")
                     )
              ),
              column(width = 12,
                     box(width = 12, title = "Grafics", solidHeader = TRUE,
                         collapsible = TRUE
                         
                     )
                     #plotOutput("plot1", height = 300)
              )
            )
    ),
    # Tab de 5X2CV
    tabItem(tabName="5x2",
            fluidRow(
              column(width = 12,
                     box(width = 12,
                         title = "Ten fold cross", status = "primary",
                         verbatimTextOutput("value")
                     )
              ),
              column(width = 12,
                     box(width = 12, title = "Grafics", solidHeader = TRUE,
                         collapsible = TRUE
                         
                     )
                     #plotOutput("plot1", height = 300)
              )
            )
      )
  )
)

ui <- dashboardPage( title = "Exploratory analysis", skin = "purple",
                     header, #titulo
                     sidebar, #menu lateral
                     body #cuerpo
)

server <- function(input, output) {
  output$value <- renderPrint({ 'Model' })
}

shinyApp(ui, server)


#---------------------



