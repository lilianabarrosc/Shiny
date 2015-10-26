#install.packages('shiny')
#install.packages("shinydashboard")
library('shinydashboard')
library('shiny')

header <- dashboardHeader(title = ":)")

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Dashboard", tabName = "Dsh", icon = icon("dashboard")),
    menuItem("Polynomial", tabName = "polynomial", icon("cog", lib = "glyphicon"),
             menuSubItem("PLS", tabName = "pls", icon = shiny::icon("angle-double-right")),
             menuSubItem("RGML", tabName = "rgml", icon = shiny::icon("angle-double-right"))
             ),
    menuItem("Neuronal", tabName = "neuronal", icon("cog", lib = "glyphicon")),
    menuItem("SVM", tabName = "svm", icon("cog", lib = "glyphicon")),
    menuItem("Function", tabName = "function", icon("cog", lib = "glyphicon"))
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
      # Tab de la visualizacion
      tabItem(tabName = "pls",
              # ordeno el contenido por columna
              fluidRow(
                box(
                  title = "PLS parameters", width = 12, solidHeader = TRUE, status = "primary",
                  textInput("ncomp", label = "ncomp", value = "name..."),
                  textInput("yadd", label = "y.add", value = "name..."),
                  textInput("naction", label = "na.action", value = "name...")
                ),
                box(
                  title = "Results", width = 12, solidHeader = TRUE, status = "primary",
                  verbatimTextOutput("value")
                )
              )
            ),
      tabItem(tabName = "rgml",
              # ordeno el contenido por columna
              fluidRow(
                box(
                  title = "RGML parameters", width = 12, solidHeader = TRUE, status = "primary",
                  textInput("type", label = "type", value = "name...")
                ),
                box(
                  title = "Results", width = 12, solidHeader = TRUE, status = "primary",
                  verbatimTextOutput("value")
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



