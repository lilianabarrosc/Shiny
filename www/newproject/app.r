#install.packages('shiny')
library('shiny')
library('shinydashboard')


header <- dashboardHeader(title = ":)")

sidebar <- dashboardSidebar(disable = "true",sidebarMenu(
  menuItem("Dashboard", tabName = "dsh", icon = icon("list-alt"))))

newProjects <- fluidPage(
  fluidRow(
    column(width = 1),
    box(
      title = "New project", width = 10, solidHeader = TRUE, status = "primary",
      tags$a(href="../project/", "Back!"),
      textInput("name", label = "Name", value = "name..."),
      textInput("description", label = "Description", value = "description..."),
      #cargar archivo csv o txt
      fileInput("file", label = "File upload csv/txt",
                accept=c('text/csv', 
                         'text/comma-separated-values,text/plain', 
                         '.csv')
      ),
      fluidRow(
        column(width = 4, ""),
        column(width = 2, actionButton("save", label = "Save")),
        column(width = 4, actionButton("cancel", label = "Cancel"))
      )
    )
  )
)

navBar <- navbarPage("", tabPanel("Data", newProjects),
                     tabPanel("exploratory analysis"),
                     tabPanel("to train"),
                     tabPanel("predict"),
                     tabPanel("validation"),
                     tabPanel("regression"))

body <- dashboardBody(navBar)


ui <- dashboardPage(header, sidebar, body)


server <- function(input, output) {
  # You can access the value of the widget with input$newProject, e.g.
  output$valuenewproject <- renderPrint({ input$newProject })
  
  #inFile <- input$file
  
  #if (is.null(inFile))
   # return(NULL)
  
  #read.csv(inFile)
  
}

#para cambiar de vistas, cambio el ui dependiendo si se presiona el boton new o no

shinyApp(ui, server)


