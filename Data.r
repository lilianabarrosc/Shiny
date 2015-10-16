#install.packages('shiny')
library('shiny')
library('shinydashboard')


header <- dashboardHeader(title = ":)")

sidebar <- dashboardSidebar(disable = "true",sidebarMenu(
  menuItem("Dashboard", tabName = "dsh", icon = icon("list-alt"))))

body <- dashboardBody(navbar)


navbar <- navbarPage("", tabPanel("Data", 
                                  if(valuenewproject == 0)
                                    projects
                                  else
                                    newProjects
                                    ),
                 tabPanel("exploratory analysis"),
                 tabPanel("to train"),
                 tabPanel("predict"),
                 tabPanel("validation"),
                 tabPanel("regression"))
    
projects <- fluidPage(
              fluidRow(
                box(
                  title = "Project", width = 12, solidHeader = TRUE, status = "primary",
                  actionButton("new project", label = "New Project"),hr(),
                  dataTableOutput('table')
                )
              )
            ) 

newProjects <- fluidPage(
              fluidRow(
                box(
                  title = "New project", width = 10, solidHeader = TRUE, status = "primary",
                  actionButton("back", label = "Back"),
                  textInput("name", label = "Name", value = "name..."),
                  textInput("description", label = "Description", value = "description..."),
                  fileInput("file", label = "File upload"),
                  fluidRow(
                    Column(4, "algo"),
                    column(2, actionButton("save", label = "Save")),
                    column(4, actionButton("cancel", label = "Cancel"))
                  )
                )
              )
            )

ui <- dashboardPage(header, sidebar, body)


server <- function(input, output) {
  # You can access the value of the widget with input$newProject, e.g.
  output$valuenewproject <- renderPrint({ input$newProject })
  
  #datos que se muestran en la tabla
  output$table <- renderDataTable(iris,
                    options = list(
                      pageLength = 5,
                      initComplete = I("function(settings, json) {alert('Done.');}")
                    )
                  )
  
}

#para cambiar de vistas, cambio el ui dependiendo si se presiona el boton new o no

shinyApp(ui, server)


