#install.packages('shiny')
library('shiny')
library('shinydashboard')


header <- dashboardHeader(title = ":)")

sidebar <- dashboardSidebar(disable = "true",sidebarMenu(
  menuItem("Dashboard", tabName = "dsh", icon = icon("list-alt"))))

body <- dashboardBody(projects)

elements <-  fluidRow(
  valueBox( "New Orders",icon = icon("list"), color = "purple")
  
)

#navbar <- navbarPage("", tabPanel("Data", projects),
#                 tabPanel("exploratory analysis"),
#                 tabPanel("to train"),
#                 tabPanel("predict"),
#                tabPanel("validation"),
#                 tabPanel("regression"))
    
projects <- fluidPage(
              fluidRow(
                box(
                  title = "Project", width = 10, solidHeader = TRUE, status = "primary",
                  actionButton("new project", label = "New Project"),hr(),
                  dataTableOutput('table')
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


