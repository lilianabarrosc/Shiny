#install.packages('shiny')
library('shiny')
library('shinydashboard')


header <- dashboardHeader(title = ":)")

sidebar <- dashboardSidebar(disable = "true",sidebarMenu(
  menuItem("Dashboard", tabName = "dsh", icon = icon("list-alt"))))

projects <- fluidPage(
  fluidRow(
    column(width = 1),
    box(
      title = "Project", width = 10, solidHeader = TRUE, status = "primary",
      tags$div( class = 'col-sm-6',
              actionButton("new", icon = icon("eye"),
               tags$a(href="../newproject/", "New project"))
              ),
      tags$div( class = 'col-sm-6',
                textInput("search", label = "Search")
               # tags$label("Search",
               #             tags$input( type="search", class= "form-control input-sm"
               #            )
                #)
              ),
      #dataTableOutput('table')
      #tags$textarea("text", rows="4", cols="50", style="resize: none")
      tags$hr(),
      tags$table( class="table table-striped table-hover dataTable", style="width:100%",
               tags$tr(
                 tags$th("Name"),
                 tags$th("Description"),
                 tags$th("Date"),
                 tags$th("Num Files"),
                 tags$th("Actions")
               ),
               tags$tr(
                 tags$td("proyecto 1"),
                 tags$td("Descripcion proy 1"),
                 tags$td("10/10/2015"),
                 tags$td("3"),
                 tags$td(
                   tags$div( class = 'btn-group',
                          actionButton("view", icon = icon("eye"),
                          tags$a(href="../viewData/", "View")),
                          actionButton("edit", label = "Edit", icon = icon("edit")),
                          actionButton("delete", label = "Delete", icon = icon("trash"))
                   )
                 )
               )
      )
    )
  )
) 

body <- dashboardBody(includeCSS("../custom.css"), projects)

elements <-  fluidRow(
  valueBox( "New Orders","",icon = icon("list"), color = "purple")
  
)

#navbar <- navbarPage("", tabPanel("Data", projects),
#                 tabPanel("exploratory analysis"),
#                 tabPanel("to train"),
#                 tabPanel("predict"),
#                tabPanel("validation"),
#                 tabPanel("regression"))
    


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


