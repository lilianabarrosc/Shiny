#install.packages('shiny')
library('shiny')
library('shinydashboard')


header <- dashboardHeader(title = ":)")

sidebar <- dashboardSidebar(disable = "true",sidebarMenu(
  menuItem("Dashboard", tabName = "dsh", icon = icon("list-alt"))))

projects <- fluidPage(
  fluidRow(
    box(
      title = "Project", width = 12, solidHeader = TRUE, status = "primary",
      tags$div( class = 'col-sm-6',
                actionButton("back", label = "Back"), href="./www/projects"
      ),
      tags$div( class = 'col-sm-6',
                textInput("search", label = "Search")
                # tags$label("Search",
                #             tags$input( type="search", class= "form-control input-sm"
                #            )
                #)
      ),
      tags$label("Original data set: ",
               tags$link( "Data set 1"
                 )
      ),
      tags$hr(),
      tags$table( class="table table-striped table-hover dataTable", style="width:100%",
                  tags$tr(
                    tags$th("Data set"),
                    tags$th("Description"),
                    tags$th("Size"),
                    tags$th("Date"),
                    tags$th("Dimensions"),
                    tags$th("Instance"),
                    tags$th("Actions")
                  ),
                  tags$tr(
                    tags$td(""),
                    tags$td(""),
                    tags$td(""),
                    tags$td(""),
                    tags$td(""),
                    tags$td(""),
                    tags$td(
                      tags$div( class = 'btn-group',
                                actionButton("analyze", icon = icon("eye")
                                  tags$a(href="../exploratoryAnalysis/", "Analyze")),
                                actionButton("download", label = "Download", icon = icon("download")),
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


