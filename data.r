
#vista que muestra el listado de data set utilizados
viewData <- function() {
  fluidPage(
    fluidRow(
      box(
        title = "Data", width = 12, solidHeader = TRUE, status = "primary", 
        radioButtons("select_file", label = h3("Select data Set"), selected = 2,
                             choices = data_sets
        ),
          # Salida de componente dinamico (subir archivo)
          uiOutput("ui")

        #dataTableOutput('contents')
      ),
     # HTML("<div class='col-sm-12' style='min-width: 500px !important;'>"),
#         box(
#           title = "Summay", width = 12, status = "primary", collapsible = TRUE, height = '500px',
        tabBox(width = 12,
        tabPanel("STR", verbatimTextOutput("str_data")),
        tabPanel("Summary", verbatimTextOutput("summary_data")) #dataTableOutput(outputId="summary_data")
        )          
      #  )
    #HTML("</div>")
    )
  )
}
