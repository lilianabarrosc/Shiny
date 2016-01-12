data_sets <- list("iris" = 1, "airquality" = 2, "cars" = 3, "new" = 4)
#vista que muestra el listado de data set utilizados
viewData <- function() {
  fluidPage(
    fluidRow(
      box(width = 12, status = "success",
        radioButtons("select_file", label = h3("Select data Set"), selected = 2,
                             choices = data_sets
        ),
          # Salida de componente dinamico (subir archivo)
          uiOutput("ui")

        #dataTableOutput('contents')
      ),
        tabBox(width = 12,
        tabPanel("STR", verbatimTextOutput("str_data")),
        tabPanel("Summary", verbatimTextOutput("summary_data")) #dataTableOutput(outputId="summary_data")
        )          
      #  )
    #HTML("</div>")
    )
  )
}
