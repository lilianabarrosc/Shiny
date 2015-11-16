
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
      )
    )
  )
}
