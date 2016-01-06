data_sets <- list("iris" = 1, "airquality" = 2, "cars" = 3, "new" = 4)
#vista que muestra el listado de data set utilizados
viewData <- function() {
  fluidPage(
    fluidRow(
      box(
        title = "Data", width = 12, solidHeader = TRUE, status = "warning", 
        radioButtons("select_file", label = h3("Select data Set"), selected = 2,
                             choices = data_sets
        ),
          # Salida de componente dinamico (subir archivo)
          uiOutput("ui")

        #dataTableOutput('contents')
      ),
        tabBox(width = 12,
          HTML("style {
          .nav-tabs-custom>.nav-tabs>li.active {
               border-top-color: #f39c12;}
               }")
        ,
        tabPanel("STR", verbatimTextOutput("str_data")),
        tabPanel("Summary", verbatimTextOutput("summary_data")) #dataTableOutput(outputId="summary_data")
        )          
      #  )
    #HTML("</div>")
    )
  )
}
