
#Funcion que muestra la vista de Ten fold cross
tenFoldCross <- function() {
  fluidRow(
    column(width = 12,
           HTML('
                <ul class="breadcrumb">
                <li>Validation</li>
                <li>Ten fold cross</li>
                </ul>')
    ),
    box(
      width = 12, title = "Ten fold cross", solidHeader = TRUE, status = "success",
      plotOutput("crossPlot"),
      verbatimTextOutput("validationTFC")
    )
  )
}