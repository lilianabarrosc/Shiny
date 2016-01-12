linearRegression <- function() {
  fluidRow(
    column(width = 12,
           HTML('
              <ul class="breadcrumb">
              <li>Train</li>
              <li>Linear Regression</li>
              </ul>')
          ),
    box(
      width = 12, title = "Inputs", status = "success",
      #opciones de entrada
      uiOutput("select_box_lm_y"),
      uiOutput("select_box_lm_x"),
      #actionButton("button_lm", "Apply")
      verbatimTextOutput("summary_lm")
    )
  )
}