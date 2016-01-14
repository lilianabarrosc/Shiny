linearRegression <- function() {
  fluidRow(
    column(width = 12,
           HTML('
              <ul class="breadcrumb">
              <li>Regression</li>
              <li>Linear Regression</li>
              </ul>')
          ),
     box(
       width = 12, title = "Linear Regression", solidHeader = TRUE, status = "success",
       "By default, the system will use", em("all"), "the independent variables to predict
       the last column in the data.", br(),
       "If you wish, you may use the selector tool to predict something else.",
       #opciones de entrada
       uiOutput("select_box_lm_y"),
       uiOutput("select_box_lm_x"),
       #actionButton("button_lm", "Apply")
       verbatimTextOutput("summary_lm")
     )
  )
}