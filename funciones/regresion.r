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
       width = 12, title = "Validation type", solidHeader = TRUE, status = "success",
       radioButtons("select_validation", label = "", selected = 1,
                    choices = list("10xCV" = 1, "Test file" = 2, "% test" = 3)),
       uiOutput("ui_lm")
     ),
     box(
       width = 12, title = "Linear Regression", solidHeader = TRUE, status = "success",
       "By default, the system will use", em("all"), "the independent variables to predict
       the last column in the data.", br(),
       "If you wish, you may use the selector tool to predict something else.",
       #opciones de entrada
       uiOutput("select_lm_y"),
       uiOutput("select_lm_x"),
       #actionButton("button_lm", "Apply")
       verbatimTextOutput("summary_lm")
     ),
    box(
      width = 12, title = "Prediction", solidHeader = TRUE, status = "success",
      "It shows the predicted value for each testing instance provided by the user.",
      br(),
      conditionalPanel("input.select_validation == 1",
                       plotOutput("crossPlot")
      ),
      verbatimTextOutput("resultValidation_lm")
    )
  )
}

pls <- function(){
  fluidRow(
    column(width = 12,
           HTML('
                <ul class="breadcrumb">
                <li>Regression</li>
                <li>Partial Least Squares Regression (PLS)</li>
                </ul>')
           ),
    box(
      width = 12, title = "Partial Least Squares Regression", solidHeader = TRUE, status = "success",
      "By default, the system will use", em("all"), "the independent variables to predict
       the last column in the data.", br(),
      "If you wish, you may use the selector tool to predict something else.",
      bsAlert("alertpls"),
      column(6,
             uiOutput("select_plsy"),
             numericInput("comps", h4("Components"), min = 2, value = 2)
      ),
      column(6,
             uiOutput("select_plsx"),
             selectInput("crosval", label = h4("Cross validation"), 
                  choices = c("TRUE","FAlSE"), selected = "TRUE")
      ),
      #actionButton("button_lm", "Apply")
      uiOutput("select_pls"),
      h4("Standard coefficients"),
      verbatimTextOutput("coef_pls"), #coeficientes estandar
      h4("Statistical pls"),
      verbatimTextOutput("statistical_pls"), #resultado obtenido
      plotOutput("plotPLS")
    )
#     box(
#       width = 12, title = "Prediction", solidHeader = TRUE, status = "success",
#       "It shows the predicted value for each testing instance provided by the user.",
#       br(),
#       verbatimTextOutput("resultValidationpls")
#     )
  )
}