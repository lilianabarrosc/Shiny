#vista del modelo lineal
linearRegression <- function() {
  fluidRow(
    column(width = 12,
           HTML('
              <ul class="breadcrumb">
              <li>Regression</li>
              <li>Linear Regression</li>
              </ul>')
          ),
    validation("validationType_lm", "fileTest_lm", "porcentTest_lm"),
#      box(
#        width = 12, title = "Validation type", solidHeader = TRUE, status = "success",
#        radioButtons("select_validation", label = "", selected = 1,
#                     choices = list("10xCV" = 1, "Test file" = 2, "% test" = 3)),
#        uiOutput("ui_lm")
#      ),
     box(
       width = 12, title = "Linear Regression", solidHeader = TRUE, status = "success",
       p(style = "text-align:justify;","By default, the system will use", em("all"), "the independent variables to predict
       the last column in the data.", br(),
       "If you wish, you may use the selector tool to predict something else."),
       bsAlert("alertlm"),
       #opciones de entrada
       uiOutput("select_lm"),
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

#vista del modelo pls
pls <- function(){
  fluidRow(
    column(width = 12,
           HTML('
                <ul class="breadcrumb">
                <li>Regression</li>
                <li>Partial Least Squares Regression (PLS)</li>
                </ul>')
           ),
    validation("validationType_pls", "fileTest_pls", "porcentTest_pls"),
    box(
      width = 12, title = "Partial Least Squares Regression", solidHeader = TRUE, status = "success",
      p(style = "text-align:justify;","By default, the system will use", em("all"), "the independent variables to predict
       the last column in the data.", br(),
      "If you wish, you may use the selector tool to predict something else."),
      bsAlert("alertpls"),
      column(6,
             uiOutput("select_pls")
      ),
      column(6,
             uiOutput("componentes_pls"),
             selectInput("crosval", label = h4("Cross validation"), 
                  choices = c("TRUE","FAlSE"), selected = "TRUE")
      ),
      column(12,
          #actionButton("button_lm", "Apply")
          h4("PLS result"),
          verbatimTextOutput("result_pls"), #coeficientes estandar
          h4("Statistical pls"),
          verbatimTextOutput("statistical_pls"), #resultado obtenido
          plotOutput("plotPLS")
      )
    )
#     box(
#       width = 12, title = "Prediction", solidHeader = TRUE, status = "success",
#       "It shows the predicted value for each testing instance provided by the user.",
#       br(),
#       verbatimTextOutput("resultValidationpls")
#     )
  )
}

#vista del modelo ridge
ridge <- function() {
  fluidRow(
    column(width = 12,
           HTML('
                <ul class="breadcrumb">
                <li>Regression</li>
                <li>Ridge</li>
                </ul>')
           ),
    validation("validationType_ridge", "fileTest_ridge", "porcentTest_ridge"),
    box(
      width = 12, title = "Ridge", solidHeader = TRUE, status = "success"
    )
  )
}

#vista del modelo rglm
rglm <- function() {
  fluidRow(
    column(width = 12,
           HTML('
                <ul class="breadcrumb">
                <li>Regression</li>
                <li>Random General Linear Model</li>
                </ul>')
           ),
    validation("validationType_rglm", "fileTest_rglm", "porcentTest_rglm"),
    box(
      width = 12, title = "Random General Linear Model", solidHeader = TRUE, status = "success"
    )
           )
}

#vista para la validación de los modelos recibe el id de los componentes, es decir,
#idValidation para seleccionar el tipo de validacion, el idFileTest para el archivo
#proporcionado para test y idPorcentTest para particionar test y train en carpetas de %.
#validacion 10kfold, test/train y % test
validation <- function(idValidationType, idFileTest, idPorcentTest){
  box(
    width = 12, title = "Validation type", solidHeader = TRUE, status = "success",
    radioButtons(idValidationType, label = "", selected = 1,
       choices = list("10xCV" = 1, "Test file" = 2, "% test" = 3)),
    #salida dinamica para subir archivo de test
    conditionalPanel(paste("input.", idValidationType, "==2", sep = ''),
       fileInput(idFileTest, 'Test File',
                 accept=c('text/csv', 'text/comma-separated-values,text/plain','.csv'))
    ),
    #salida dinamica para particionar train/test
    conditionalPanel(paste("input.", idValidationType, "==3", sep = ''),
       numericInput(idPorcentTest, "Train size in %", 0.75, min = 0.0, max = 1, step = 0.02)
    )
  )
}