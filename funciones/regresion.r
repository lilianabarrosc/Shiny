
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
    optionsModel("Linear Regression", "alertlm", uiOutput("select_linearModel"), 
                 tags$div( class = 'col-sm-2', bsButton("apply_lm", label = "Apply",
                          style = "success"))),
    tabBox(width = 12,
           tabPanel("Model", verbatimTextOutput("summary_lm")),
           tabPanel("Prediction", "It shows the predicted value for each testing 
                    instance provided by the user.", 
                    verbatimTextOutput("resultValidation_lm")),
           tabPanel("Colinearity Test",
                    "Result", verbatimTextOutput("colinearity_result"),
                    "Tolerance", verbatimTextOutput("colinearity_tolerance"),
                    "Means", verbatimTextOutput("colinearity_mean"), br(),
                    "Your can be applied here:",
                    tags$ul(tags$li(
                      p(style = "text-align:justify;","If the largest colinearity test is greater than 10 then 
                        there is cause for concern (Bowerman & O'Connell. 1990; Myres, 1990).")
                        )),
                    tags$ul(tags$li(
                      p(style = "text-align:justify;","If the average colinearity test is substantially greater 
                        than 1 then the regression may be biased ( Bowerman & O'Connell. 1990; Myres, 1990).")               
                        )),
                    tags$ul(tags$li(
                      p(style = "text-align:justify;","Tolerance below 0.1 indicates a serious problem.")               
                    )),
                    tags$ul(tags$li(
                      p(style = "text-align:justify;","Tolerance below 0.2 indicates a potencial problem
                        (Menard, 1995).")               
                      ))
                  )
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
    optionsModel("Partial Least Squares Regression", "alertpls", 
                 column(6, uiOutput("select_pls")),
                 column(6, uiOutput("componentes_pls"),
                        selectInput("crosval", label = h4("Cross validation"), 
                                    choices = c("TRUE","FAlSE"), selected = "TRUE"),
                        tags$div( class = 'col-sm-2', bsButton("apply_pls", label = "Apply",
                                                               style = "success")))
                 ),
    tabBox(width = 12,
           tabPanel("Model",h4("PLS result"),
                    verbatimTextOutput("result_pls"), #coeficientes estandar
                    h4("Statistical pls"),
                    verbatimTextOutput("statistical_pls") #resultado obtenido
            ),
           tabPanel("Prediction", verbatimTextOutput("resultValidationpls")),
           tabPanel("Colinearity Test", plotOutput("plotPLS"))
    )
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
    optionsModel("Ridge", "alertRidge", uiOutput("select_ridge"), 
                 tags$div( class = 'col-sm-2', bsButton("apply_ridge", label = "Apply",
                                                        style = "success"))),
    tabBox(width = 12,
           tabPanel("Model", verbatimTextOutput("result_cvridge"),  verbatimTextOutput("result_ridge")),
           tabPanel("Prediction", "It shows the predicted value for each testing 
                    instance provided by the user."
                    ),
           tabPanel("Parameter optimization", plotOutput("plot_ridge"))
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

#vista para las opciones de entrada de cada modelo
#como entrada recibe el nombre del modelo el id del alert y las opciones del modelo
optionsModel <- function(nameModel, idAlert, options, otherOptions){
  box(
    width = 12, title = nameModel, solidHeader = TRUE, status = "success",
    p(style = "text-align:justify;","By default, the system will use", em("all"), "the independent variables to predict
      the last column in the data.", br(),
      "If you wish, you may use the selector tool to predict something else."),
    bsAlert(idAlert),
    #opciones de entrada para el modelo
    options,
    otherOptions
  )
}

#Funcion que particiona el data set en train y test segun un porcentaje dado
dataPartition <- function(data, porcent){
  dataset <- data.frame(data)
  smp_size <- floor(porcent * nrow(dataset))
  print(smp_size)
  set.seed(123)
  return(sample(seq_len(nrow(dataset)), size = smp_size))
}

#funcion general para la validacion cruzada, recibe como parametro el modelo generado, 
#los coeficientes a predecir (con el formato theta.predict <- function(fit,x){
                  # cbind(1,x)%*%fit$coefficients         
                  # }) del modelo y la variable de respuesta y los predictores;)
#Retorna un data frame con la prediccion realizada y la variable de respuesta a modo de comparacion.
crossValidation <- function(model, theta.predict, y, x){
  theta.fit <- function(x,y){model}
#   theta.predict <- function(fit,x){
#     cbind(1,x)%*%fit$coefficients         
#   }
  response <- bootstrap::crossval(x, y, theta.fit, theta.predict, ngroup = 10)
  prediction <- data.frame(cbind(response$cv.fit, y))
  names(prediction) <- c("Prediction", "response_variable")
  return(prediction)
}

#funcion que devuelve los predictores a utilizar en el modelo, si no se especifican,
#por defecto se incluyen todos, menos la variable a predecir. x = variables predictoras e
# y = variable a predecir
predictors <- function(dadaSet, x, y){
    if(is.null(x)){
      return (dadaSet[, !names(dadaSet) %in% y])
    }else{
      predictors <- datav[, !names(dadaSet) %in% y]
      return(predictors[,x])
    }
}