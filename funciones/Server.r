library('shiny')
source('funciones/LOF.R')
source('funciones/dataBase.r')

#--------------------Servidor-------------------

server <- function(input, output, session) {
  #--------------> variables globales
  strx <- "Dependent variables"
  stry <- "Response variable"
  strz <- "Observations"
  titleAlert <- "Oops"
  
  #--------------> logo
  output$logo <- renderImage({
    list(
      src = "images/icon.png",
      contentType = 'image/png',
      width = 120,
      height = 50,
      alt = "Logo")
  }, deleteFile = FALSE)
  
  #--------------> conexion con la base de datos
  #CON <- reactiveValues(conexionbd())
  
  #obtencion de user y password desde la bd
  my_username <- c("test","admin") #dos usuarios test y admin
  my_password <- c("test","123") # las claves de los usuarios
  
  #-------------------------------------------------------
  #-----------------------> home <-----------------------
  
  output$home <- renderImage({
    list(
      src = "images/logo.png",
      contentType = 'image/png',
      width = 400,
      height = 250,
      alt = "Logo")
  }, deleteFile = FALSE)
  
  #---------------> login
  
  #inicio la variable user
  USER <- reactiveValues(Logged = FALSE,role=NULL)
  
  #se valida el nombre del usuario con la contraseña ingresada
  observe({ 
    if (USER$Logged == FALSE) {
      if (!is.null(input$Login)) {
        if (input$Login > 0) {
          #se obtienen los valores ingresados
          Username <- isolate(input$userName)
          Password <- isolate(input$passwd)
          #Se comparan los valores obtenidos con los registros
          Id.username <- which(my_username == Username)
          Id.password <- which(my_password == Password)
          if (length(Id.username) > 0 & length(Id.password) > 0) {
            if (Id.username == Id.password) { #si ambas variables son true el usuario es valido
              USER$Logged <- TRUE
              USER$role=get_role(Username)
              closeAlert(session, "alertLoginID")
            }
          }else {
            createAlert(session, "alertLogin", "alertLoginID", title = titleAlert,
                        content = "Username and password do not match.", 
                        style = "warning", append = FALSE)
          } 
        }
      }
    }
  })
  
  #Se muestran las opciones correspondientes para un usuario logeado o no
  observe({
    if(USER$Logged == FALSE){ # si no esta logeado, solo se muestra el home
      output$signIn <- renderUI({
        loginRegister()
      })
      #menu del sidebar
      output$side <- renderMenu({
        sidebar(FALSE)
      })
    }
    if(USER$Logged == TRUE){ # el usuario esta logeado, se muestran todas las opciones
      output$signIn <- renderUI({
        fluidPage(
          titlePanel("Welcome!")
        )
      })
      #menu del sidebar
      output$side <- renderMenu({
        sidebar(TRUE, USER$role)
      })
    }
  })
  
  #---------------> Register
  observe({
    if (!is.null(input$register)) {
      if (input$register > 0) { # se presiona el boton para registrarse
        if(input$newUserName == "" || input$name == "" || input$lastName == ""
           || input$email == "" || input$newPasswd == "" || input$confirmPasswd == ""){
          createAlert(session, "alertRegister", "alertRegisterID", title = titleAlert,
                      content = "All fields marked with * are required.", 
                      style = "warning", append = FALSE)
        }else if(input$newPasswd != input$confirmPasswd){
          createAlert(session, "alertRegister", "alertRegisterID", title = titleAlert,
                      content = "Password and confirm password do not match.", 
                      style = "warning",  append = FALSE)
        }else{ #Se puede registrar
          closeAlert(session, "alertRegisterID")
          #           #registro en la bd
          #           drv <- dbDriver("PostgreSQL")
          #           con <- conexionbd(drv)
          #           sql <- paste("insert into user_guinia (user_name,name,last_name,email,password) values (",
          #                        paste(input$newUserName,input$name,input$lastName,input$email,input$newPasswd, sep = ","),
          #                        ")")
          #           
          #           rs <- dbSendQuery(con, sql)
          #           desconexionbd(con, drv)
        }
      }
    }
  })
  
  #-------------------------------------------------------
  #-----------------------> data <-----------------------
  
  #----------> data set
  output$ui <- renderUI({
    if (is.null(input$select_file))
      return()
    switch(input$select_file,
           #Cargar archivo desde el equipo o mediante una URL
           '4' = fileInput('file1', 'Choose CSV File',
                           accept=c('text/csv', 'text/comma-separated-values,text/plain', 
                                    '.csv')),
           '5' = column(6, 
                        textInput("url", label = "URL (only csv) ", value = "https://dl.dropboxusercontent.com/u/12599702/autosclean.csv"),
                        actionButton("upload", label = "Upload")
           )
    )
  })
  
  #Seleccion de data set a utilizar
  file <- reactive({
    inFile <- input$file1
    if (is.null(input$select_file) && is.null(input$url))
      return()
    switch(input$select_file,
           '1'= iris,
           '2'= airquality,
           '3'= sleep,
           '4'= read.csv(inFile$datapath),
           '5'= if(input$upload)
             read.csv(input$url, sep = ";", dec = ",")
    )
  })
  
  #muestro un resumen del data set seleccionado
  output$str_data <- renderPrint({
    if (input$select_file==4 && is.null(input$file1))
      return()
    str(file())
  })
  
  #muestro un sumary del data set seleccionado
  output$summary_data <- renderPrint({
    if (input$select_file==4 && is.null(input$file1))
      return()
    summary(file())
  })
  
  #----------> dimensionalidad del archivo
  #Con dim puedo saber la cantidad de atributos y observaciones que posee el archivo,
  #en dim(data)[1] se pueden encontrar la cantidad de observaciones y en dim(data)[2] la 
  
  #*************************************************
  #----------> Sacar columnas con valores nominales
  only_file_nums <- reactive({
    aux <- data.frame(file())
    nums <- sapply(aux, is.numeric)
    aux[ , nums]
  }) 
  
  #***************************************
  #-----------------------> visualization
  
  #----------> Graficos de visualizacion
  
  #Actualizo el maximo del slider con el valor del tamaÃ±o del archivo seleccionado
  output$slider_range_range_density <- renderUI({
    treeSlider("x1", "y1", "z1", only_file_nums(), strx, stry, strz)
  })
  
  #seleccion de atributos y observaciones del data set
  dat1 <- reactive({
    only_file_nums()[input$z1[1]:input$z1[2],input$x1[1]:input$x1[2]]
  })
  
  scatterPlot <- reactive({ #funcion que genera el scatterPlot
    if(is.null(input$x1) || is.na(input$x1)){
      return()
    }
    
    #paleta de colores para el gráfico
    myPalette <- c(input$col1, input$col2, input$col3)
    withProgress({
      setProgress(message = "This may take a while...")
      ScatterplotMatrix(dat1(), c(input$x1[1]:input$x1[2]), only_file_nums()[,input$y1], 
                        names(only_file_nums())[[input$y1]], colours = myPalette)
    })
  })
  
  #Grafico correspondiente a scatterPlot 
  output$scatter1 <- renderPlot({
    tryCatch({
      closeAlert(session, "alertScatter1ID")
      scatterPlot()
    }, error = function(e) {
      createAlert(session, "alertScatter1", "alertScatter1ID", title = titleAlert,
                  content = paste("",e), 
                  style = "warning")
    })
    
  })
  
  #-------------->dowload image plot
  observe({
    output$downloadPlot <- downloadGeneral(input$radioScatterplot, scatterPlot())
  })
  
  #Slider visualizacion grafico parallel x e y
  output$slider_range_range_parallel <- renderUI({
    box(
      width = 6, status = "success",
      h4("Range"),
      sliderInput("x2", label = strx, min = 1, 
                  max = dim(only_file_nums())[2], value = c(1, dim(only_file_nums())[2])),
      sliderInput("y2", label = stry, min = 1, 
                  max = dim(file())[2], value = 2)
    )
  })
  
  #Slider visualizacion grafico parallel observations y alfa
  output$slider_range_range_parallel2 <- renderUI({
    box(
      width = 6, status = "success",
      h4("Range"),
      sliderInput("z2", label = strz, min = 1, 
                  max = dim(only_file_nums())[1], value = c(1, dim(only_file_nums())[1])),
      sliderInput("lineSize", label = "Line Size", min = 1, 
                  max = 5, value = 2),
      sliderInput("alphaLine", label = "Alpha Line", min = 0.01, 
                  max = 0.99, value = 0.11)
    )
  })
  
  
  #seleccion de atributos y observaciones del data set
  datParallelx <- reactive({
    only_file_nums()[input$z2[1]:input$z2[2],input$x2[1]:input$x2[2]]
  })
  
  datParallely <- reactive({
    file()[,input$y2]
  })
  
  #funcion que genera el grafico
  parallelplot <- reactive({
    if(is.null(input$x2) || is.na(input$x2)){
      return()
    }
    myPalette <- c(input$col1, input$col2, input$col3)
    # A ParallelPlot of all rows and all columns
    withProgress({
      setProgress(message = "This may take a while...")
      ParallelPlot(datParallelx(), seq(1,nrow(datParallelx()),1), seq(1,ncol(datParallelx()),1), datParallely(), 
                   names(file())[[input$y2]], 1, input$alphaLine, TRUE, colours = myPalette)
    })
  })
  
  #grafico paralelo
  output$parallel <- renderPlot({
    tryCatch({
      closeAlert(session, "alertParallelID")
      parallelplot()
    }, error = function(e) {
      createAlert(session, "alertParallel", "alertParallelID", title = titleAlert,
                  content = paste("",e), 
                  style = "warning")
    })
  })
  
  #-------------->dowload image plot
  observe({
    output$downloadPlotPrallel <- downloadGeneral(input$radioParallelplot, parallelplot())
  })
  
  #-------------------------------------------------------
  #-----------------------> Preprocessing <-----------------------
  
  #*********************************************
  #---------------> Graficos correspondientes a missing values
  
  missingV <- reactive({
    if(input$deleteMS)
      na.omit(only_file_nums())
    else
      only_file_nums()
  })
  
  #Slider visualizacion grafico de missing values Amelia
  output$slider_range_range_amelia <- renderUI({
    twoSlider("attributes","observation",missingV(),"Attributes",strz)
  })
  
  #Obtengo la seleccion de atributos y observaciones para la Opcion 1
  selectedData1 <- reactive({
    missingV()[input$observation[1]:input$observation[2], 
               input$attributes[1]:input$attributes[2]]
  })
  
  #Opcion 1 generacion del grafico boxPlot
  boxPlotfunction <- function(){
    if(is.null(input$attributes) || is.na(input$attributes)){
      return()
    }
    #missmap(selectedData1(), main = "Missing values vs observed") grafico amelia
    withProgress({
      setProgress(message = "This may take a while...")
      matrixplot(selectedData1())
    })
  }
  
  #Visualizacion del grafico
  output$missing1 <- renderPlot({
    tryCatch({
      closeAlert(session, "alertMissing1ID")
      boxPlotfunction()
    }, error = function(e) {
      createAlert(session, "alertMissing1", "alertMissing1ID", title = titleAlert,
                  content = paste("",e), 
                  style = "warning")
    })
  })
  
  #-------------->dowload image plot
  observe({
    output$downloadPlotboxplot <- downloadGeneral(input$radioboxplot, boxPlotfunction())
  })
  
  #Slider visualizacion grafico de missing VIM option2
  output$slider_range_range_option1 <- renderUI({
    twoSlider("attributes2","observation2",missingV(),"Attributes",strz)
  })
  
  #Obtengo la seleccion de atributos y observaciones para la Opcion 2
  selectedData2 <- reactive({
    missingV()[input$observation2[1]:input$observation2[2], 
               input$attributes2[1]:input$attributes2[2]]
  })
  
  #Opcion 2 (libreria VIM)
  output$missing2 <- renderPlot({
    if(is.null(input$attributes2) || is.na(input$attributes2)){
      return()
    }
    tryCatch({
      closeAlert(session, "alertmissing2ID")
      withProgress({
        setProgress(message = "This may take a while...")
        aggr(selectedData2(), col=c('red','dark grey'), numbers=TRUE, 
             sortVars=TRUE, labels=names(data), cex.axis=.8, gap=1, 
             ylab=c("Histogram of missing data","Pattern"))
      })
    }, error = function(e) {
      createAlert(session, "alertmissing2", "alertmissing2ID", title = titleAlert,
                  content = paste("",e), 
                  style = "warning")
    })
  })
  
  #Slider visualizacion grafico de missing VIM option2
  output$slider_range_range_option2 <- renderUI({
    treeSlider("x3", "y3", "z3", missingV(), strx, stry, strz)
  })
  
  #Obtengo la seleccion de atributos a comparar para la Opcion 3
  dat3 <- reactive({
    missingV()[input$z3[1]:input$z3[2],input$x3[1]:input$x3[2]]
  })
  
  #Option 3 (matricial)
  output$missing3 <- renderPlot({
    if(is.null(input$x3) || is.na(input$x3)){
      return()
    }
    tryCatch({
      closeAlert(session, "alertMissing3ID")
      scattmatrixMiss(dat3(), interactive = F, highlight = c(names(missingV())[[input$y3]]))
    }, error = function(e) {
      createAlert(session, "alertMissing3", "alertMissing3ID", title = titleAlert,
                  content = paste("",e), 
                  style = "warning")
    })
  })
  
  #************************************************
  #-------------> Eliminacion de ruido
  
  #   #Slider visualizacion grafico ruido
  #   output$slider_range_range_nremoval <- renderUI({
  #     twoSlider("attributes4","observation4",missingV(),"Attributes",strz)
  #   })
  
  #   #Se detectan las columnas con ruido
  #   columnsNoise <- reactive({
  #     if(is.na(noiseR() | is.null(noiseR()))){
  #       diffValues <- calculateDiff(missingV())
  #       getColumnsNoise(diffValues, input$limitNoise)
  #     } else {
  #       diffValues <- calculateDiff(noiseR())
  #       getColumnsNoise(diffValues, input$limitNoise)
  #     }
  #   })
  
  noiseR <- reactive({
    if(input$rnoise){
      diffValues <- calculateDiff(missingV())
      columnsNoise <- getColumnsNoise(diffValues, input$limitNoise)
      #  columnsNoise <- as.data.frame(columnsNoise[,1] + ncol(missingV()))
      as.data.frame(missingV()[,-columnsNoise[,1]])
    }else {missingV()}
  })
  
  #Slider visualizacion grafico parallel x e y
  output$slider_range_range_nremoval <- renderUI({
    box(
      width = 6, status = "success",
      h4("Range"),
      sliderInput("attributes4", label = strx, min = 1, 
                  max = dim(noiseR())[2], value = c(1, dim(noiseR())[2])),
      sliderInput("observation4", label = stry, min = 1, 
                  max = dim(file())[2], value = 2)
    )
  })
  
  #Slider visualizacion grafico parallel observations y alfa
  output$slider_range_range_nremoval2 <- renderUI({
    box(
      width = 6, status = "success",
      h4("Range"),
      sliderInput("ZNremoval", label = strz, min = 1, 
                  max = dim(noiseR())[1], value = c(1, dim(noiseR())[1])),
      sliderInput("lineSizeNremoval", label = "Line Size", min = 1, 
                  max = 5, value = 2),
      sliderInput("alphaLineNremoval", label = "Alpha Line", min = 0.01, 
                  max = 0.99, value = 0.11)
    )
  })
  
  #   #accion del boton noise removal
  #   noiseR <- eventReactive(input$rnoise, {
  #     dcolumnsNoise <- as.data.frame(columnsNoise()[,1] + countVariables)
  #     dataSet[,-columnsNoise[,1]]
  #   })
  
  #seleccion de atributos y observaciones del data set
  datNremovalx <- reactive({
    noiseR()[input$ZNremoval[1]:input$ZNremoval[2],
             input$attributes4[1]:input$attributes4[2]] 
  })
  
  #seleccion de la variable de respuesta
  datNremovaly <- reactive({
    file()[,input$observation4]
  })
  
  #grafico de ruido
  output$nremoval <- renderPlot({
    if(is.null(input$attributes4) || is.na(input$attributes4)){
      return()
    }
    myPalette <- c(input$col1, input$col2, input$col3)
    # A ParallelPlot of all rows and all columns
    withProgress({
      setProgress(message = "This may take a while...")
      ParallelPlot(datNremovalx(), seq(1,nrow(datNremovalx()),1), seq(1,ncol(datNremovalx()),1), datNremovaly(), 
                   names(file())[[input$observation4]], 1, input$alphaLineNremoval, TRUE, colours = myPalette)
    })
  })
  
  #Calcular el numero de columnas con ruido
  output$columnsNoise <- renderPrint({
    tryCatch({
      closeAlert(session, "alertNoiseID")
      diffValues <- calculateDiff(noiseR())
      columnsNoise <- getColumnsNoise(diffValues, input$limitNoise)
      paste("Have ", paste(nrow(columnsNoise), " noise columns."))
    }, error = function(e) {
      createAlert(session, "alertNoise", "alertNoiseID", title = titleAlert,
                  content = "Missing values in data", 
                  style = "warning")
    })
  })
  
  #************************************************
  #-------------> Local outlier factor
  
  ## scores for the original data
  outlier.scores <- reactive({
    lof(missingV(), k= c(5:10))
  })
  
  #Slider visualizacion grafico de missing VIM option2
  output$sliderLOF <- renderUI({
    minimo <- round(min(outlier.scores()), digits=2)
    maximo <- round(max(outlier.scores()), digits=2)
    sliderInput("thresholdt", "Threshold", min = minimo,
                max = maximo, value = 1.25, step= 0.01)
  })
  
  #llamado a la funcion lof, la cual devuelve una lista
  res <- reactive({
    if(is.na(outlier.scores()) && is.null(outlier.scores())){return}
    else{
      if(!is.null(input$thresholdt)){
        LOFCraft(data = missingV(), threshold = input$thresholdt, data.frame(outlier.scores())) ##calling LOF
      } else {
        LOFCraft(data = missingV(), data.frame(outlier.scores())) ##calling LOF
      }
    }
    
  })
  
  ## scores for the without outliers data
  withoutOutliers.scores <- reactive({
    data.frame(res()[1]) ## scores of data without outliers
  })
  
  #grafico inicial density plot
  output$densityPlot <- renderPlot({
    tryCatch({
      withProgress({
        closeAlert(session, "alertlof1ID")
        setProgress(message = "This may take a while...")
        DensityPlot(data.frame(outlier.scores()), ncol(data.frame(outlier.scores())))
      })
    }, error = function(e) {
      createAlert(session, "alertlof1", "alertlof1ID", title = titleAlert,
                  content = paste("",e), 
                  style = "warning")
    })
  })
  
  #Grafico resultante tras realizar corte del primer density
  output$densityPlotResult <- renderPlot({
    tryCatch({
      withProgress({
        closeAlert(session, "alertlof2ID")
        setProgress(message = "This may take a while...")
        DensityPlot(withoutOutliers.scores(), ncol(outlier.scores())) #Generating a plot of outliers scores
      })
    }, error = function(e) {
      createAlert(session, "alertlof2", "alertlof2ID", title = titleAlert,
                  content = paste("",e), 
                  style = "warning")
    })
  })
  
  #Cantaidad de outlier existentes
  output$howManyOutliers <- renderPrint({
    as.numeric(res()[3])
  })
  
  #Posicion de los outlier en el archivo
  output$posOutliers <- renderPrint({
    data.frame(res()[4])  ## the positions of the outliers in the original data and theirs respective scores
  })
  
  #without outliers data
  output$strWithoutOutliers <- renderPrint({
    dataWithoutOutliers<-data.frame(res()[2])  ##the data without outliers
    str(dataWithoutOutliers)
  })
  
  #************************************************
  #-------------> Normalization
  #salida dinamica de rango para normalizacion
  output$range <- renderUI({
    if (is.null(input$normalizationType))
      return()
    switch(input$normalizationType,
           '2' =  tags$div( class = 'col-sm-8',
                            tags$div( class = 'col-sm-4',
                                      numericInput("min", label = "Min", value = 0)
                            ),
                            tags$div( class = 'col-sm-4',
                                      numericInput("max", label = "Max", value = 1)
                            )
           ),
           '3'= tags$div( class = 'col-sm-8',
                          selectInput("type_normalization", label = "Other normalization", 
                                      choices = list("without normalization" = "n0", 
                                                     "standardization ((x-mean)/sd)" = "n1", 
                                                     "positional standardization ((x-median)/mad)" = "n2",
                                                     "unitization ((x-mean)/range)" = "n3", 
                                                     "positional unitization ((x-median)/range)" = "n3a",
                                                     "unitization with zero minimum ((x-min)/range)" = "n4",
                                                     "normalization in range <-1,1> ((x-mean)/max(abs(x-mean)))" = "n5",
                                                     "positional normalization in range <-1,1> ((x-median)/max(abs(x-median)))" = "n5a",
                                                     "quotient transformation (x/sd)" = "n6",
                                                     "positional quotient transformation (x/mad)" = "n6a",
                                                     "quotient transformation (x/range)" = "n7",
                                                     "quotient transformation (x/max)" = "n8",
                                                     "quotient transformation (x/mean)" = "n9",
                                                     "positional quotient transformation (x/median)" = "n9a",
                                                     "quotient transformation (x/sum)" = "n10",
                                                     "quotient transformation (x/sqrt(SSQ))" = "n11",
                                                     "normalization ((x-mean)/sqrt(sum((x-mean)^2)))" = "n12",
                                                     "positional normalization ((x-median)/sqrt(sum((x-median)^2)))" = "n12a",
                                                     "normalization with zero being the central point ((x-midrange)/(range/2))" = "n13") 
                          ),
                          radioButtons("type", label = "Type",
                                       choices = list("Column" = "column", "Row" = "row")
                          )
           )
    )
  })
  
  #obtengo el tipo de normalizacion seleccionada y aplico la normalizacion correspondiente
  normalization_type <- reactive({
    if (is.null(input$normalizationType))
      return()
    switch(input$normalizationType,
           '1'=  withProgress({
             setProgress(message = "This may take a while...")
             normalizeData(missingV())
           }),
           '2'=  withProgress({
             setProgress(message = "This may take a while...")
             normalizeData(missingV(), input$min, input$max)
           }),
           '3'=  withProgress({
             setProgress(message = "This may take a while...") 
             data.Normalization(missingV(),type=input$type_normalization ,normalization= input$type)
           })
    )
  })
  
  #muestro los primeros 10 atributos del data set original
  output$original_data <- renderPrint({#renderDataTable(
    missingV()[1:10,]
    #options = list(paging = FALSE, searching = FALSE)
  })
  
  #muestro os primeros 10 atributos del data set normalizado
  output$normalized_data <- renderPrint({#renderDataTable(
    normalization_type()[1:10,]
    #options = list(paging = FALSE, searching = FALSE)
  })
  
  #muestro un sumary
  output$summary_normalization <- renderPrint({#renderDataTable(
    summary(normalization_type())
    #options = list(paging = FALSE, searching = FALSE)
  })
  
  
  #************************************************
  #-------------> Reduccion de la dimencionalidad
  
  #Slider visualizacion grafico PCA
  output$slider_range_range_pca <- renderUI({
    twoSlider("attributes3","observation3",missingV(),"Attributes",strz)
  })
  
  
  #Obtengo la seleccion de atributos y observaciones para pca
  selectedDataPCA <- reactive({
    missingV()[input$observation3[1]:input$observation3[2], 
               input$attributes3[1]:input$attributes3[2]]
  })
  
  pca <- reactive({
    prcomp(selectedDataPCA(), center = TRUE, scale. = TRUE)
  })
  
  pcaGrafic <- function(){
    if(is.null(input$attributes3) || is.na(input$attributes3)){
      return()
    }
    withProgress({
      setProgress(message = "This may take a while...")
      elbowPlot(pca())
    })
  }
  
  #grafico de PCA
  output$pca <- renderPlot({
    tryCatch({
      closeAlert(session, "alertPCAID")
      pcaGrafic()
    }, error = function(e) {
      createAlert(session, "alertPCA", "alertPCAID", title = titleAlert,
                  content = paste("",e), 
                  style = "warning")
    })
    
  })
  
  #-------------->dowload image plot
  observe({
    output$downloadPlotpca <- downloadGeneral(input$radiopca, pcaGrafic())
  })
  
  #Informacion resumen de los pc's obtenidos
  output$summary_pcs <- renderPrint({
    summary(pca())
  })
  
  reduceDimensionality <- reactive({
    if(input$reduceDim){
      selectedDataPCA()
    }
    else
      missingV()
  })
  
  output$summary_reduceDimensionality <- renderPrint({
    summary(reduceDimensionality())
  })
  
  #------------SVD
  s <- reactive({
    dat <- as.matrix(missingV())
    svd(dat)
  })
  
  #grafico para SVD
  output$svd <- renderPlot({
    withProgress({
      setProgress(message = "This may take a while...")
      plot(cumsum(s()$d^2/sum(s()$d^2))) 
    })
  })
  
  output$s <- renderPrint({
    s()$d
  })
  
  #-------------------------------------------------------
  #-----------------------> Train <-----------------------
  
  #-----------------------> validation type
  #salida dinamica para solicitar unn archivo o un % para particionar
  output$testFile <- renderUI({
    if (is.null(input$select_validation))
      return()
    switch(input$select_validation,
           '2' = fileInput('fileTest', 'Test File',
                           accept=c('text/csv', 'text/comma-separated-values,text/plain','.csv')),
           '3' = numericInput("porcentTest", "Train size in %", 0.75, 
                              min = 0.0, max = 1, step = 0.02)
    )
  })
  
  #particion en porcentaje de train y test
  train_ind <- function(){
    dataset <- data.frame(reduceDimensionality())
    smp_size <- floor(input$porcentTest * nrow(dataset))
    set.seed(123)
    sample(seq_len(nrow(dataset)), size = smp_size)
    #     train <- dataset[train_ind, ]
    #     test <- dataset[-train_ind, ]
    #     return(list(train, test))
  }
  
  #-----------------------> lm
  #seleccion de la variable dependiente
  output$select_box_lm_y <- renderUI({
    numVariables <- dim(reduceDimensionality())[2]
    namesVariables <- names(reduceDimensionality())
    selectInput("lm_y", label = h4("Dependent variable"), 
                choices = namesVariables, selected = names(reduceDimensionality())[numVariables])
  })
  
  #seleccion de la variable independiente
  output$select_box_lm_x <- renderUI({
    selectInput("lm_x", label = h4("Independent variable"), 
                choices = names(reduceDimensionality()), multiple = TRUE)
  })
  
  #Aplicando el modelo lm
  fit <- reactive({
    if(is.null(input$lm_x)){
      (fmla <- as.formula(paste(paste(input$lm_y, " ~ "), ".")))
    }
    #input$lm_y
    else
      (fmla <- as.formula(paste(paste(input$lm_y, " ~ "), paste(input$lm_x, collapse= "+"))))
    
    if (is.null(input$select_validation)){return()}
    else{
      switch (input$select_validation,
              '1' = lm(fmla, data=reduceDimensionality()),
              '2' = lm(fmla, data=reduceDimensionality()),
              '3' = lm(Sepal.Length ~ ., data = data.frame( 
                reduceDimensionality()[train_ind(), ]))
      )
    }
    #lm(fmla, data=reduceDimensionality())
  })
  
  #Resultado obtenido tras aplicar el  modelo
  output$summary_lm <- renderPrint({
    withProgress({
      setProgress(message = "This may take a while...")
      summary(fit())
    })
  })
  
  validation <- reactive({
    if (is.null(input$select_validation))
      return()
    switch(input$select_validation,
           '1' = CVlm(reduceDimensionality(), fit(), m=10), # ten-fold cross validation
           '2' = if(!is.null(input$fileTest)){
                   predict(fit(), input$fileTest)
                 },
           '3' = predict(fit(), data.frame(reduceDimensionality()[-train_ind(), ]))
           
    )
  })
  
  #grafico para la validacionn cruzada
  observe({
    if(input$select_validation == '1'){
      output$crossPlot <- renderPlot({
        CVlm(reduceDimensionality(), fit(), m=10)
      })
    }
  })
  
  output$resultValidation <- renderPrint({
    validation()
  })
  
  #-----------------------> pls
  #seleccion de la variable de respuesta
  output$select_response <- renderUI({
    numVariables <- dim(reduceDimensionality())[2]
    #predictors <- reduceDimensionality()[, !names(reduceDimensionality()) %in% input$pls_response]
    namesVariables <- names(reduceDimensionality())
    selectInput("pls_response", label = h4("Response variable"), 
                choices = namesVariables, selected = names(reduceDimensionality())[numVariables])
  })
  
  #seleccion de la variables predictoras
  output$select_predictors <- renderUI({
    selectInput("pls_predictors", label = h4("Predictor variables"), 
                choices = names(reduceDimensionality()), multiple = TRUE)
  })
  
  #Aplicando el modelo pls
  fit2 <- reactive({
    tryCatch({
      closeAlert(session, "alertplsID")
      #Se aplica el modelo pls
      if(is.null(input$pls_predictors)){
        withProgress({
          setProgress(message = "This may take a while...")
          #sacar la variable de respuesta del data set
          predictors <- reduceDimensionality()[, !names(reduceDimensionality()) %in% input$pls_response]
          plsreg1(predictors, reduceDimensionality()[input$pls_response], crosval = TRUE)
        })
      }else{
        withProgress({
          setProgress(message = "This may take a while...")
          predictors <- reduceDimensionality()[, !names(reduceDimensionality()) %in% paste("", input$pls_response)]
          plsreg1(predictors[,input$pls_predictors], reduceDimensionality()[input$pls_response], crosval = TRUE)
        })
      }
    }, error = function(e) {
      createAlert(session, "alertpls", "alertplsID", title = titleAlert,
                  content = "Predictors must contain more than one column", style = "warning")
    })
  })
  
  #Resultado obtenido tras aplicar el  modelo
  output$summary_pls <- renderPrint({
    fit2()
  })
  
  #grafico correspondiente al modelo pls
  output$plotPLS <- renderPlot({
    plot(fit2())
  })
  
  #-------------------------------------------------------
  #-----------------------> outlier <-----------------------
  
  #-----------------------> Diagnostic Plots
  
  diagnostic <- reactive({
    diagnosticData(fit())
  })
  
  #funcion para el grafico residual vs fitted
  plotRF <- function(){
    myPalette <- c(input$col1, input$col2, input$col3)
    withProgress({
      setProgress(message = "This may take a while...")
      ResidualsFitted(diagnostic(), input$lm_y, colours = myPalette)
    })
  }
  
  #visualizacion del grafico residual vs fitted
  output$ResidualsFitted <- renderPlot({
    tryCatch({
      closeAlert(session, "alertRFID")
      plotRF()
    }, error = function(e) {
      createAlert(session, "alertRF", "alertRFID", title = titleAlert,
                  content = paste("",e), style = "warning")
    })
  })
  
  #muestra informacion de los puntos seleccionados
  output$ResidualsFitted_brushInfo <- renderPrint({
    brushedPoints(diagnostic(), input$ResidualsFitted_brush)[1:dim(reduceDimensionality())[2]]
  })
  
  #-------------->dowload image plot
  observe({
    output$downloadPlotRF <- downloadGeneral(input$radioRF, plotRF())
  })
  
  #funcion para el grafico Standarized Residuals v/s Fitted Values
  plotSF <- function(){
    myPalette <- c(input$col1, input$col2, input$col3)
    withProgress({
      setProgress(message = "This may take a while...")
      StResidualsFitted(diagnostic(), input$lm_y, colours = myPalette)
    })
  }
  
  #vista de grafico Standarized Residuals v/s Fitted Values
  output$StResidualsFitted <- renderPlot({
    tryCatch({
      closeAlert(session, "alertSFID")
      plotSF()
    }, error = function(e) {
      createAlert(session, "alertSF", "alertSFID", title = titleAlert,
                  content = paste("",e), style = "warning")
    })
  })
  
  #muestra informacion de los puntos seleccionados
  output$StResidualsFitted_brushInfo <- renderPrint({
    brushedPoints(diagnostic(), input$StResidualsFitted_brush)[1:dim(reduceDimensionality())[2]]
  })
  
  #-------------->dowload image plot
  observe({
    output$downloadPlotSF <- downloadGeneral(input$radioSF, plotSF())
  })
  
  #funcion para el grafico normal Q-Q
  plotQQ <- function(){
    withProgress({
      setProgress(message = "This may take a while...")
      NormalQQ(diagnostic(), input$lm_y)
    })
  }
  
  #visualizacion del grafico normal Q-Q
  output$NormalQQ <- renderPlot({
    tryCatch({
      closeAlert(session, "alertQQID")
      plotQQ()
    }, error = function(e) {
      createAlert(session, "alertQQ", "alertQQID", title = titleAlert,
                  content = paste("",e), style = "warning")
    })
  })
  
  #   #muestra informacion de los puntos seleccionados
  #   output$NormalQQ_brushInfo <- renderPrint({
  #     brushedPoints(diagnostic(), input$NormalQQ_brush)[1:dim(reduceDimensionality())[2]]
  #   })
  
  #-------------->dowload image plot
  observe({
    output$downloadPlotQQ <- downloadGeneral(input$radioQQ, plotQQ())
  })
  
  #funcion para el grafico residual vs leverage
  plotRL <- function(){
    myPalette <- c(input$col1, input$col2, input$col3)
    withProgress({
      setProgress(message = "This may take a while...")
      StResidualsLeverange(diagnostic(), input$lm_y, colours = myPalette)
    })
  }
  
  #visualizacion del grafico residual vs leverage
  output$StResidualsLeverange <- renderPlot({
    tryCatch({
      closeAlert(session, "alertRLID")
      plotRL()
    }, error = function(e) {
      createAlert(session, "alertRL", "alertRLID", title = titleAlert,
                  content = paste("",e), style = "warning")
    })
  })
  
  #muestra informacion de los puntos seleccionados
  output$StResidualsLeverange_brushInfo <- renderPrint({
    brushedPoints(diagnostic(), input$StResidualsLeverange_brush)[1:dim(reduceDimensionality())[2]]
  })
  
  #-------------->dowload image plot
  observe({
    output$downloadPlotRL <- downloadGeneral(input$radioRL, plotRL())
  })
}