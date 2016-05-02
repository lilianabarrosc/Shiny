source('funciones/LOF.R')
source('funciones/dataBase.r')

#--------------------Servidor-------------------

#--------------> conexion con la base de datos
# drv <- dbDriver("PostgreSQL")
# con <- conexionbd(drv)

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
  USER <- reactiveValues(Logged = FALSE, role=NULL)
  #se valida el nombre del usuario con la contraseña ingresada
  #observe({
  observeEvent(input$Login, {
    #obtencion de user y password desde la bd
    if (USER$Logged == FALSE) {
      #       if (!is.null(input$Login)) {
      #         if (input$Login > 0 ) {
      if (input$userName != "" & input$passwd != ""){ 
        #se obtienen los valores ingresados
        Username <- "admin"#isolate(input$userName)
        Password <- isolate(input$passwd)
        
        sql <- paste("select password from user_guinia where user_name ='",
                     Username, "'")
        tryCatch({
          rs <- dbSendQuery(con, sql)
          #obtengo el valor de la bd
          my_password <- fetch(rs,n=-1) #la funcion fetch() devuelve un frame
          #print(paste(">>>",dbGetStatement(rs)))
          if (nrow(my_password) > 0){ #length(Id.username) > 0 & length(Id.password) > 0) {
            #Se comparan los valores obtenidos con los registros
            if (as.character(my_password) == Password){ #Id.username == Id.password) { #si ambas variables son true el usuario es valido
              closeAlert(session, "alertLoginID")
              USER$Logged <- TRUE
              USER$role=get_role(Username)
              desconexionbd(con, drv)
            }
            else{ #la contraseña no coinside
              createAlert(session, "alertLogin", "alertLoginID", title = titleAlert,
                          content = "Username and password do not match.", 
                          style = "warning", append = FALSE)
            }
          }
        },error = function(e) {
          print(e)
          createAlert(session, "alertLogin", "alertLoginID", title = titleAlert,
                      content = "Username does not exist", 
                      style = "warning", append = FALSE)
        })
      }else { #existen campos vacios
        createAlert(session, "alertLogin", "alertLoginID", title = titleAlert,
                    content = "There are empty fields.", 
                    style = "warning", append = FALSE)
      }
      #         }
      #       }
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
          #registro en la bd
          inputs <- paste(isolate(input$newUserName),isolate(input$name),isolate(input$lastName),
                          isolate(input$email),isolate(input$newPasswd), sep = "','")
          sql <- paste("insert into user_guinia (user_name,name,last_name,email,password) values ('",
                       inputs, "')")
          tryCatch({
            rs <- dbSendQuery(con, sql)
            session$onSessionEnded(desconexionbd(con, drv))
            USER$Logged <- TRUE
          },error = function(e) {
            createAlert(session, "alertRegister", "alertRegisterID", title = titleAlert,
                        content = "Username already exists.", 
                        style = "warning", append = FALSE)
          })
          #rs <- dbSendQuery(con, sql) #dbSendQuery(con, "insert into user_guinia (user_name,password) values ('user','1234')") 
        }
      }
    }
  })
  
  #-------------------------------------------------------
  #-----------------------> data <-----------------------
  #Variable contiene el data set atualizado
  DATA_SET <- reactiveValues(name = NULL, data = NULL)
  
  #----------> data set nuevos que se ingresan mediante directorio o URI
  output$data_extern <- renderUI({
    if (is.null(input$select_file))
      return()
    switch(input$select_file,
           #Cargar archivo desde el equipo o mediante una URL
           '4' = fileInput('file1', 'Choose CSV File',
                           accept=c('text/csv', 'text/comma-separated-values,text/plain', 
                                    '.csv')),
           '5' = urls()#funcion contenida en data.r
    )
  })
  
  #Seleccion de data set a utilizar
  file <- reactive({
    inFile <- input$file1
    if (input$select_file == '4' && is.null(inFile)){
      return()
    }
    if(input$select_file == '5' && is.null(input$url)){return()}
    
    switch(input$select_file,
           '1'= iris,
           '2'= airquality,
           '3'= sleep,
           '4'= tryCatch({
                   closeAlert(session, "alertUploadID") 
                   read.csv(inFile$datapath)
                 }, error = function(e) {
                   createAlert(session, "alertUpload", "alertUploadID", title = titleAlert,
                               content = paste("",e), style = "warning")
                 })
    )
  })
  
  #Accion a realizar tras presionar el boton upload de la opcion URL (lee el archivo)
  observeEvent(input$upload, { #if(input$upload){
    tryCatch({
      closeAlert(session, "alertURLID")
      withProgress({
        setProgress(message = "This may take a while...")
        DATA_SET$data <- read.csv(input$url, sep = input$sep, dec = input$dec)
      })
    }, error = function(e) {
      createAlert(session, "alertRUL", "alertURLID", title = titleAlert,
                  content = paste("",e), style = "warning")
    })
  })
  
  observe({ #Se asigna el data set seleccionado a la varible global
    if (input$select_file==4 && is.null(input$file1) && is.null(input$upload))
      return()
    else if(input$select_file==5 && is.null(input$upload))
      return()
    #obtener el nombre del data set
    switch(input$select_file,
           '1'= DATA_SET$name <-"iris",
           '2'= DATA_SET$name <-"airquality",
           '3'= DATA_SET$name <-"sleep",
           '4'= DATA_SET$name <- input$file1$name,
           '5'= observe({
             splitURL <- strsplit(input$url, "/")
             DATA_SET$name <- splitURL[[1]][length(splitURL[[1]])]
           })
    )
    if(input$select_file!=5){
      DATA_SET$data <- file()
    }
  })
  
  #muestro un resumen del data set seleccionado
  output$str_data <- renderPrint({
    if (input$select_file==4 && is.null(input$file1))
      return()
    str(DATA_SET$data)
  })
  
  #muestro un sumary del data set seleccionado
  output$summary_data <- renderPrint({
    if (input$select_file==4 && is.null(input$file1))
      return()
    summary(DATA_SET$data)
  })
  
  #----------> dimensionalidad del archivo
  #Con dim puedo saber la cantidad de atributos y observaciones que posee el archivo,
  #en dim(data)[1] se pueden encontrar la cantidad de observaciones y en dim(data)[2] la 
  
  #*************************************************
  #----------> Sacar columnas con valores nominales
  observe({
    if(is.null(file()))
      return()
    aux <- data.frame(file())
    nums <- sapply(aux, is.numeric)
    DATA_SET$data <- aux[ , nums]
  }) 
  
  #***************************************
  #-----------------------> visualization
  
  #----------> Graficos de visualizacion
  
  #Actualizo el maximo del slider con el valor del tamaÃ±o del archivo seleccionado
  output$slider_Scatterplot <- renderUI({
    treeSlider("x_scatter", "y_scatter", "z_scatter", DATA_SET$data, strx, stry, strz)
  })
  
  #seleccion de atributos y observaciones del data set
  dat_Scatterplot <- reactive({
    DATA_SET$data[input$z_scatter[1]:input$z_scatter[2],input$x_scatter[1]:input$x_scatter[2]]
  })
  
  scatterPlot <- reactive({ #funcion que genera el scatterPlot
    if(is.null(input$x_scatter) || is.na(input$x_scatter)){
      return()
    }
    #paleta de colores para el gráfico
    myPalette <- c(input$col1, input$col2, input$col3)
    withProgress({
      setProgress(message = "This may take a while...")
      ScatterplotMatrix(dat_Scatterplot(), c(input$x_scatter[1]:input$x_scatter[2]), DATA_SET$data[,input$y_scatter], 
                        names(DATA_SET$data)[[input$y_scatter]], colours = myPalette)
    })
  })
  
  #Grafico correspondiente a scatterPlot 
  output$scatter_plot <- renderPlot({
    tryCatch({
      closeAlert(session, "alertScatterID")
      scatterPlot()
    }, error = function(e) {
      createAlert(session, "alertScatter", "alertScatterID", title = titleAlert,
                  content = paste("",e), 
                  style = "warning")
    })
    
  })
  
  #-------------->dowload image plot
  observe({
    output$download_Scatterplot <- downloadGeneral(input$radio_Scatterplot, output$scatter_plot, DATA_SET$name)
  })
  
  #Slider visualizacion grafico parallel x e y
  output$slider_parallelPlot <- renderUI({
    box(
      width = 6, status = "success",
      h4("Range"),
      sliderInput("x_parallel", label = strx, min = 1, 
                  max = dim(DATA_SET$data)[2], value = c(1, dim(DATA_SET$data)[2])),
      sliderInput("y_parallel", label = stry, min = 1, 
                  max = dim(file())[2], value = 2)
    )
  })
  
  #Slider visualizacion grafico parallel observations y alfa
  output$slider_parallelPlot2 <- renderUI({
    box(
      width = 6, status = "success",
      h4("Range"),
      sliderInput("z_parallel", label = strz, min = 1, 
                  max = dim(DATA_SET$data)[1], value = c(1, dim(DATA_SET$data)[1])),
      sliderInput("lineSize", label = "Line Size", min = 1, 
                  max = 5, value = 2),
      sliderInput("alphaLine", label = "Alpha Line", min = 0.01, 
                  max = 0.99, value = 0.11)
    )
  })
  
  
  #seleccion de atributos y observaciones del data set
  data_Parallelx <- reactive({
    DATA_SET$data[input$z_parallel[1]:input$z_parallel[2],input$x_parallel[1]:input$x_parallel[2]]
  })
  
  data_Parallely <- reactive({
    file()[,input$y_parallel]
  })
  
  #funcion que genera el grafico
  parallelplot <- reactive({
    if(is.null(input$x_parallel) || is.na(input$x_parallel)){
      return()
    }
    myPalette <- c(input$col1, input$col2, input$col3)
    # A ParallelPlot of all rows and all columns
    withProgress({
      setProgress(message = "This may take a while...")
      ParallelPlot(data_Parallelx(), seq(1,nrow(data_Parallelx()),1), seq(1,ncol(data_Parallelx()),1), data_Parallely(), 
                   names(file())[[input$y_parallel]], 1, input$alphaLine, TRUE, colours = myPalette)
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
    output$download_parallelplot <- downloadGeneral(input$radio_parallelplot, parallelplot(), DATA_SET$name)
  })
  
  #-------------------------------------------------------
  #-----------------------> Preprocessing <-----------------------
  
  #*********************************************
  #---------------> Graficos correspondientes a missing values
  observe({
    if(input$deleteMS)
      DATA_SET$data <- na.omit(DATA_SET$data)
  })
  
  output$sumMV <- renderPrint({
    paste("The data set contains",sum(is.na(DATA_SET$data)), "missing values.")
  })
  
  #Slider visualizacion grafico de missing values
  output$slider_missingValues <- renderUI({
    twoSlider("attributes","observation",DATA_SET$data,"Attributes",strz)
  })
  
  #Obtengo la seleccion de atributos y observaciones para la Opcion 1
  data_boxPlot <- reactive({
    DATA_SET$data[input$observation[1]:input$observation[2], 
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
      matrixplot(data_boxPlot())
    })
  }
  
  #Visualizacion del grafico
  output$boxplot <- renderPlot({
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
    output$download_boxplot <- downloadGeneral(input$radio_boxplot, boxPlotfunction(), DATA_SET$name)
  })
  
  #Slider visualizacion grafico de missing VIM option2
  output$slider_histogramPlot <- renderUI({
    twoSlider("attributes_histogram","observation_histogram",DATA_SET$data,"Attributes",strz)
  })
  
  #Obtengo la seleccion de atributos y observaciones para la Opcion 2
  data_histogramPlot <- reactive({
    DATA_SET$data[input$observation_histogram[1]:input$observation_histogram[2], 
               input$attributes_histogram[1]:input$attributes_histogram[2]]
  })
  
  #Opcion 2 (libreria VIM)
  output$histogramPlot <- renderPlot({
    if(is.null(input$attributes_histogram) || is.na(input$attributes_histogram)){
      return()
    }
    tryCatch({
      closeAlert(session, "alertmissing2ID")
      withProgress({
        setProgress(message = "This may take a while...")
        aggr(data_histogramPlot(), col=c('red','dark grey'), numbers=TRUE, 
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
  output$slider_missingScatter <- renderUI({
    treeSlider("x_missingScatter", "y_missingScatter", "z_missingScatter", DATA_SET$data, strx, stry, strz)
  })
  
  #Obtengo la seleccion de atributos a comparar para la Opcion 3
  data_missingScatter <- reactive({
    DATA_SET$data[input$z_missingScatter[1]:input$z_missingScatter[2],
               input$x_missingScatter[1]:input$x_missingScatter[2]]
  })
  
  #Option 3 (matricial)
  output$missingScatterPlot <- renderPlot({
    if(is.null(input$x_missingScatter) || is.na(input$x_missingScatter)){
      return()
    }
    tryCatch({
      closeAlert(session, "alertMissing3ID")
      scattmatrixMiss(data_missingScatter(), interactive = F, highlight = c(names(DATA_SET$data)[[input$y_missingScatter]]))
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
    if(anyNA(DATA_SET$data)){
      createAlert(session, "alertNoise", "alertNoiseID", title = titleAlert,
                  content = "Data set have missing values", style = "warning")
    }else{
      if(input$rnoise){
        diffValues <- calculateDiff(DATA_SET$data)
        columnsNoise <- getColumnsNoise(diffValues, input$limitNoise)
        #  columnsNoise <- as.data.frame(columnsNoise[,1] + ncol(missingV()))
        as.data.frame(DATA_SET$data[,-columnsNoise[,1]])
      }else {DATA_SET$data}
    }
  })
  
  #Slider visualizacion grafico parallel x e y
  output$slider_nremoval <- renderUI({
    box(
      width = 6, status = "success",
      h4("Range"),
      sliderInput("attributes_nremoval", label = strx, min = 1, 
                  max = dim(noiseR())[2], value = c(1, dim(noiseR())[2])),
      sliderInput("observation_nremoval", label = stry, min = 1, 
                  max = dim(file())[2], value = 2)
    )
  })
  
  #Slider visualizacion grafico parallel observations y alfa
  output$slider_nremoval2 <- renderUI({
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
             input$attributes_nremoval[1]:input$attributes_nremoval[2]] 
  })
  
  #seleccion de la variable de respuesta
  datNremovaly <- reactive({
    file()[,input$observation_nremoval]
  })
  
  #grafico de ruido
  output$nremoval <- renderPlot({
    if(is.null(input$attributes_nremoval) || is.na(input$attributes_nremoval)){
      return()
    }
    myPalette <- c(input$col1, input$col2, input$col3)
    # A ParallelPlot of all rows and all columns
    withProgress({
      setProgress(message = "This may take a while...")
      ParallelPlot(datNremovalx(), seq(1,nrow(datNremovalx()),1), seq(1,ncol(datNremovalx()),1), datNremovaly(), 
                   names(file())[[input$observation_nremoval]], 1, input$alphaLineNremoval, TRUE, colours = myPalette)
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
    lof(DATA_SET$data, k= c(5:10))
  })
  
  #Slider visualizacion grafico de missing VIM option2
  output$sliderLOF <- renderUI({
    minimo <- round(min(outlier.scores()), digits=2)
    maximo <- round(max(outlier.scores()), digits=2)
    sliderInput("thresholdt", "Threshold", min = minimo,
                max = maximo, value = 1.25, step= 0.01)
  })
  
  #llamado a la funcion lof, la cual devuelve una lista
  res_lof <- reactive({
    if(is.na(outlier.scores()) && is.null(outlier.scores())){return}
    else{
      if(!is.null(input$thresholdt)){
        LOFCraft(data = DATA_SET$data, threshold = input$thresholdt, data.frame(outlier.scores())) ##calling LOF
      } else {
        LOFCraft(data = DATA_SET$data, data.frame(outlier.scores())) ##calling LOF
      }
    }
    
  })
  
  ## scores for the without outliers data
  withoutOutliers.scores <- reactive({
    data.frame(res_lof()[1]) ## scores of data without outliers
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
    as.numeric(res_lof()[3])
  })
  
  #Posicion de los outlier en el archivo
  output$posOutliers <- renderPrint({
    data.frame(res_lof()[4])  ## the positions of the outliers in the original data and theirs respective scores
  })
  
  #without outliers data
  output$strWithoutOutliers <- renderPrint({
    dataWithoutOutliers<-data.frame(res_lof()[2])  ##the data without outliers
    str(dataWithoutOutliers)
  })
  
  
  #-------------------------------------------------------
  #-----------------------> Transformation <-----------------------
  
  #************************************************
  #-------------> Normalization
  #salida dinamica de rango para normalizacion
  output$range_normalization <- renderUI({
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
                                      choices = type_normalization() 
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
             normalizeData(DATA_SET$data)
           }),
           '2'=  withProgress({
             setProgress(message = "This may take a while...")
             normalizeData(DATA_SET$data, input$min, input$max)
           }),
           '3'=  withProgress({
             setProgress(message = "This may take a while...") 
             data.Normalization(DATA_SET$data,type=input$type_normalization ,normalization= input$type)
           })
    )
  })
  
  #muestro los primeros 10 atributos del data set original
  output$original_data <- renderPrint({#renderDataTable(
    DATA_SET$data[1:10,]
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
  output$slider_pca <- renderUI({
    twoSlider("attributes_pca","observation_pca",DATA_SET$data,"Attributes",strz)
  })
  
  
  #Obtengo la seleccion de atributos y observaciones para pca
  data_pca <- reactive({
    DATA_SET$data[input$observation_pca[1]:input$observation_pca[2], 
               input$attributes_pca[1]:input$attributes_pca[2]]
  })
  
  pca <- reactive({
    prcomp(data_pca(), center = TRUE, scale. = TRUE)
  })
  
  pcaGrafic <- function(){
    if(is.null(input$attributes_pca) || is.na(input$attributes_pca)){
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
  
    reduceDimensionality <- reactive({
      if(input$reduceDim){
        data_pca()
        #print("hola if")
      }
      else{
        #print("hola else")
        data.frame(DATA_SET$data)
      }
    })
#   
  #data luego de aplicar un metodo de reduccion como pls
  observeEvent(input$reduceDim, {
    if(is.null(data_pca())){return()}
    DATA_SET$data <- data_pca()
    })
  
  #-------------->dowload image plot
  observe({
    output$download_pcaPlot <- downloadGeneral(input$radio_pca, pcaGrafic(), DATA_SET$name)
  })
  
  #Informacion resumen de los pc's obtenidos
  output$summary_pcs <- renderPrint({
    summary(pca())
  })
  
  output$summary_reduceDimensionality <- renderPrint({
    summary(DATA_SET$data)
  })
  
  #------------SVD
  s <- reactive({
    dat <- as.matrix(DATA_SET$data)
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
  #-----------------------> Regresion <-----------------------
  
  #-----------------------> validation type
  
  #particion en porcentaje de train y test
  train_lm <- reactive({
    if(is.null(input$porcentTest_lm)){ return() }
     dataPartition(DATA_SET$data, input$porcentTest_lm)
  })
  
  #-----------------------> lm
  #seleccion de la variable dependiente o de respuesta
  output$select_linearModel <- renderUI({
    select_model("lm_response", "lm_predictors", data.frame(DATA_SET$data))  
  })
  
  #Aplicando el modelo lm
  model_lm <- reactive({
    if(is.null(input$lm_response)){return()}
    if(is.null(input$lm_predictors)){
      (fmla <- as.formula(paste(paste(input$lm_response, " ~ "), ".")))
    }
    #input$lm_y
    else{
      (fmla <- as.formula(paste(paste(input$lm_response, " ~ "), paste(input$lm_predictors, collapse= "+"))))
    }
    
    if (is.null(input$validationType_lm)){return()}
    else{
      switch (input$validationType_lm,
              '1' = lm(fmla, data=DATA_SET$data),
              '2' = lm(fmla, data=DATA_SET$data),
              '3' = lm(fmla, data = data.frame( 
                        DATA_SET$data[train_lm(), ]))
     )
    }
    #lm(fmla, data=DATA_SET$data) 
  })
  
  #Resultado obtenido tras aplicar el  modelo
  output$summary_lm <- renderPrint({
   if(is.null(model_lm())) return()
     withProgress({
       setProgress(message = "This may take a while...")
      summary(model_lm())
   })
  })
  
  #******* validacion del modelo lm
  #obtengo los predictores del modelo
  predictores_lm <-  reactive({
    data.frame(predictors(DATA_SET$data, input$lm_predictors, input$lm_response))
  })
  
  #coeficientes del modelo
  thetaPredict_lm <- function(fit,x){ cbind(1,x)%*%fit$coefficients }
  
  #tipo de validacion para el modelo
  validation_lm <- reactive({
    tryCatch({
      if (is.null(input$validationType_lm))
        return()
      if(input$validationType_lm == '2' && is.null(input$fileTest_lm))
        return()
      if(is.null(model_lm()))
        return()
      closeAlert(session,"alertValidationID")
      switch(input$validationType_lm,
             '1' = crossValidation(model_lm(), thetaPredict_lm, DATA_SET$data[,input$lm_response], predictores_lm()), # ten-fold cross validation funcion en regresion.r
             '2' = { inFile <- input$fileTest_lm
                      Prediction <-predict(model_lm(), read.csv(inFile$datapath))
                      response_variable <- DATA_SET$data[,input$lm_response]
                      data.frame(cbind(Prediction, response_variable))
                    },
             '3' = { Prediction <- predict(model_lm(), data.frame(DATA_SET$data[-train_lm(), ]))
                     response_variable <- DATA_SET$data[,input$lm_response]
                    data.frame(cbind(Prediction, response_variable))
                   }
             
      )
    }, error = function(e) {
      createAlert(session, "alertValidation", "alertValidationID", title = titleAlert,
                  content = paste("",e), style = "warning")
    })
  })
      
  output$resultValidation_lm <- renderPrint({
    validation_lm()
  })
  
  #***** Colinearity Test lm
  
  #resultado de aplicar el test de colinealidad
  output$colinearity_result <- renderPrint({
    vif(model_lm())
  })
  
  #tolerancia del test de colinealidad
  output$colinearity_tolerance <- renderPrint({
    1/vif(model_lm())
  })
  
  #promedio del test de colinealidad
  output$colinearity_mean <- renderPrint({
    mean(vif(model_lm()))
  })
  
  #-----------------------> pls
  #seleccion de la variable de respuesta
  output$select_pls <- renderUI({
    select_model("pls_response", "pls_predictors", DATA_SET$data)  
  })
  
  #particion en porcentaje de train y test
  train_pls <- reactive({
    if(is.null(input$porcentTest_lm)){ return() }
    dataPartition(DATA_SET$data, input$porcentTest_pls)
  })
  
  #numero de componentes para el modelo
  output$componentes_pls <- renderUI({
    numericInput("comp_pls", h4("PLS comp"), value = 2, min = 2, max = ncol(DATA_SET$data) )
  })
  
  #Aplicando el modelo pls
  model_pls <- reactive({
    tryCatch({
      if(anyNA(DATA_SET$data)){
        createAlert(session, "alertpls", "alertplsID", title = titleAlert,
                    content = "Data set have missing values", style = "warning")
      }else{
        closeAlert(session, "alertplsID")
        val <- TRUE #valor por defecto para la validacion cruzada
        if(input$crosval == "FALSE"){ val <- FALSE }
        #Se aplica el modelo pls
        if(is.null(input$pls_predictors)){
          withProgress({
            setProgress(message = "This may take a while...")
            #sacar la variable de respuesta del data set
            predictors <- DATA_SET$data[, !names(DATA_SET$data) %in% input$pls_response]
            plsreg1(predictors, DATA_SET$data[,input$pls_response], 
                    crosval = val, comps = input$comp_pls)
          })
        }else{
          withProgress({
            setProgress(message = "This may take a while...")
            predictors <- DATA_SET$data[, !names(DATA_SET$data) %in% paste("", input$pls_response)]
            plsreg1(predictors[,input$pls_predictors], DATA_SET$data[,input$pls_response], 
                    crosval = val, comps = input$comp_pls)
          })
        }
      }
    }, error = function(e) {
      createAlert(session, "alertpls", "alertplsID", title = titleAlert,
                  content = paste("",e), style = "warning")
    })
  })
  
  #Resultado obtenido tras aplicar el  modelo
  output$result_pls <- renderPrint({
    if(is.null(model_pls()))
      return()
    list(reg.coefs = model_pls()$reg.coefs, 
         R2 = model_pls()$R2,
         cor.xyt = model_pls()$cor.xyt,
         R2Xy = model_pls()$R2Xy)
    #input$pls_predictors
  })
  
  #Estadisticos RME, R2, IA
  statistical_resultpls <- function(){
    if(is.null(model_pls()))
      return()
    
    overfittedRMSE <- rmse(model_pls()$y.pred, DATA_SET$data[,input$pls_response]) #overfitted RMSE
    overfittedR2 <- cor(model_pls()$y.pred, DATA_SET$data[,input$pls_response])^2 #overfitted R2
    overfittedIA <- d(model_pls()$y.pred, DATA_SET$data[,input$pls_response]) #overfitted IA
    
    Statistical <- as.data.frame(array(0, dim=c(1,4)))
    names(Statistical) <- c("Name", "RMSE", "R2", "IA")
    Statistical[1,2] <- overfittedRMSE
    Statistical[1,3] <- overfittedR2
    Statistical[1,4] <- overfittedIA
    return(Statistical) 
  }
  
  #Muestra los estadisticos de pls
  output$statistical_pls <- renderPrint({
    statistical_resultpls()
  })
  
  #****** validacion pls
  #obtengo los predictores del modelo
  predictores_pls <-  reactive({
    data.frame(predictors(DATA_SET$data, input$pls_predictors, input$pls_response))
  })
  
  #coeficientes del modelo
  thetaPredict_pls <- function(fit,x){ cbind(1,x)%*%fit$reg.coefs }
  
  #tipo de validacion para el modelo
  validation_pls <- reactive({
    tryCatch({
      if (is.null(input$validationType_pls))
        return()
      if(input$validationType_pls == '2' && is.null(input$fileTest_pls))
        return()
      if(is.null(model_pls()))
        return()
      closeAlert(session,"alertValidationID")
      switch(input$validationType_pls,
             '1' = crossValidation(model_pls(), thetaPredict_pls, DATA_SET$data[,input$pls_response], predictores_pls()), # ten-fold cross validation funcion en regresion.r
             '2' = model_pls()$Q2,
             '3' = { Prediction <- model_pls()$y.pred
                     response_variable <- DATA_SET$data[,input$pls_response]
                     data.frame(cbind(Prediction, response_variable))
                   }
             
      )
    }, error = function(e) {
      createAlert(session, "alertValidation", "alertValidationID", title = titleAlert,
                  content = paste("",e), style = "warning")
    })
  })
  #resultado de la validacion
  output$resultValidation_pls <- renderPrint({
    #model_pls()$Q2
    validation_pls()
  })
  
  #***** test de colinealidad pls
  #grafico correspondiente al modelo pls
  output$plotPLS <- renderPlot({
    if(is.null(model_pls()))
      return()
    plot(model_pls())
  })
  
  #-----------------------> ridge
  #seleccion de la variable dependiente
  output$select_ridge <- renderUI({
    select_model("ridge_response", "ridge_predictors", DATA_SET$data)
  })
  
  #obtengo los predictores del modelo
  predictores_ridge <-  reactive({
    data.frame(predictors(DATA_SET$data, input$ridge_predictors, input$ridge_response))
  })
  
  #Aplicando el modelo de validacion cruzada ridge para determinar el lambda minimo
  model_cvridge <- reactive({
    tryCatch({
      if(anyNA(DATA_SET$data)){
        createAlert(session, "alertRidge", "alertRidgeID", title = titleAlert,
                    content = "Data set have missing values", style = "warning")
      }else{
        grid <- 10^seq(10, -2, length = 100)
        closeAlert(session, "alertCVRidgeID")
        #Se aplica el modelo ridge
        withProgress({
          setProgress(message = "This may take a while...")
          #sacar la variable de respuesta del data set
          cv.glmnet(as.matrix(predictores_ridge()), DATA_SET$data[,input$ridge_response], 
                    alpha = 0, lambda = grid)
        })
      }
    }, error = function(e) {
      createAlert(session, "alertCVRidge", "alertCVRidgeID", title = titleAlert,
                  content = paste("",e), style = "warning")
    })
  })
  
  #aplicando rigde para el lambda min obtenido en model_cvridge()
  model_ridge <- reactive({
    tryCatch({
      if(anyNA(DATA_SET$data)){
        createAlert(session, "alertRidge", "alertRidgeID", title = titleAlert,
                    content = "Data set have missing values", style = "warning")
      }else{
        closeAlert(session, "alertRidgeID")
        #Se aplica el modelo ridge con una formula como el lm
        if(is.null(input$ridge_predictors)){
          (fmla <- as.formula(paste(paste(input$ridge_response, " ~ "), ".")))
        }
        #input$lm_y
        else{
          (fmla <- as.formula(paste(paste(input$ridge_response, " ~ "), paste(input$ridge_predictors, collapse= "+"))))
        }
        lm.ridge(fmla, data = DATA_SET$data, lambda = model_cvridge()$lambda.min)
      }
    }, error = function(e) {
      createAlert(session, "alertRidge", "alertRidgeID", title = titleAlert,
                  content = paste("",e), style = "warning")
    })
  })
  
#   #Resultado obtenido tras aplicar el  modelo cv
#   output$result_cvridge <- renderPrint({
#     if(is.null(model_cvridge()))
#       return()
#     summary(model_cvridge())
#   })
  
  #Resultado obtenido tras aplicar el  modelo
  output$result_ridge <- renderPrint({
    if(is.null(model_ridge()))
      return()
    summary(model_ridge())
  })
  
  #grafico de lamda
  output$plot_ridge <- renderPlot({
    if(is.null(model_cvridge()))
      return()
    plot(model_cvridge())
  })
  
  #******* validacion
  #coeficientes del modelo
  thetaPredict_ridge <- function(fit,x){ cbind(1,x)%*%coef(fit) }
  
  #tipo de validacion para el modelo
  validation_ridge <- reactive({
    tryCatch({
      if (is.null(input$validationType_ridge))
        return()
      if(input$validationType_ridge == '2' && is.null(input$fileTest_ridge))
        return()
      if(is.null(model_ridge()))
        return()
      closeAlert(session,"alertValidationID")
      switch(input$validationType_ridge,
             '1' = crossValidation(model_ridge(), thetaPredict_ridge, DATA_SET$data[,input$ridge_response], predictores_ridge()) # ten-fold cross validation funcion en regresion.r
             
      )
    }, error = function(e) {
      createAlert(session, "alertValidation", "alertValidationID", title = titleAlert,
                  content = paste("",e), style = "warning")
    })
  })
  #resultado de la validacion
  output$resultValidation_ridge <- renderPrint({
    validation_ridge()
  })
  
  #-----------------------> rglm
  #seleccion de la variable dependiente
  output$select_lm <- renderUI({
    select_model("rglm_y", "rglm_x", DATA_SET$data)
  })
  
  
  #-------------------------------------------------------
  #-----------------------> Linear Model Evaluation <-----------------------
  
  #-----------------------> Diagnostic Plots
  
  diagnostic <- reactive({
    if(is.null(model_lm)){return()}
    diagnosticData(model_lm())
  })
  
  #funcion para el grafico residual vs fitted
  plotRF <- function(){
    if(is.null(model_lm)){return()}
    myPalette <- c(input$col1, input$col2, input$col3)
    withProgress({
      setProgress(message = "This may take a while...")
      ResidualsFitted(diagnostic(), input$lm_response, colours = myPalette)
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
    if(is.null(model_lm)){return()}
    brushedPoints(diagnostic(), input$ResidualsFitted_brush)[1:dim(DATA_SET$data)[2]]
  })
  
  #-------------->dowload image plot
  observe({
    output$downloadPlotRF <- downloadGeneral(input$radioRF, plotRF(), DATA_SET$name)
  })
  
  #funcion para el grafico Standarized Residuals v/s Fitted Values
  plotSF <- function(){
    if(is.null(model_lm)){return()}
    myPalette <- c(input$col1, input$col2, input$col3)
    withProgress({
      setProgress(message = "This may take a while...")
      StResidualsFitted(diagnostic(), input$lm_response, colours = myPalette)
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
    if(is.null(model_lm)){return()}
    brushedPoints(diagnostic(), input$StResidualsFitted_brush)[1:dim(DATA_SET$data)[2]]
  })
  
  #-------------->dowload image plot
  observe({
    output$downloadPlotSF <- downloadGeneral(input$radioSF, plotSF(), DATA_SET$name)
  })
  
  #funcion para el grafico normal Q-Q
  plotQQ <- function(){
    if(is.null(model_lm)){return()}
    withProgress({
      setProgress(message = "This may take a while...")
      NormalQQ(diagnostic(), input$lm_response)
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
  #     brushedPoints(diagnostic(), input$NormalQQ_brush)[1:dim(DATA_SET$data)[2]]
  #   })
  
  #-------------->dowload image plot
  observe({
    output$downloadPlotQQ <- downloadGeneral(input$radioQQ, plotQQ(), DATA_SET$name)
  })
  
  #funcion para el grafico residual vs leverage
  plotRL <- function(){
    if(is.null(model_lm)){return()}
    myPalette <- c(input$col1, input$col2, input$col3)
    withProgress({
      setProgress(message = "This may take a while...")
      StResidualsLeverange(diagnostic(), input$lm_response, colours = myPalette)
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
    brushedPoints(diagnostic(), input$StResidualsLeverange_brush)[1:dim(DATA_SET$data)[2]]
  })
  
  #-------------->dowload image plot
  observe({
    output$downloadPlotRL <- downloadGeneral(input$radioRL, plotRL(), DATA_SET$name)
  })
  
}