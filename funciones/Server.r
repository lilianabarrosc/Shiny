source('funciones/LOF.R')
source('funciones/dataBase.r')

#--------------------Servidor-------------------

#--------------> conexion con la base de datos
# drv <- dbDriver("PostgreSQL")
# con <- conexionbd(drv)

# By default, the file size limit is 5MB. It can be changed by
# setting this option. Here we'll raise limit to 9MB.
options(shiny.maxRequestSize = 9*1024^2)

server <- function(input, output, session) {
  #--------------> variables globales
  strx <- "Dependent variables"
  stry <- "Response variable"
  strz <- "Observations"
  titleAlert <- "Oops"
  titleAlertInfo <- "Congratulations"
  outputDir <- "file" #directorio donde se almacenan los data set
  
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
  
  # #inicio la variable user
  # USER <- reactiveValues(Logged = FALSE, role=NULL)
  # #se valida el nombre del usuario con la contraseña ingresada
  # #observe({
  # observeEvent(input$Login, {
  #   #obtencion de user y password desde la bd
  #   if (USER$Logged == FALSE) {
  #     #       if (!is.null(input$Login)) {
  #     #         if (input$Login > 0 ) {
  #     if (input$userName != "" & input$passwd != ""){ 
  #       #se obtienen los valores ingresados
  #       Username <- "admin"#isolate(input$userName)
  #       Password <- isolate(input$passwd)
  #       
  #       sql <- paste("select password from user_guinia where user_name ='",
  #                    Username, "'")
  #       tryCatch({
  #         rs <- dbSendQuery(con, sql)
  #         #obtengo el valor de la bd
  #         my_password <- fetch(rs,n=-1) #la funcion fetch() devuelve un frame
  #         #print(paste(">>>",dbGetStatement(rs)))
  #         if (nrow(my_password) > 0){ #length(Id.username) > 0 & length(Id.password) > 0) {
  #           #Se comparan los valores obtenidos con los registros
  #           if (as.character(my_password) == Password){ #Id.username == Id.password) { #si ambas variables son true el usuario es valido
  #             closeAlert(session, "alertLoginID")
  #             USER$Logged <- TRUE
  #             USER$role=get_role(Username)
  #             desconexionbd(con, drv)
  #           }
  #           else{ #la contraseña no coinside
  #             createAlert(session, "alertLogin", "alertLoginID", title = titleAlert,
  #                         content = "Username and password do not match.", 
  #                         style = "warning", append = FALSE)
  #           }
  #         }
  #       },error = function(e) {
  #         print(e)
  #         createAlert(session, "alertLogin", "alertLoginID", title = titleAlert,
  #                     content = "Username does not exist", 
  #                     style = "warning", append = FALSE)
  #       })
  #     }else { #existen campos vacios
  #       createAlert(session, "alertLogin", "alertLoginID", title = titleAlert,
  #                   content = "There are empty fields.", 
  #                   style = "warning", append = FALSE)
  #     }
  #     #         }
  #     #       }
  #   }
  # })
  # 
  # #Se muestran las opciones correspondientes para un usuario logeado o no
  # observe({
  #   if(USER$Logged == FALSE){ # si no esta logeado, solo se muestra el home
  #     output$signIn <- renderUI({
  #       loginRegister()
  #     })
  #     #menu del sidebar
  #     # output$side <- renderMenu({
  #     #   sidebar(FALSE)
  #     # })
  #   }
  #   if(USER$Logged == TRUE){ # el usuario esta logeado, se muestran todas las opciones
  #     output$signIn <- renderUI({
  #       fluidPage(
  #         titlePanel("Welcome!")
  #       )
  #     })
  #     #menu del sidebar
  #     # output$side <- renderMenu({
  #     #   sidebar(TRUE, USER$role)
  #     # })
  #   }
  # })
  # 
  # #---------------> Register
  # observe({
  #   if (!is.null(input$register)) {
  #     if (input$register > 0) { # se presiona el boton para registrarse
  #       if(input$newUserName == "" || input$name == "" || input$lastName == ""
  #          || input$email == "" || input$newPasswd == "" || input$confirmPasswd == ""){
  #         createAlert(session, "alertRegister", "alertRegisterID", title = titleAlert,
  #                     content = "All fields marked with * are required.", 
  #                     style = "warning", append = FALSE)
  #       }else if(input$newPasswd != input$confirmPasswd){
  #         createAlert(session, "alertRegister", "alertRegisterID", title = titleAlert,
  #                     content = "Password and confirm password do not match.", 
  #                     style = "warning",  append = FALSE)
  #       }else{ #Se puede registrar
  #         closeAlert(session, "alertRegisterID")
  #         #registro en la bd
  #         inputs <- paste(isolate(input$newUserName),isolate(input$name),isolate(input$lastName),
  #                         isolate(input$email),isolate(input$newPasswd), sep = "','")
  #         sql <- paste("insert into user_guinia (user_name,name,last_name,email,password) values ('",
  #                      inputs, "')")
  #         tryCatch({
  #           rs <- dbSendQuery(con, sql)
  #           session$onSessionEnded(desconexionbd(con, drv))
  #           USER$Logged <- TRUE
  #         },error = function(e) {
  #           createAlert(session, "alertRegister", "alertRegisterID", title = titleAlert,
  #                       content = "Username already exists.", 
  #                       style = "warning", append = FALSE)
  #         })
  #         #rs <- dbSendQuery(con, sql) #dbSendQuery(con, "insert into user_guinia (user_name,password) values ('user','1234')") 
  #       }
  #     }
  #   }
  # })
  
  #-------------------------------------------------------
  #-----------------------> data <-----------------------
  #Variable contiene el data set atualizado
  DATA_SET <- reactiveValues(name = NULL, data = NULL, original.obs = NULL, 
                             original.var = NULL)
  #variable que contiene la lista de data set contenidos en la app
  list.data <- reactiveValues(data_sets = list("iris" = 1, "airquality" = 2, "sleep" = 3),
                              data_setsID = list(iris, airquality, sleep))
  
  #data set disponibles
  observe({
    #data set contenidos en la app
   #data_sets <- list("iris" = 1, "airquality" = 2, "sleep" = 3)
    other_data <- list("Upload file" = 1, "URL file" = 2)
    #consulta para obtener los nombres de los data set
    sql <- "select name,id from data_set"
    drv <- dbDriver("PostgreSQL")
    con <- conexionbd(drv)
    tryCatch({
      rs <- dbSendQuery(con, sql)
      dataSet_name <- fetch(rs,n=-1)
      tam <- 3 #tres data set inicialmente
      if(nrow(dataSet_name) > 0){
        #agrego los nombres obtenidos a la lista data_sets
        list.data$data_sets[dataSet_name[,1]] <- c((tam+1):(tam+nrow(dataSet_name)))
        #agrego los id de los data set obtenidos a la lista
        n <- 4
        k <- (n-1) + nrow(dataSet_name)
        j <- 1
        for(i in n:k){
          list.data$data_setsID[[i]] <- as.character(dataSet_name[j,2])
          j <- j+1
        }
      }
      desconexionbd(con, drv) #desconexion con la bd
    },error = function(e) {
      desconexionbd(con, drv) #desconexion con la bd
      createAlert(session, "alertData", "alertDataID", title = titleAlert,
                  content = paste("",e), style = "warning", append = FALSE)
    })
  
    #muestra los data set disponibles
    output$view_data <- renderUI({
      if (is.null(input$dataSet)){ return()}
      switch(input$dataSet,
             '1'= selectInput("select_file", label = NULL, selected = 2,
                              choices = list.data$data_sets),
             '2'= radioButtons("select_newfile", label = NULL, choices = other_data)
      )
    })
  })
  
  #Seleccion de data set a utilizar
  file <- reactive({
    inFile <- input$file1
    if(is.null(input$select_file)){return()}
    if(input$dataSet == '1'){
      if(input$select_file < 4){ #data set locales de R
        list.data$data_setsID[[as.numeric(input$select_file)]]
      }else{ #se leen los archivos desde la carpeta file
  #      print(as.numeric(input$select_file))
        filename <- list.data$data_setsID[[as.numeric(input$select_file)]]
        read.csv( file.path(outputDir, filename), header=TRUE, sep=",")
      }
      
    }else if(!is.null(input$select_newfile) && input$select_newfile == '1' && !is.null(inFile)){
      tryCatch({
        closeAlert(session, "alertUploadID")
        head <- FALSE #por defecto header no se lee.
        if(input$header_upload == "TRUE"){head <- TRUE}
        DATA_SET$name <- input$file1$name
        read.csv(inFile$datapath, header = head, sep = input$sep_upload, 
                 quote = input$quote_upload, dec = input$dec_upload, na.strings= input$na_upload)
      }, error = function(e) {
        createAlert(session, "alertUpload", "alertUploadID", title = titleAlert,
                    content = paste("",e), style = "warning")
      })
    }
  })
  
  #Accion a realizar tras presionar el boton upload de la opcion URL (lee el archivo)
  observeEvent(input$upload, {
    tryCatch({
      closeAlert(session, "alertURLID")
      withProgress({
        setProgress(message = "This may take a while...") #"sep_url", "dec_url", "quote_url", "header_url"
        head <- FALSE #por defecto header no se lee.
        if(input$header_url == "TRUE"){head <- TRUE}
        DATA_SET$data <- read.csv(input$url, header = head, sep = input$sep_url, 
                                  quote = input$quote_url, dec = input$dec_url, na.strings= input$na_url)
        #nombre del data set
        splitURL <- strsplit(input$url, "/")
        DATA_SET$name <- splitURL[[1]][length(splitURL[[1]])]
        DATA_SET$original.obs <- nrow(DATA_SET$data)
        DATA_SET$original.var <- ncol(DATA_SET$data)
      })
    }, error = function(e) {
      createAlert(session, "alertRUL", "alertURLID", title = titleAlert,
                  content = paste("",e), style = "warning")
    })
  })
  
  observe({ #Se asigna el data set seleccionado a la varible global
    if(is.null(input$select_file)){return()}
    #obtener el nombre del data set
    if(input$dataSet == '1'){
      switch(input$select_file,
             '1'= DATA_SET$name <-"iris",
             '2'= DATA_SET$name <-"airquality",
             '3'= DATA_SET$name <-"sleep"
      )
    }
    DATA_SET$data <- file()
    DATA_SET$original.obs <- nrow(file())
    DATA_SET$original.var <- ncol(file())
  })
  
  #guardar el data set obtenido de upload
  observeEvent(input$saveFileUpload, {
    inFile <- input$file1
    if(!is.null(inFile)){
      drv <- dbDriver("PostgreSQL")
      con <- conexionbd(drv)
      #se guarda el data en la carpeta file
      idData <- saveData(DATA_SET$data)
      #se guarda su info en la bd
      sql <- paste("insert into data_set(id,name) values ('",
                   paste(idData, DATA_SET$name, sep = "','"),"')", sep = '')
      tryCatch({
        rs <- dbSendQuery(con, sql)
        desconexionbd(con, drv) #desconexion con la bd
      },error = function(e) {
        createAlert(session, "alertData", "alertDataID", title = titleAlert,
                    content = paste("",e), 
                    style = "warning", append = FALSE)
      })
      desconexionbd(con, drv) #desconexion con la bd
      createAlert(session, "alertSaveData", "alertSaveDataID", title = titleAlertInfo,
                  content = "The data set was saved successfully.", style = "success")
    }else{
      createAlert(session, "alertSaveData", "alertSaveDataID", title = titleAlert,
                  content = "The file couldn't be saved.", style = "success")
    }
  })
  #guardar el data set obtenido de la URL
  observeEvent(input$saveFileURL, {
    if(input$upload){
      drv <- dbDriver("PostgreSQL")
      con <- conexionbd(drv)
      #se guarda el data en la carpeta file
      idData <- saveData(DATA_SET$data)
      #se guardan su info en la bd
      sql <- paste("insert into data_set(id,name) values ('",
                   paste(idData, DATA_SET$name, sep = "','"),"')", sep = '')
      tryCatch({
        rs <- dbSendQuery(con, sql)
        desconexionbd(con, drv) #desconexion con la bd
      },error = function(e) {
        createAlert(session, "alertData", "alertDataID", title = titleAlert,
                    content = paste("",e), style = "warning", append = FALSE)
      })
      desconexionbd(con, drv) #desconexion con la bd
      createAlert(session, "alertSaveData", "alertSaveDataID", title = titleAlertInfo,
                  content = "The data set was saved successfully.", style = "success")
    }else{
      createAlert(session, "alertSaveData", "alertSaveDataID", title = titleAlert,
                  content = "The file couldn't be saved.", style = "success")
    }
  })
  
  #muestro un resumen del data set seleccionado
  output$str_data <- renderPrint({
    str(DATA_SET$data)
  })
  
  #muestro un sumary del data set seleccionado
  output$summary_data <- renderPrint({
    summary(DATA_SET$data)
  })
  
  #-------> Editar el data set
  observe({ #opciones para editar el data set
    if(is.null(DATA_SET$data)){return()}
    switch (input$dataSetEdit,
      '1' = {output$selectVarDelete <- renderUI({
                                            fluidRow(column(12,
                                               selectInput("varDelete", label = "Select columns", 
                                                           choices = names(DATA_SET$data), multiple = TRUE),
                                               bsButton("deleteCol", "Delete Columns", style = "success")
                                               ))
            })},
      '2' = { nums <- sapply(DATA_SET$data, is.numeric)
              print(nums)
              DATA_SET$data <- DATA_SET$data[, nums]
              createAlert(session, "alertEditFile", "alertEditFileID", title = titleAlertInfo,
                          content = "Nominal values have been removed correctly.", style = "success")}
    )
  })
  
  #eliminar columnas del data set
  observeEvent(input$deleteCol,{
    if(input$varDelete != "" && !is.null(input$varDelete)){
      DATA_SET$data <- DATA_SET$data[,!names(DATA_SET$data) %in% input$varDelete]
      createAlert(session, "alertEditFile", "alertEditFileID", title = titleAlertInfo,
                  content = "Columns have been removed correctly.", style = "success")
    }else{
      createAlert(session, "alertEditFile", "alertEditFileID", title = titleAlert,
                  content = "Columns haven't been removed.", style = "warning")
    }
  })
  
  #***************************************
  #-----------------------> visualization
  
  #----------> Graficos de visualizacion
  
  #Actualizo el maximo del slider con el valor del tamaÃ±o del archivo seleccionado
  output$slider_Scatterplot <- renderUI({
    if(is.null(DATA_SET$data)){return()}
    treeSlider("x_scatter", "y_scatter", "z_scatter", DATA_SET$data, strx, stry, strz, FALSE)
  })
  
  output$slider_SatterplotSize <- renderUI({
    if(is.null(DATA_SET$data)){return()}
    box(
      width = 6, status = "success", h4("Range"),
      sizeAndAlpha("pointSizeScatter","alphaPointScatter", 
                   "Point Size", "Alpha Point")
    )
  })
  #seleccion de atributos y observaciones del data set
  # dat_Scatterplot <- reactive({
  #   if(is.null(DATA_SET$data)){return()}
  #   if(is.null(input$y_scatter) || is.null(input$x_scatter) || is.null(input$z_scatter)){return()}
  #   x <- DATA_SET$data[,-input$y_scatter]
  #   
  #   x[input$z_scatter[1]:input$z_scatter[2],input$x_scatter[1]:input$x_scatter[2]]
  #   #print("x2",str(x))
  # })
  
  scatterPlot <- function(){ #funcion que genera el scatterPlot
    if(is.null(input$x_scatter) || is.na(input$x_scatter)){return()}
    if(is.null(input$y_scatter) || is.null(input$z_scatter)){return()}
    #paleta de colores para el gráfico
    myPalette <- c(input$col1, input$col2, input$col3)
    withProgress({
      setProgress(message = "This may take a while...")
      ScatterplotMatrix(DATA_SET$data[,-input$y_scatter], c(input$x_scatter[1]:input$x_scatter[2]), DATA_SET$data[,input$y_scatter],
                        names(DATA_SET$data)[[input$y_scatter]], as.numeric(input$pointSizeScatter), input$alphaPointScatter,
                        colours = myPalette)
    })
  }
  
  #Grafico correspondiente a scatterPlot 
  output$scatter_plot <- renderPlot({
    tryCatch({
      closeAlert(session, "alertScatterID")
      scatterPlot()
    }, error = function(e) {
      print(e)
    createAlert(session, "alertScatter", "alertScatterID", title = titleAlert,
                content = paste("",e), style = "warning")
    })
    
  })
  
  #-------------->dowload image plot
  observe({
    output$download_Scatterplot <- downloadGeneral(input$radio_Scatterplot, scatterPlot(), DATA_SET$name)
  })
  
  #Slider visualizacion grafico parallel x e y
  output$slider_parallelPlot <- renderUI({
    if(is.null(DATA_SET$data)){return()}
    box(
      width = 6, status = "success",
      h4("Range"),
      sliderInput("x_parallel", label = strx, min = 1, 
                  max = ncol(DATA_SET$data)-1, value = c(1, ncol(DATA_SET$data)-1)),
      sliderInput("y_parallel", label = stry, min = 1, 
                  max = ncol(DATA_SET$data), value = 2)
    )
  })
  
  #Slider visualizacion grafico parallel observations y alfa
  output$slider_parallelPlot2 <- renderUI({
    if(is.null(DATA_SET$data)){return()}
    box(
      width = 6, status = "success",
      h4("Range"),
      sliderInput("z_parallel", label = strz, min = 1, 
                  max = nrow(DATA_SET$data), value = c(1, nrow(DATA_SET$data))),
      sizeAndAlpha("lineSize","alphaLine", "Line Size", "Alpha Line")
    )
  })
  
  
  #seleccion de atributos y observaciones del data set
  data_Parallelx <- reactive({
    if(is.null(DATA_SET$data)){return()}
    x <- DATA_SET$data[,-input$y_parallel]
    x[input$z_parallel[1]:input$z_parallel[2],input$x_parallel[1]:input$x_parallel[2]]
  })
  
  data_Parallely <- reactive({
    if(is.null(DATA_SET$data)){return()}
    DATA_SET$data[,input$y_parallel]
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
                   names(DATA_SET$data)[[input$y_parallel]], as.numeric(input$lineSize), input$alphaLine, TRUE, colours = myPalette)
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
  observeEvent(input$deleteMS,{
      DATA_SET$data <- na.omit(DATA_SET$data)
  })
  
  output$sumMV <- renderUI({
    helpText("The data set contains", nrow(DATA_SET$data[!complete.cases(DATA_SET$data),]),
             "missing values.")
  })
  
  #Slider visualizacion grafico de missing values
  output$slider_missingValues <- renderUI({
    if(is.null(DATA_SET$data)){return()}
    twoSlider("attributes","observation",DATA_SET$data,"Attributes",strz)
  })
  
  #Obtengo la seleccion de atributos y observaciones para la Opcion 1
  data_boxPlot <- reactive({
    if(is.null(DATA_SET$data)){return()}
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
                  content = paste("",e), style = "warning")
    })
  })
  
  #-------------->dowload image plot
  observe({
    output$download_boxplot <- downloadGeneral(input$radio_boxplot, matrixplot(data_boxPlot()), DATA_SET$name)
  })
  
  #Slider visualizacion grafico de missing VIM option2
  output$slider_histogramPlot <- renderUI({
    if(is.null(DATA_SET$data)){return()}
    twoSlider("attributes_histogram","observation_histogram",DATA_SET$data,"Attributes",strz)
  })
  
  #Obtengo la seleccion de atributos y observaciones para la Opcion 2
  data_histogramPlot <- reactive({
    if(is.null(DATA_SET$data)){return()}
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
        aggr(data_histogramPlot(), col=c('dark grey','red'), numbers=TRUE, 
             sortVars=TRUE, labels=names(data_histogramPlot()), cex.axis=.8, gap=1, 
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
    if(is.null(DATA_SET$data)){return()}
    treeSlider("x_missingScatter", "y_missingScatter", "z_missingScatter", DATA_SET$data, strx, stry, strz, TRUE)
  })
  
  #Obtengo la seleccion de atributos a comparar para la Opcion 3
  data_missingScatter <- reactive({
    if(is.null(DATA_SET$data)){return()}
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
  
  #corroborar que el data set solo contenga valores numericos
  observe({
    numNominal <- cantCol_nominal(DATA_SET$data) #funcion contenida en generalTools.r
    if(numNominal > 0){
      createAlert(session, "alertNoise", "alertNoiseID", title = titleAlert,
                  content = paste("Only numerical instances, the data set contains",
                                  numNominal,"nominal instances"), style = "warning")
    }
  })
  
  #Slider visualizacion grafico parallel x e y
  output$slider_nremoval <- renderUI({
    if(is.null(DATA_SET$data)){return()}
    box(
      width = 6, status = "success",
      h4("Range"),
      sliderInput("attributes_nremoval", label = strx, min = 1, 
                  max = ncol(DATA_SET$data)-1, value = c(1, ncol(DATA_SET$data)-1)),
      sliderInput("response_nremoval", label = stry, min = 1, 
                  max = ncol(DATA_SET$data), value = 2)
    )
  })
  
  #Slider visualizacion grafico parallel observations y alfa
  output$slider_nremoval2 <- renderUI({
    if(is.null(DATA_SET$data)){return()}
    box(
      width = 6, status = "success",
      h4("Range"),
      sliderInput("observations_removal", label = strz, min = 1, 
                  max = nrow(DATA_SET$data), value = c(1, nrow(DATA_SET$data))),
      sizeAndAlpha("lineSizeNremoval","alphaLineNremoval", "Line Size", "Alpha Line")
    )
  })
  
  #seleccion de atributos y observaciones del data set
  datNremovalx <- reactive({
    if(is.null(DATA_SET$data)){return()}
    if(cantCol_nominal(DATA_SET$data) == 0){
      closeAlert(session, "alertNoise")
      x <- DATA_SET$data[,-input$response_nremoval]
      x[input$observations_removal[1]:input$observations_removal[2],
               input$attributes_nremoval[1]:input$attributes_nremoval[2]]
    }
  })
  
  #seleccion de la variable de respuesta
  datNremovaly <- reactive({
    if(is.null(DATA_SET$data)){return()}
    if(cantCol_nominal(DATA_SET$data) == 0){
      DATA_SET$data[,input$response_nremoval]
    }
  })
  
  plotNremoval <- function(){
    myPalette <- c(input$col1, input$col2, input$col3)
    # A ParallelPlot of all rows and all columns
    withProgress({
      setProgress(message = "This may take a while...")
      ParallelPlot(datNremovalx(), seq(1,nrow(datNremovalx()),1), seq(1,ncol(datNremovalx()),1), datNremovaly(), 
                   names(DATA_SET$data)[[input$response_nremoval]], as.numeric(input$lineSizeNremoval), input$alphaLineNremoval, TRUE, colours = myPalette)
    })
  }
  
  #grafico de ruido
  output$nremoval <- renderPlot({
    if(is.null(input$attributes_nremoval) || is.na(input$attributes_nremoval)){
      return()
    }
    if(cantCol_nominal(DATA_SET$data) > 0){return()}
    #grafico del ruido
    plotNremoval()
  })
  
  #-------------->dowload image plot
  observe({
    output$download_plotNoise <- downloadGeneral(input$radio_nremoval, plotNremoval(), DATA_SET$name)
  })
  
  #Calcular el numero de columnas con ruido
  output$columnsNoise <- renderUI({
    if(cantCol_nominal(DATA_SET$data) > 0 || is.null(DATA_SET$data)){return()}
    tryCatch({
      closeAlert(session, "alertNoiseID")
      diffValues <- calculateDiff(DATA_SET$data)
      columnsNoise <- getColumnsNoise(diffValues, input$limitNoise)
      helpText("Have ", paste(nrow(columnsNoise), " noise columns."))
    }, error = function(e) {
      createAlert(session, "alertNoise", "alertNoiseID", title = titleAlert,
                  content = paste("",e), #"Missing values in data", 
                  style = "warning")
    })
  })
  
  #Eliminación del ruido del data set
  observeEvent(input$rnoise,{
    if(cantCol_nominal(DATA_SET$data) > 0 || is.null(DATA_SET$data)){return()}
    if(anyNA(DATA_SET$data)){
      createAlert(session, "alertNoise", "alertNoiseID", title = titleAlert,
                  content = "Data set have missing values", style = "warning")
    }else{
      diffValues <- calculateDiff(DATA_SET$data)
      columnsNoise <- getColumnsNoise(diffValues, input$limitNoise)
      #  columnsNoise <- as.data.frame(columnsNoise[,1] + ncol(missingV()))
      DATA_SET$data[,-columnsNoise[,1]]
    }
  })
  
  #************************************************
  #-------------> Local outlier factor
  
  #corroborar que el data set solo contenga valores numericos
  observe({
    numVar_nominal <- cantCol_nominal(DATA_SET$data)
    if(numVar_nominal > 0){
      createAlert(session, "alertlof", "alertlofID", title = titleAlert,
                  content = paste("Only numerical instances, the data set contains",
                                  numVar_nominal,"nominal instances"), style = "warning")
    }
  })
  
  ## scores for the original data
  outlier.scores <- reactive({
    if(cantCol_nominal(DATA_SET$data) > 0 || is.null(DATA_SET$data)){return()}
    closeAlert(session, "alertlofID")
    lof(DATA_SET$data, k= c(5:10))
  })
  
  #Slider visualizacion grafico de missing VIM option2
  output$sliderLOF <- renderUI({
    if(is.null(outlier.scores)){return()}
    minimo <- round(min(outlier.scores()), digits=2)
    maximo <- round(max(outlier.scores()), digits=2)
    sliderInput("thresholdt", "Threshold", min = minimo,
                max = maximo, value = (minimo + maximo)/2, step= 0.01)
  })
  
  #llamado a la funcion lof, la cual devuelve una lista
  res_lof <- reactive({
    if(is.na(outlier.scores()) || is.null(outlier.scores())){return()}
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
    if(is.null(res_lof())){return()}
    data.frame(res_lof()[1]) ## scores of data without outliers
  })
  
  #funcion que genera el grafico de densidad original
  lof_plot1 <- function(){
    tryCatch({
      withProgress({
        closeAlert(session, "alertlof1ID")
        setProgress(message = "This may take a while...")
        DensityPlot(data.frame(outlier.scores()), ncol(data.frame(outlier.scores())))
      })
    }, error = function(e) {
      createAlert(session, "alertlof1", "alertlof1ID", title = titleAlert,
                  content = paste("",e), style = "warning")
    })
  }
  
  #grafico inicial density plot
  output$densityPlot <- renderPlot({
    if(is.null(outlier.scores())){return()}
    lof_plot1()
  })
  
  #funcion que genera el grafico de densidad tras el corte
  lof_plot2 <- function(){
    tryCatch({
      withProgress({
        closeAlert(session, "alertlof2ID")
        setProgress(message = "This may take a while...")
        DensityPlot(withoutOutliers.scores(), ncol(outlier.scores())) #Generating a plot of outliers scores
      })
    }, error = function(e) {
      createAlert(session, "alertlof2", "alertlof2ID", title = titleAlert,
                  content = paste("",e), style = "warning")
    })
  }
  
  #Grafico resultante tras realizar corte del primer density
  output$densityPlotResult <- renderPlot({
    if(is.null(outlier.scores()) || is.null(withoutOutliers.scores()) ){return()}
    lof_plot2()
  })
  
  #-------------->dowload image plot
  observe({
    output$download_plotLOF <- downloadTwoPlot(input$radio_lof, lof_plot1(), lof_plot2(), DATA_SET$name)
  })
  
  #Cantidad de outlier existentes
  output$howManyOutliers <- renderPrint({
    if(is.na(res_lof()) || is.null(res_lof())){return()}
    as.numeric(res_lof()[3])
  })
  
  #Posicion de los outlier en el archivo
  output$posOutliers <- renderPrint({
    if(is.na(res_lof()) || is.null(res_lof())){return()}
    data.frame(res_lof()[4])  ## the positions of the outliers in the original data and theirs respective scores
  })
  
  #without outliers data
  output$strWithoutOutliers <- renderPrint({
    if(is.na(res_lof()) || is.null(res_lof())){return()}
    dataWithoutOutliers<-data.frame(res_lof()[2])  ##the data without outliers
    str(dataWithoutOutliers)
  })
  
  #Accion a realizar tras presionar el boton "WithoutOutliers" de LOF
  observeEvent(input$delete_lof, {
    if(is.null(res_lof())){return()}
    tryCatch({
      DATA_SET$data <- data.frame(res_lof()[2])
    }, error = function(e) {
      print(e)
    })
  })
  
  #-------------------------------------------------------
  #-----------------------> Transformation <-----------------------
  
  #************************************************
  #-------------> Normalization
  #salida dinamica de rango para normalizacion
  output$range_normalization <- renderUI({
    if (is.null(input$normalizationType) || is.null(DATA_SET$data))
      return()
    switch(input$normalizationType,
           '1' =  tags$div( class = 'col-sm-8',
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
  
  #se valida que solo sean datos numericos
  observe({
    numVar_nominal <- cantCol_nominal(DATA_SET$data)
    if(numVar_nominal > 0){
      createAlert(session, "alertNormalization", "alertNormalizationID", title = titleAlert,
                  content = paste("Only numerical instances, the data set contains",
                                  numVar_nominal, "nominal instances"), style = "warning")
    }
  })
  
  #obtengo el tipo de normalizacion seleccionada y aplico la normalizacion correspondiente
  normalization_type <- reactive({
    if (is.null(input$normalizationType) || cantCol_nominal(DATA_SET$data) > 0)
      return()
    closeAlert(session, "alertNormalizationID")
    switch(input$normalizationType,
           '1'=  withProgress({
             setProgress(message = "This may take a while...")
             normalizeData(DATA_SET$data, input$min, input$max)
           }),
           '2'=  withProgress({
             setProgress(message = "This may take a while...")
             normalizeData(DATA_SET$data)
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
  output$normalized_data <- renderPrint({
    if(is.null(normalization_type())){ return()}
    normalization_type()[1:10,]
  })
  
  #muestro un sumary
  output$summary_normalization <- renderPrint({
    if(is.null(normalization_type())){ return()}
    summary(normalization_type())
  })
  
  #evento tras precionar el boton apply
  observeEvent(input$apply_normalization,{
    if(is.null(normalization_type())){ return()}
    DATA_SET$data <- normalization_type()
    createAlert(session, "alertNormalizationApply", "alertNormalizationApplyID", title = titleAlertInfo,
                content = "The normalization has been applied correctly.", style = "success")
  })
  
  #************************************************
  #-------------> Reduccion de la dimencionalidad
  
  #Slider visualizacion grafico PCA
  output$slider_pca <- renderUI({
    if(is.null(DATA_SET$data)){return()}
    twoSlider("attributes_pca","observation_pca",DATA_SET$data,"Attributes",strz)
  })
  
  #Obtengo la seleccion de atributos y observaciones para pca
  data_pca <- reactive({
    if(cantCol_nominal(DATA_SET$data) > 0 || is.null(input$observation_pca) ||
       is.null(input$attributes_pca)){ return()}
    DATA_SET$data[input$observation_pca[1]:input$observation_pca[2], 
                  input$attributes_pca[1]:input$attributes_pca[2]]
  })
  
  pca <- reactive({
    if(is.null(data_pca())){return()}
    prcomp(data_pca(), center = TRUE, scale. = TRUE)
  })
  
  pcaGrafic <- function(){
    if(is.null(input$attributes_pca) || is.na(input$attributes_pca)){
      return()
    }
    withProgress({
      setProgress(message = "This may take a while...")
      # p1 <- elbowPlot(pca()) #Proportion of Variance Explained
      # p2 <- CumulativeVariances_plot(pca())
      # grid.arrange( p1, p2, ncol=2)
      pve=100*pca()$sdev^2/sum(pca()$sdev^2)
      par(mfrow=c(1,2))
      plot(pve,  type="o", ylab="PVE", xlab="Principal Component", col="blue")
      plot(cumsum(pve), type="o", ylab="Cumulative PVE", xlab="Principal Component", col="brown3")
    })
  }
  
  #grafico de PCA
  output$pca <- renderPlot({
    tryCatch({
      closeAlert(session, "alertPCAID")
      pcaGrafic()
    }, error = function(e) {
      createAlert(session, "alertPCA", "alertPCAID", title = titleAlert,
                  content = paste("",e), style = "warning")
    })
  })

  #data luego de aplicar un metodo de reduccion como pca
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
    if(is.null(pca())){return()}
    summary(pca())
  })
  
  output$summary_reduceDimensionality <- renderUI({
    d <- dim(DATA_SET$data)
    helpText("Data frame", DATA_SET$name,":",d[1],"obs. of",d[2],"variables")
  })
  
  #------------SVD
  #solo valores numericos
  observe({
    numVar_nominal <- cantCol_nominal(DATA_SET$data)
    if(numVar_nominal > 0){
      createAlert(session, "alertSVD", "alertSVDID", title = titleAlert,
                  content = paste ("Only numerical instances, the data set contains",
                                   numVar_nominal,"nominal instances"), style = "warning")
    }
  })
  
  s <- reactive({
    if(cantCol_nominal(DATA_SET$data) > 0 || is.null(DATA_SET$data)){return()}
    closeAlert(session,"alertSVDID")
    dat <- as.matrix(DATA_SET$data)
    svd(dat)
  })
  
  #grafico para SVD
  output$svd <- renderPlot({
    if(is.null(s())){return()}
    withProgress({
      setProgress(message = "This may take a while...")
      plot(cumsum(s()$d^2/sum(s()$d^2))) 
    })
  })
  
  output$s <- renderPrint({
    if(is.null(s())){return()}
    s()$d
  })
  
  #------------> Attribute Selection
  #seleccion de la variable de respuesta y numero de atributos a seleccionar
  output$option_attributeS <- renderUI({
    if(is.null(DATA_SET$data)){return()}
    namesVariables <- names(DATA_SET$data)
    column(12,
           selectInput("attributeS_response", label = h4("Response variable"), 
                       choices = namesVariables, selected = namesVariables[ncol(DATA_SET$data)]),
           numericInput("attribute_num", "Number of attributes to be selected", value = 2,
                        min = 1, max = ncol(DATA_SET$data)-1)
          )
  })
  
  #calculos de los pesos de las variables
  weights <- reactive({
    if(is.null(input$attributeS_response) || is.na(input$attributeS_response)) {return()}
    (fmla <- as.formula(paste(input$attributeS_response, " ~ ", ".", sep='')))
    information.gain(fmla, DATA_SET$data)
  })
  
  #muestra informacion de los pesos
  output$print_weights <- renderPrint({
    if( is.null(weights())) {return()}
    weights()
  })
  
  #indica ordenado las mejores variables del dataSet
  output$print_subset <- renderPrint({
    if( is.null(weights())) {return()}
    cutoff.k(weights(), input$attribute_num)
  })
  
  #indica un resumen del dataSet
  output$print_dataAtributte <- renderUI({
    d <- dim(DATA_SET$data)
    helpText("Data frame", DATA_SET$name,":",d[1],"obs. of",d[2],"variables.")
  })
  
  #evento tras presionar el boton para aplicar cambios
  observeEvent(input$apply_attributeS,{
    atributte <- cutoff.k(weights(), input$attribute_num)
    atributte[input$attribute_num+1] <- input$attributeS_response
    DATA_SET$data <- DATA_SET$data[, names(DATA_SET$data) %in% atributte]
  })
  
  #-------------------------------------------------------
  #-----------------------> Regresion <-----------------------
  
  #-----------------------> validation type
  
  #particion en porcentaje de train y test
  train_lm <- reactive({
    if(is.null(input$porcentTest_lm) || is.null(DATA_SET$data)){ return() }
    dataPartition(DATA_SET$data, input$porcentTest_lm)
  })
  
  #-----------------------> lm
  #seleccion de la variable dependiente o de respuesta
  output$select_linearModel <- renderUI({
    if(is.null(DATA_SET$data)){return()}
    select_model("lm_response", "lm_predictors", data.frame(DATA_SET$data))  
  })
  
  #Aplicando el modelo lm
  model_lm <- reactive({
    if(is.null(input$lm_response)){return()}
    fmla <- formula_model(input$lm_predictors,input$lm_response)
    print('fmla:')
    print(fmla)
    if (is.null(input$validationType_lm)){return()}
    if(input$validationType_lm == '3'){
      lm(fmla, data = data.frame(DATA_SET$data[train_lm(), ]))
    }else{
      lm(fmla, data=DATA_SET$data)
    }
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
    if(is.null(DATA_SET$data)){return()}
    data.frame(predictors(DATA_SET$data, input$lm_predictors, input$lm_response))
  })
  
  #coeficientes del modelo
  thetaPredict_lm <- function(fit,x){ cbind(1,x)%*%fit$coefficients }
  
  #tipo de validacion para el modelo
  validation_lm <- reactive({
    # tryCatch({
      if (is.null(input$validationType_lm) || is.null(model_lm()) || is.null(DATA_SET$data))
        return()
      if(input$validationType_lm == '2' && is.null(input$fileTest_lm))
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
                   response_variable <- DATA_SET$data[-train_lm(),input$lm_response]
                   data.frame(cbind(Prediction, response_variable))
                   }
             
      )
    # }, error = function(e) {
    #   print(e)
    #   # createAlert(session, "alertValidation", "alertValidationID", title = titleAlert,
    #   #             content = paste("",e), style = "warning")
    # })
  })
  
  #resultado tras aplicar el metodo de validacion seleccionado
  output$resultValidation_lm <- renderPrint({
    if(!is.null(validation_lm())){
      validation_lm()
    }
  })
  
  #resultado grafico de la prediccion realizada
  output$plotValitation_lm <- renderPlot({
    if(is.null(validation_lm())){return()}
    if(input$validationType_lm == '3'){
      simplePlot(validation_lm(),  DATA_SET$data[-train_lm(),input$lm_response], 2, 1, input$lm_response, 2, 0.9)
    }else{
      simplePlot(validation_lm(),  DATA_SET$data[,input$lm_response], 2, 1, input$lm_response, 2, 0.9)
    }
  })
  
  #***** Colinearity Test lm
  
  #resultado de aplicar el test de colinealidad
  output$colinearity_result <- renderPrint({
    if(!is.null(model_lm()))
      vif(model_lm())
  })
  
  #tolerancia del test de colinealidad
  output$colinearity_tolerance <- renderPrint({
    if(!is.null(model_lm()))
      1/vif(model_lm())
  })
  
  #promedio del test de colinealidad
  output$colinearity_mean <- renderPrint({
    if(!is.null(model_lm()))
    mean(vif(model_lm()))
  })
  
  #-----------------------> pls
  #seleccion de la variable de respuesta
  output$select_pls <- renderUI({
    if(is.null(DATA_SET$data)){return()}
    select_model("pls_response", "pls_predictors", DATA_SET$data)  
  })
  
  #particion en porcentaje de train y test
  train_pls <- reactive({
    if(is.null(input$porcentTest_pls) || is.null(DATA_SET$data)){ return() }
    dataPartition(DATA_SET$data, input$porcentTest_pls)
  })
  
  #numero de componentes para el modelo
  output$componentes_pls <- renderUI({
    if(is.null(DATA_SET$data)){return()}
    numericInput("comp_pls", h4("PLS comp"), value = 2, min = 2, max = ncol(DATA_SET$data) )
  })
  
  #obtengo los predictores del modelo
  predictores_pls <-  reactive({
    if(is.null(DATA_SET$data)){return()}
    data.frame(predictors(DATA_SET$data, input$pls_predictors, input$pls_response))
  })
  
  #Aplicando el modelo pls
  model_pls <- reactive({
    tryCatch({
      if(anyNA(DATA_SET$data)){
        createAlert(session, "alertpls", "alertplsID", title = titleAlert,
                    content = "Data set have missing values", style = "warning")
      }else{
        closeAlert(session, "alertplsID")
#         val <- TRUE #valor por defecto para la validacion cruzada
#         if(input$crosval == "FALSE"){ val <- FALSE }
        if(input$validationType_pls == '2' && is.null(input$fileTest_pls)){
          return()
        }else{ #Se aplica el modelo pls
#           if(is.null(input$pls_predictors)){
#             (fmla <- as.formula(paste(paste(input$pls_response, " ~ "), ".")))
#           }
#           else{
#             (fmla <- as.formula(paste(paste(input$pls_response, " ~ "), paste(input$pls_predictors, collapse= "-"))))
#           }
          
          fmla <- formula_model(input$pls_predictors,input$pls_response)
          print('fmla:')
          print(fmla)
          
          #aplicando el modelo pls
          if (is.null(input$validationType_pls)){return()}
          switch(input$validationType_pls,
                 '1' = plsr(fmla, ncomp = input$comp_pls, data = DATA_SET$data, validation = "CV", segments = 10),
                 '2' = plsr(fmla, ncomp = input$comp_pls, data = DATA_SET$data),
                 '3' = plsr(fmla, ncomp = input$comp_pls, data = DATA_SET$data[train_pls(),])
          )
#           plsreg1(predictores_pls(), DATA_SET$data[,input$pls_response], 
#                   crosval = val, comps = input$comp_pls)
        }
      }
    }, error = function(e) {
      createAlert(session, "alertpls", "alertplsID", title = titleAlert,
                  content = paste("",e), style = "warning")
    })
  })
  
  #Resultado obtenido tras aplicar el  modelo
  output$result_pls <- renderPrint({
    if(is.null(model_pls())){ return()}
    summary(model_pls())
  })
  
  #****** validacion pls
  
  #coeficientes del modelo
  thetaPredict_pls <- function(fit,x){ cbind(1,x)%*%fit$coefficients }
  
  #tipo de validacion para el modelo
  validation_pls <- reactive({
    tryCatch({
      if (is.null(input$validationType_pls) || is.null(model_pls()))
        return()
      if(input$validationType_pls == '2' && is.null(input$fileTest_pls))
        return()
      closeAlert(session,"alertValidationID")
      
      switch(input$validationType_pls,
             '1' = model_pls()$validation,
             '2' = { inFile <- input$fileTest_pls
                    predict(model_pls(), newdata = read.csv(inFile$datapath))},
             '3' = { Prediction <-  predict(model_pls(), newdata = DATA_SET$data[-train_pls(),])
                     response_variable <- DATA_SET$data[-train_pls(),input$pls_response]
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
    if(!is.null(validation_pls()))
      validation_pls()
  })
  
  #grafico correspondiente a correlaciones del modelo pls
  output$plotCorr <- renderPlot({
    if(is.null(model_pls())){ return()}
    corrplot(model_pls(), comps = 1:input$comp_pls)
  })
  
  #grafico correspondiente a MSEP del modelo pls
  output$plotMSEP <- renderPlot({
    if(is.null(model_pls())){ return()}
    plot(MSEP(model_pls()), legendpos = "topright")
  })
  
  #grafico correspondiente a los coeficientes del modelo pls
  output$plotCoef <- renderPlot({
    if(is.null(model_pls())){ return()}
    coefplot(model_pls(), ncom = 1:input$comp_pls, legendpos = "bottomright")
  })
  
  #grafico correspondiente a la predicción del modelo pls
  output$plotPred <- renderPlot({
    if(is.null(model_pls())){ return()}
    predplot(model_pls(), ncomp = 1:input$comp_pls)
  })
  #-----------------------> ridge
  
  #particion en porcentaje de train y test
  train_ridge <- reactive({
    if(is.null(input$porcentTest_ridge) || is.null(DATA_SET$data)){ return() }
    dataPartition(DATA_SET$data, input$porcentTest_ridge)
  })
  
  #seleccion de la variable dependiente
  output$select_ridge <- renderUI({
    if(is.null(DATA_SET$data)){return()}
    select_model("ridge_response", "ridge_predictors", DATA_SET$data)
  })
  
  #obtengo los predictores del modelo
  predictores_ridge <-  reactive({
    if(is.null(DATA_SET$data)){return()}
    as.data.frame(predictors(DATA_SET$data, input$ridge_predictors, input$ridge_response))
  })
  
  #Aplicando el modelo de validacion cruzada ridge para determinar el lambda minimo
  model_cvridge <- reactive({
    if(is.null(DATA_SET$data)){return()}
    tryCatch({
      if(anyNA(DATA_SET$data)){
        createAlert(session, "alertCVRidge", "alertCVRidgeID", title = titleAlert,
                    content = "Data set have missing values", style = "warning")
      }else{
        closeAlert(session, "alertCVRidgeID")
        #Se aplica el modelo ridge
        withProgress({
          setProgress(message = "This may take a while...")
          #aplicar validacion cruzada de ridge para obtener el mejor lambda
          if (is.null(input$validationType_ridge)){return()}
          
          grid <- 10^seq(10, -2, length = 100) #secuencia de lambda's
          if(input$validationType_ridge == "3"){
            train_predictors <- as.matrix(predictores_ridge()[train_ridge(),])
              # model.matrix(fmla,DATA_SET$data[train_ridge(),])[,-1]
            cv.glmnet(train_predictors, DATA_SET$data[train_ridge(),input$ridge_response], 
                      alpha = 0, lambda = grid)
          }else{
            cv.glmnet(as.matrix(predictores_ridge()), DATA_SET$data[,input$ridge_response], 
                      alpha = 0, lambda = grid)
          }
        })
      }
    }, error = function(e) {
      createAlert(session, "alertCVRidge", "alertCVRidgeID", title = titleAlert,
                  content = paste("",e), style = "warning")
    })
  })
  
  #aplicando rigde para el lambda min obtenido en model_cvridge()
  model_ridge <- reactive({
    if(is.null(DATA_SET$data)){return()}
    tryCatch({
      if(anyNA(DATA_SET$data)){
        createAlert(session, "alertRidge", "alertRidgeID", title = titleAlert,
                    content = "Data set have missing values", style = "warning")
      }else{
        closeAlert(session, "alertRidgeID")
        #Se aplica el modelo ridge con una formula como el lm
        fmla <- formula_model(input$ridge_predictors,input$ridge_response)
        print('fmla:')
        print(fmla)
        
        if (is.null(input$validationType_ridge)){return()}
        if(input$validationType_ridge == '3'){
          lm.ridge(fmla, data = DATA_SET$data[train_ridge(),], lambda = model_cvridge()$lambda.min)
        }else{ 
          lm.ridge(fmla, data = DATA_SET$data, lambda = model_cvridge()$lambda.min)}
      }
    }, error = function(e) {
      createAlert(session, "alertRidge", "alertRidgeID", title = titleAlert,
                  content = paste("",e), style = "warning")
    })
  })
  
  #Resultado obtenido tras aplicar el  modelo
  output$result_ridge <- renderPrint({
    if(is.null(model_ridge())){ return()}
    summary(model_ridge())
  })
  
  #grafico de lamda
  output$plot_ridge <- renderPlot({
    if(is.null(model_cvridge())){ return()}
    RidgePlot(model_cvridge())
  })
  
  #******* validacion
  #coeficientes del modelo
  thetaPredict_ridge <- function(fit,x){ cbind(1,x)%*%coef(fit) }
  
  #tipo de validacion para el modelo
  validation_ridge <- reactive({
    tryCatch({
      if (is.null(input$validationType_ridge) || is.null(model_ridge()))
        return()
      if(input$validationType_ridge == '2' && is.null(input$fileTest_ridge))
        return()
      closeAlert(session,"alertValidationID")
      switch(input$validationType_ridge,
             '1' = crossValidation(model_ridge(), thetaPredict_ridge, DATA_SET$data[,input$ridge_response], predictores_ridge()), # ten-fold cross validation funcion en regresion.r
             '2' = { inFile <- input$fileTest_lm
                     Prediction <- scale(read.csv(inFile$datapath),center = F, scale = model_ridge()$scales)%*% model_ridge()$coef
                     response_variable <- DATA_SET$data[,input$ridge_response]
                     data.frame(cbind(Prediction, response_variable))
                   },
             '3' = { Prediction <- scale(predictores_ridge()[-train_ridge(), ],center = F, scale = model_ridge()$scales)%*% model_ridge()$coef
                     response_variable <- DATA_SET$data[-train_ridge(),input$ridge_response]
                     data.frame(cbind(Prediction, response_variable))
                   }
      )
    }, error = function(e) {
      createAlert(session, "alertValidation", "alertValidationID", title = titleAlert,
                  content = paste("",e), style = "warning")
    })
  })
  #resultado de la validacion
  output$resultValidation_ridge <- renderPrint({
    if(!is.null(validation_ridge()))
      validation_ridge()
  })
  
  #resultado grafico de la prediccion realizada
  output$plotValitation_ridge <- renderPlot({
    if(is.null(validation_ridge())){ return()}
    if(input$validationType_ridge =='3'){
      simplePlot(validation_ridge(),  DATA_SET$data[-train_ridge(),input$ridge_response], 2, 1, input$ridge_response, 2, 0.9)
    }else{
      simplePlot(validation_ridge(),  DATA_SET$data[,input$ridge_response], 2, 1, input$ridge_response, 2, 0.9)
    }
  })
  
  #-----------------------> rglm
  #seleccion de la variable dependiente
  output$select_rglm <- renderUI({
    if(is.null(DATA_SET$data)){return()}
    select_model("rglm_response", "rglm_predictors", DATA_SET$data)
  })
  
  #obtengo los predictores del modelo
  predictores_rglm <-  reactive({
    if(is.null(DATA_SET$data)){return()}
    data.frame(predictors(DATA_SET$data, input$rglm_predictors, input$rglm_response))
  })
  
  #particion en porcentaje de train y test
  train_rglm <- reactive({
    if(is.null(input$porcentTest_rglm) || is.null(DATA_SET$data)){ return()}
    dataPartition(predictores_rglm(), input$porcentTest_rglm)
  })
  
  #aplicando el modelo rgml
  model_rglm <- reactive({
    if(is.null(DATA_SET$data)){return()}
    tryCatch({
      if(anyNA(DATA_SET$data)){
        createAlert(session, "alertRGLM", "alertRGLMID", title = titleAlert,
                    content = "Data set have missing values", style = "warning")
      }else{
        closeAlert(session, "alertRGLMID")
        #se aplica el modelo dependiendo del tipo de validacion seleccionada
        if (is.null(input$validationType_rglm)){return()}
        if(input$validationType_rglm == '2' && is.null(input$fileTest_rglm))
          return()
        else{
          switch (input$validationType_rglm,
                  '1' = randomGLM(predictores_rglm(), DATA_SET$data[,input$rglm_response], 
                                  nCandidateCovariates=ncol(predictores_rglm()), 
                                  nBags=10, keepModels = TRUE, nThreads = 1),
                  '2' = { inFile <- input$fileTest_lm
                          randomGLM(predictores_rglm(), DATA_SET$data[,input$rglm_response], read.csv(inFile$datapath),
                                  nCandidateCovariates=ncol(predictores_rglm()), 
                                  nBags=1, keepModels = TRUE, nThreads = 1)},
                  '3' = {randomGLM(predictores_rglm()[train_rglm(), ], DATA_SET$data[train_rglm(),input$rglm_response], predictores_rglm()[-train_rglm(),],
                                  nCandidateCovariates=ncol(predictores_rglm()), nBags=1, keepModels = TRUE, nThreads = 1)}
          )
        }
      }
    }, error = function(e) {
      createAlert(session, "alertRGLM", "alertRGLMID", title = titleAlert,
                  content = paste("",e), style = "warning")
    })
  })
  
  #Resultado obtenido tras aplicar el  modelo
  output$result_rglm <- renderPrint({
    if(is.null(model_rglm())){ return()}
    summary(model_rglm())
  })
  
  #******* validacion
  #coeficientes del modelo
  thetaPredict_rglm <- function(fit,x){ cbind(1,x)%*%coef(fit) }
  
  #tipo de validacion para el modelo
  validation_rglm <- reactive({
    tryCatch({
      if (is.null(input$validationType_rglm) || is.null(model_rglm))
        return()
      if(input$validationType_rglm == '2' && is.null(input$fileTest_rglm))
        return()
      
      Prediction <- model_rglm()$predictedOOB
      if(input$validationType_rglm == '3'){
        (response_variable <- DATA_SET$data[-train_rglm(),input$rglm_response])
      }else{
        (response_variable <- DATA_SET$data[,input$rglm_response])
      }
      data.frame(cbind(Prediction, response_variable))

    }, error = function(e) {
      print(e)
    })
  })
  #resultado de la validacion
  output$resultValidation_rglm <- renderPrint({
    if(!is.null(validation_rglm()))
      na.omit(validation_rglm())
  })
  
  #resultado grafico de la prediccion realizada
  output$plotValitation_rglm <- renderPlot({
    if(is.null(validation_rglm())){ return()}
    if(input$validationType_rglm == '3'){
      simplePlot(validation_rglm(),  DATA_SET$data[train_rglm(),input$rglm_response], 2, 1, input$rglm_response, 2, 0.9)
    }else{
      simplePlot(validation_rglm(),  DATA_SET$data[,input$rglm_response], 2, 1, input$rglm_response, 2, 0.9)
    }
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
  
  #-------------------------------------------------------
  #-----------------------> report <-----------------------
  
  #Acciones realizadas en el item Data
  output$data_report <- renderUI({
    if(is.null(DATA_SET$data) || is.null(input$dataSetEdit)){return()}
    # data <- paste("The data set selected:", DATA_SET$name,
    #          "Original observations",DATA_SET$original.obs, 
    #          "and original variables",DATA_SET$original.var)
    # editdata <- if(input$dataSetEdit == 1 & !is.null(input$varDelete)){
    #               # if(input$deleteCol){
    #                 paste("Edit data set with 'Delete column'", 
    #                 "Column deleted:",input$varDelete)
    #               #}
    #             }else if(input$dataSetEdit == 2){"Edit data set with 'Delete nominal values'"}
    # paste(data, editdata)
    column(12,
      p(style = "text-align:justify;","The data set selected:", DATA_SET$name),
      p(style = "text-align:justify;","Original observations",DATA_SET$original.obs, 
       "and original variables",DATA_SET$original.var),
      if(input$dataSetEdit == '1' & !is.null(input$varDelete)){
         if(!is.null(input$deleteCol)){
            if(input$deleteCol > 0){
        p(style = "text-align:justify;","Edit data set with 'Delete column'",
          "Column deleted:",input$varDelete)}
        }
      },
      if(input$dataSetEdit == '2'){p(style = "text-align:justify;","Edit data set with 'Delete nominal values'")}
    )
  })
  
  #Acciones realizadas en el item Preprocessing
  #Acciones realizadas en el item Transformation
  #Acciones realizadas en el item Regression
  #Acciones realizadas en el item Linear model evaluation
  
}