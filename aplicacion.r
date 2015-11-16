#install.packages('shiny')
library('shiny')
library('shinydashboard')
library('RegressionLibs')
library('plotly')
library('Amelia')
library('VIM')
library('ggbiplot')

source('opcionesDashboard.r')
source('analisisExploratorio.r')
source('data.r')

#variable global que contendra el nombre de los archivos de la bd
data_sets <- list("iris" = 1, "airquality" = 2, "new" = 3)

#Cuerpo de la página
body <- dashboardBody(
  tabItems( 
    #Tab del home
    tabItem(tabName = "home",
            h2("Home")
          ),
    #Tab del data
    tabItem(tabName = "data",
            viewData()
    ),
    #Inicion tabs Analisis exploratorio
    tabItem(tabName = "visualization",
            tabsVisualization("visualization", "Scatter plot1", "Scatter plot2")
    ),
    tabItem(tabName = "mvalues",
            tabsMissingValues("Missing values", "Plot 1", "Plot 2","Plot 3")
    ),
    tabItem(tabName = "nremoval",
            h2("Noise removal")
    ),
    tabItem(tabName = "normalization",
            normalizations("Normalization")
    ),
    tabItem(tabName = "dreduccion",
            tabsDimensionalityReduction("Dimensionality reduccion", "PCA", "SVD", 
                                        "Colinearity test", "Attribute selection")
    ),
    tabItem(tabName = "odetection",
            h2("Outlier detection")
    )
    #Fin tabs Analisis exploratorio
  )
)
#--------------------Cliente-------------------
#head() y sidebar() son funciones contenidas en el archivo opcionesDashboard.r
ui <- dashboardPage(head(), sidebar(), body)

#--------------------Servidor-------------------

server <- function(input, output, session) {
  
  # -------------> Lectura de archivo
  output$contents <- renderDataTable(
    
    #inFile <- input$file1
    
    #if (is.null(inFile))
     # return(NULL)
    
    #read.csv(inFile$datapath, header=TRUE, sep=',', quote = '"')
    datatable(iris, colnames = c('Data set', 'Size', 'Date', 'Dimensions', 'Actions'))
  )
  
  #----------> data set
  
  output$ui <- renderUI({
    if (is.null(input$select_file))
      return()
    switch(input$select_file,
           '3' = fileInput('file1', 'Choose CSV File',
                         accept=c('text/csv', 
                                  'text/comma-separated-values,text/plain', 
                                  '.csv'))
    )
  })
  
  #Seleccion de data set a utilizar
  file <- reactive({
    if (is.null(input$select_file))
      return()
    switch(input$select_file,
           '1'= iris,
           '2'= airquality,
           '3'= input$file1
    )
  })
  
  #----------> dimensionalidad del archivo
  #Con dim puedo saber la cantidad de atributos y observaciones que posee el archivo,
  #en dim(data)[1] se pueden encontrar la cantidad de observaciones y en dim(data)[2] la 

  #*************************************************
  #----------> Graficos de visualizacion
  
  #Slider visualizacion grafico de densidad
  output$slider_range_range_density <- renderUI({
    box(
      title = "Range", width = 6, solidHeader = TRUE,
      background = "aqua",
      sliderInput("x1", label = "X", min = 1, 
                  max = dim(file())[2], value = c(1, dim(file())[2])),
      sliderInput("y1", label = "Y", min = 1, 
                  max = dim(file())[2], value = 2),
      sliderInput("z1", label = "Observations", min = 1, 
                  max = dim(file())[1], value = c(1, dim(file())[1]))
    )
  })
  
  #seleccion de atributos y observaciones del data set
  dat1 <- reactive({
    file()[input$z1[1]:input$z1[2],input$x1[1]:input$x1[2]]
  })
  
  #Grafico correspondiente a scatterPlot con un grafico de densidad 
  output$scatter1 <- renderPlot({
    ir.pca <- prcomp(dat1(), center = TRUE, scale. = TRUE)
    ScatterplotMatrix(ir.pca, input$x1[1], input$x1[2], file()[,input$y1], names(file())[[input$y1]])
  })
  
  #Slider visualizacion grafico de histograma
  output$slider_range_range_hitograma <- renderUI({
    box(
      title = "Range", width = 6, solidHeader = TRUE,
      background = "aqua",
      sliderInput("x2", label = "X", min = 1, 
                  max = dim(file())[2], value = c(1, dim(file())[2])),
      sliderInput("y2", label = "Y", min = 1, 
                  max = dim(file())[2], value = 2),
      sliderInput("z2", label = "Observations", min = 1, 
                  max = dim(file())[1], value = c(1, dim(file())[1]))
    )
  })
#   
#   #seleccion de atributos y observaciones del data set
#   dat2 <- reactive({
#     file()[input$z2[1]:input$z2[2],input$x2[1]:input$x2[2]]
#   })
  
  #*********************************************
  #---------------> Graficos correspondientes a missing values
 
   #Slider visualizacion grafico de missing values Amelia
  output$slider_range_range_amelia <- renderUI({
    box(
      title = "Range", width = 6, solidHeader = TRUE,
      background = "aqua",
      sliderInput("attributes", label = "Attributes", min = 1, 
                  max = dim(file())[2], value = c(1, dim(file())[2])),
      sliderInput("observation", label = "Observation", min = 1, 
                  max = dim(file())[1], value = c(1, (dim(file())[1])/2))
    )
  })
  
  #Obtengo la seleccion de atributos y observaciones para la Opcion 1
  selectedData1 <- reactive({
    file()[input$observation[1]:input$observation[2], 
               input$attributes[1]:input$attributes[2]]
  })
  
  #Opcion 1 (libreria Amelia)
  output$missing1 <- renderPlot({
    missmap(selectedData1(), main = "Missing values vs observed")
  })
  
#   #---------descarga del grafico opcion 1
# #   downloadInput <- reactive({
# #     switch(input$radio,
# #            '1' = '.png',
# #            '2' = '.svg',
# #            '3' = '.pdf')
# #   })
#   
  #Slider visualizacion grafico de missing VIM option1
  output$slider_range_range_option1 <- renderUI({
    box(
      title = "Range", width = 6, solidHeader = TRUE,
      background = "aqua",
      sliderInput("attributes2", label = "Attributes", min = 1, 
                  max = dim(file())[2], value = c(1, dim(file())[2])),
      sliderInput("observation2", label = "Observation", min = 1, 
                  max = dim(file())[1], value = c(1, (dim(file())[1])/2))
    )
  })
  
  #Obtengo la seleccion de atributos y observaciones para la Opcion 2
  selectedData2 <- reactive({
    file()[input$observation2[1]:input$observation2[2], 
           input$attributes2[1]:input$attributes2[2]]
  })
  
  #Opcion 2 (libreria VIM)
  output$missing2 <- renderPlot({
    aggr_plot <- aggr(selectedData2(), col=c('red','dark grey'), numbers=TRUE, 
                      sortVars=TRUE, labels=names(data), cex.axis=.8, gap=1, 
                      ylab=c("Histogram of missing data","Pattern"))
  })
  
  #Slider visualizacion grafico de missing VIM option2
  output$slider_range_range_option2 <- renderUI({
    box(
      title = "Range", width = 6, solidHeader = TRUE,
      background = "aqua",
      sliderInput("x3", label = "X", min = 1, 
                  max = dim(file())[2], value = c(1, dim(file())[2])),
      sliderInput("y3", label = "Y", min = 1, 
                  max = dim(file())[2], value = 2),
      sliderInput("z3", label = "Observations", min = 1, 
                  max = dim(file())[1], value = c(1, dim(file())[1]))
    )
  })
  
  #Obtengo la seleccion de atributos a comparar para la Opcion 3
  dat3 <- reactive({
    file()[input$z3[1]:input$z3[2],input$x3[1]:input$x3[2]]
  })
  
  #Option 3 (matricial)
  output$missing3 <- renderPlot({
    scattmatrixMiss(dat3(), interactive = F, highlight = c(names(file())[[input$y3]]))
  })
  
  #************************************************
  #-------------> Normalization
  #salida dinamica de rango para normalizacion
  output$range <- renderUI({
    if (is.null(input$normalizationType))
      return()
    switch(input$normalizationType,
           '2' =  tags$div( class = 'col-sm-8',
                            tags$div( class = 'col-sm-2',
                                      numericInput("min", label = "Min", value = 0)
                            ),
                            tags$div( class = 'col-sm-2',
                                      numericInput("max", label = "Max", value = 1)
                            )
           )
    )
  })
  
  #obtengo el tipo de normalizacion seleccionada y aplico la normalizacion correspondiente
  normalization_type <- reactive({
    if (is.null(input$normalizationType))
      return()
    switch(input$normalizationType,
           '1'= normalizeData(iris[,1:4], 1, 5),
           '2'= normalizeData(iris[,1:4], input$min, input$max)
    )
  })
  
  #muestro un sumary
  output$value <- renderPrint({
    summary(normalization_type())
  })
  
  
  #muestro os primeros 10 atributos del data set
  output$value2 <- renderPrint({
    normalization_type()[1:10,]
  })
}

#App
shinyApp(ui, server)

