source('funciones/generalTools.r')

#vista correspondiente a la normalizacion del data set, recibe como parametro el titulo de la vista
normalizations <- function(title){
  fluidRow(
    column(width = 12,
           #BreadCrumds de normalizacion    
           HTML('
                <ul class="breadcrumb">
                <li>Transformation</li>
                <li>Normalization</li>
                </ul>'),
           bsAlert("alertNormalization")
           ),
    box( width = 12, title = "Normalization", solidHeader = TRUE, status = "success",
         tags$div( class = 'col-sm-4',
                   radioButtons("normalizationType", label = "Type", selected = 1,
                                choices = list("Normalization in range" = 1, "Normalization 0-1" = 2,
                                               "Other normalization" = 3)
                   )
         ),
         uiOutput("range_normalization")
    ),
    tabBox(width = 12,
           tabPanel("Normalized data", "*First ten observations", verbatimTextOutput("normalized_data")),
           tabPanel("Sumary",verbatimTextOutput("summary_normalization")),
           tabPanel("Original data", "*First ten observations", verbatimTextOutput("original_data"))
    ),
    box(width = 12, status = "success",
        bsButton("apply_normalization", label = "Apply", style = "success")
    )
  )
}

# vista correspondiente al PCA
pca <- function(title){
  fluidRow(
    column(width = 12,
           #BreadCrumds de pca    
           HTML('
                <ul class="breadcrumb">
                <li>Transformation</li>
                <li>PCA</li>
                </ul>'),
           bsAlert("alertPCA")
    ),
    box(width = 12, title = "PCA", solidHeader = TRUE, status = "success",
           plotOutput("pca"),
           hr()
    ),
    box(width = 12, status = "success",
        #texto con el summary
        verbatimTextOutput("summary_pcs")
    ),
    column(width = 12,
           #box contenedor de opciones para el grafico
           box( width = 12, title = "Options", solidHeader = TRUE,
                collapsible = TRUE,
                #herramientas del grafico
                tools_general_grafics("radio_pca", "note4", "save4", "cancel4",
                                      "download_pcaPlot", uiOutput("slider_pca"),
                                      box(width = 6, status = "success",
                                          uiOutput("responsePCA"),
                                          bsButton("reduceDim", label = "Reduce Dimensionality", style = "success"),
                                          hr(),
                                          uiOutput("summary_reduceDimensionality")
                                      ))
           )
    )
  )
}

# vista correspondiente al svd
svd2 <- function(title){
  fluidRow(
    column(width = 12,
           #BreadCrumds svd    
           HTML('
                <ul class="breadcrumb">
                <li>Transformation</li>
                <li>SVD</li>
                </ul>'),
           bsAlert("alertSVD"),
           tab_grafics("svd", verbatimTextOutput("s")) #funcion de visualizacion (generalTools.r)
           )
    
  )
   
}

#vista correspondiente al seleccion de atributos
attributeSelection <- function(){
  fluidRow(
    column(width = 12,
           #BreadCrumds de pca    
           HTML('
                <ul class="breadcrumb">
                  <li>Transformation</li>
                  <li>Attribute Selection</li>
                </ul>'),
           bsAlert("alertAttributeS")
           ),
    box( width = 12, title = "Attribute Selection", solidHeader = TRUE, status = "success",
        column(12,
               uiOutput("option_attributeS")
               ),
        column(6,
               h4("Scores"),
               verbatimTextOutput("print_weights")
              ),
        column(6,
               h4("Attributes selected"),
               verbatimTextOutput("print_subset"),
               bsButton("apply_attributeS", label = "Apply", style = "success"),
               hr(),
               uiOutput("print_dataAtributte")
              )
      )
  )
}

#Funcion que devuelve una lista con los tipos de normalizacion
type_normalization <- function(){
  return(list("without normalization" = "n0", 
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
       "normalization with zero being the central point ((x-midrange)/(range/2))" = "n13"))
}

CumulativeVariances_plot <- function(data.pca){
  #grafico en ggplot
  rowsData <- length(data.pca$sdev)
  seqRow <- seq(from = 1, to = rowsData, length.out = rowsData)
  
  dataPlot <- data.frame(seqRow, data.pca$sdev)
  names(dataPlot) <- c("PCA", "Variances")
  if (nrow(dataPlot)>10) {
    dataPlot <- dataPlot[1:10,]
  }
  dataPlot <- CalculateVariance(dataPlot, 2)
  dataPlot[,2] <- cumsum(dataPlot[,2]) 
  
  p2 <- ggplot(data = dataPlot, aes(x = PCA, y = Variances, group = 1)) +
    geom_line(colour = "dodgerblue4", alpha = 0.5, size = 1) +
    geom_point(colour = "dodgerblue4", size = 2, alpha = 0.5) +
    expand_limits(y = 0) +
    xlab("PCs") + ylab("Cumulative variances") +
    scale_x_continuous(breaks = dataPlot$PCA) +
    theme(panel.grid.minor = element_blank(), #remove gridlines
          legend.position = "bottom" #legend at the bottom
    )
  
  return(p2)
}