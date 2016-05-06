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
                </ul>')
           ),
    box( width = 12, title = "Normalization", solidHeader = TRUE, status = "success",
         tags$div( class = 'col-sm-4',
                   radioButtons("normalizationType", label = "Type", selected = 1,
                                choices = list("Scale Standardization" = 1, "Normalization 0-1" = 2,
                                               "Other normalization" = 3)
                   )
         ),
         uiOutput("range_normalization")
    ),
    tabBox(width = 12,
           tabPanel("Normalized data", "*First ten observations", verbatimTextOutput("normalized_data")),
           tabPanel("Original data", "*First ten observations", verbatimTextOutput("original_data")),
           tabPanel("Sumary",verbatimTextOutput("summary_normalization"))
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
    column(width = 12,
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
                                          bsButton("reduceDim", label = "Reduce Dimensionality", style = "success"),
                                          hr(),
                                          verbatimTextOutput("summary_reduceDimensionality")
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
           tab_grafics("svd", verbatimTextOutput("s"))
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
        uiOutput("option_attributeS"),
        tags$div( class = 'col-sm-2', bsButton("apply_attributeS", label = "Apply", style = "success")),
        column(6,
               h4("Scores"),
               verbatimTextOutput("print_weights")
              ),
        column(6,
               h4("Attributes selected"),
               verbatimTextOutput("print_subset")
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
