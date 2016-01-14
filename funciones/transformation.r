source('funciones/preprocessing.r')

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
         uiOutput("range")
    ),
    tabBox(width = 12,
           tabPanel("Original data", "*First ten observations", verbatimTextOutput("original_data")),
           tabPanel("Normalized data", "*First ten observations", verbatimTextOutput("normalized_data")),
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
                </ul>')
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
                tools_general_grafics("radio4", "note4", "save4", "cancel4",
                                      "download4", uiOutput("slider_range_range_pca"),
                                      box(width = 6, status = "warning",
                                          actionButton("reduceDim", label = "Reduce Dimensionality"),
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

# tabsDimensionalityReduction <- function(title, tab1, tab2, tab3, tab4) {
#   fluidRow(
#     column(width = 12,
#            #BreadCrumds de reduccion de la dimencionalidad     
#            HTML('
#                 <ul class="breadcrumb">
#                 <li>Analysis</li>
#                 <li>Dimensionality reduction</li>
#                 </ul>'),
#            tabBox(
#              title = title,
#              width = 12,
#              id = "tabset2",
#              tabPanel(tab1,
#                       #PCA
#                       fluidRow(
#                         column(width = 12,
#                                plotOutput("pca")
#                         ),
#                         box(width = 12, status = "success",
#                             #texto con el summary
#                             verbatimTextOutput("summary_pcs")
#                         ),
#                         column(width = 12,
#                                #box contenedor de opciones para el grafico
#                                box( width = 12, title = "Options", solidHeader = TRUE,
#                                     collapsible = TRUE,
#                                     #herramientas del grafico
#                                     tools_general_grafics("radio4", "note4", "save4", "cancel4",
#                                                           "download4", uiOutput("slider_range_range_pca"),
#                                                           box(width = 6, status = "warning",
#                                                               actionButton("reduceDim", label = "Reduce Dimensionality"),
#                                                               hr(),
#                                                               verbatimTextOutput("summary_reduceDimensionality")
#                                                           ))
#                                )
#                         )
#                       )
#              ),
#              tabPanel(tab2,
#                       #SVD
#                       tab_grafics("svd", verbatimTextOutput("s"))
#              ),
#              tabPanel(tab3, "Working..."
#                       #test de colinealidad
#                       
#              ),
#              tabPanel(tab4, "Working..."
#                       #Seleccion de atributos
#                       
#              )
#            )
#            )
#   )
# }