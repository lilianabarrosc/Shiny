
#tabs de la vista de visualización, resibe como parametros el titulo del contenedor y los tabs 
tabsVisualization <- function(title, tab1, tab2) {
  fluidRow(
    column(width = 12,
      #BreadCrumds, archivo que contiene el html     
      includeHTML("html/breadcrumbs.html"),
       tabBox(
         title = title,
         width = 12,
         id = "tabset1",
         tabPanel(tab1,
                  #contenido del tab1 = Scatter plot 1
                  tab_grafics("scatter1",  tools_general_grafics("radio0", "note0", "save0", "cancel0",
                                                                 "download0", 
                                                                 #slider_range_int("x1","y1","z1")
                                                                 uiOutput("slider_range_range_density")
                                                                 ))
         ),
         tabPanel(tab2, 
                  #contenido del tab2 = parallel grafics
                  tab_grafics("parallel", tools_general_grafics("radio1", "note1", "save1", "cancel1",
                                                        "download1", uiOutput("slider_range_range_parallel")))
         )
       ) 
    )
  )
}

#tabs de la vista de datos faltantes, resibe como parametros el titulo del contenedor y los tabs 
tabsMissingValues <- function(title, tab1, tab2, tab3) {
  fluidRow(
    column(width = 12,
           tabBox(
             title = title,
             width = 12,
             id = "tabset2",
             tabPanel(tab1,
                      #opcion1 de visualizacion
                      tab_grafics("missing1", tools_general_grafics("radio", "note", "save", "cancel",
                                                                    "download", uiOutput("slider_range_range_amelia")))
             ),
             tabPanel(tab2,
                      #opcion2 de visualizacion
                      tab_grafics("missing2", tools_general_grafics("radio2", "note2", "save2", "cancel2",
                                                                    "download2", uiOutput("slider_range_range_option1")))
             ),
             tabPanel(tab3,
                      #opcion3 de visualizacion
                      tab_grafics("missing3", tools_general_grafics("radio3", "note3", "save3", "cancel3",
                                                                    "download3", uiOutput("slider_range_range_option2")))
             )
           ) 
    ),
    column(width = 12,
           box(width = 12, title ="Sumary", solidHeader = TRUE,
             tags$div( class = 'col-sm-2',
                       actionButton("deleteMS", label = "Delete MS",href="")
                       ),
             tags$div( class = 'col-sm-2',
                       actionButton("save", label = "Save chages",href="")
             ),
             tags$div( class = 'col-sm-2',
                       actionButton("cancel", label = "Cancel",href="")
             )
           )
    )
    
  )
}

#vista correspondiente a la normalizacion del data set, recibe como parámetro el titulo de la vista
normalizations <- function(title){
  fluidRow(
    box( width = 12, title = "Normalization", solidHeader = TRUE, status = "primary",
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
#     box(width = 6, title ="Original data", solidHeader = TRUE,
#         verbatimTextOutput("original_data")
#         #dataTableOutput(outputId="original_data")
#     ),
#     box(width = 6, title ="Normalized data", solidHeader = TRUE,
#         verbatimTextOutput("normalized_data")
#         #dataTableOutput(outputId="normalized_data")
#     ),
#     box(width = 12, title ="Sumary", solidHeader = TRUE,
#         verbatimTextOutput("summary_normalization")
#         #dataTableOutput(outputId="summary_normalization")
#     )
  )
}

#tabs de la vista de reduccion de la dimensionalidad, resibe como parametros el titulo del contenedor y los tabs 
tabsDimensionalityReduction <- function(title, tab1, tab2, tab3, tab4) {
  fluidRow(
    column(width = 12,
           tabBox(
             title = title,
             width = 12,
             id = "tabset2",
             tabPanel(tab1,
                      #PCA
                      tab_grafics("pca", tools_general_grafics("radio4", "note4", "save4", "cancel4",
                                                                    "download4", uiOutput("slider_range_range_pca"))),
                      verbatimTextOutput("summary_pcs")
             ),
             tabPanel(tab2,
                      #SVD
                      tab_grafics("svd", verbatimTextOutput("s"))
             ),
             tabPanel(tab3, "Working..."
                      #test de colinealidad
                   
             ),
             tabPanel(tab4, "Working..."
                      #Seleccion de atributos
                      
             )
           ) 
    )
  )
}

tabsOutlier <- function(title, tab1, tab2, tab3, tab4) {
  fluidRow(
    column(width = 12,
           tabBox(
             title = title,
             width = 12,
             id = "tabset2",
             tabPanel(tab1,
                      #residual vs fitted
                      imageOutput("rsidualFitted")
             ),
             tabPanel(tab2,
                      #scale-location
                      imageOutput("sacaleLocation")
             ),
             tabPanel(tab3, 
                      #normal Q-Q
                      imageOutput("normalQQ")
             ),
             tabPanel(tab4,
                      #residual vs leverage
                      imageOutput("residualLeverage")
             )
           ) 
    )
  )
}

#--------------------------
#strtoi("att")
#Tanto atributos como observaciones son un rango
# slider_range_range <- function(x,y){
#   box(
#     title = "Range", width = 6, solidHeader = TRUE,
#     background = "aqua",
#     sliderInput(x, label = "Atributes", min = 1, 
#                 max = dim(airquality)[2], value = c(1, dim(airquality)[2])),
#     sliderInput(y, label = "Observation", min = 1, 
#                 max = dim(airquality)[1], value = c(1, dim(airquality)[1]))
#   )
# }
# 
# #X representa atributos en un rango e Y la variable a comparar
# slider_range_int <- function(x,y,z){
#   box(
#     title = "Range", width = 6, solidHeader = TRUE,
#     background = "aqua",
#     sliderInput(x, label = "X", min = 1, 
#                 max = 5, value = c(1,4)),
#     sliderInput(y, label = "Y", min = 1, 
#                 max = 5, value = 2),
#     sliderInput(z, label = "Observations", min = 1,
#                 max = 60, value = c(1,20))
#   )
# }

#Funcion contenedora de las herramientas de un grafico que contenga atributos y observaciones
#Recive como parametros los nombres de cada una de lass herramientas para cada una de las vistas
tools_general_grafics <- function(radio, note, save, cancel, download, slider_type){
  fluidRow(
    #Tipo de slider correspondiente (con rango o sin rango)
    slider_type,  
    box(
      title = "Download image", width = 4, solidHeader = TRUE,
      background = "blue",
      radioButtons(radio,NULL,
                   choices = list("PNG" = 1, "SVG" = 2, "PDF" = 3), 
                   selected = 1),
      downloadButton(download, "Download")
    ),
    #box para los apuntes
    box(
      title = "Notes", width = 4, solidHeader = TRUE,
      background = "light-blue",
      tags$div( class='form-group shiny-input-container',
                tags$textarea("notes...", class="form-control shiny-bound-input", style="resize: none")
      )
    )
  )
}

#contenedor con dos box, uno para el grafico y otro para las opciones
tab_grafics <- function(plot, options){
  fluidRow(
    column(width = 12,
           box(width = 12,
               title = NULL, status = "primary",
               plotOutput(plot)
           )),
    column(width = 12,
           #box contenedor de opciones para el grafico
           box( width = 12, title = "Options", solidHeader = TRUE,
                collapsible = TRUE,
                #herramientas del grafico
                options
           )
    )
  )
}
