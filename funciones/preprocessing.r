
#tabs de la vista de datos faltantes, resibe como parametros el titulo del contenedor y los tabs 
tabsMissingValues <- function(title, tab1, tab2, tab3) {
  fluidRow(
    column(width = 12,
           #BreadCrumds de Missing values     
           HTML('
                <ul class="breadcrumb">
                <li>Preprocessing</li>
                <li>Missing Values</li>
                </ul>'),
            box(width = 12, title ="",
                tags$div( class = 'col-sm-2',
                          actionButton("deleteMS", label = "Delete MS",href="")
                 )
#                 tags$div( class = 'col-sm-2',
#                           actionButton("save", label = "Save chages",href="")
#                 ),
#                 tags$div( class = 'col-sm-2',
#                           actionButton("cancel", label = "Cancel",href="")
#                 )
            ),
           tabBox(
             title = title,
             width = 12,
             id = "tabset2",
             tabPanel(tab1,
                      #opcion1 de visualizacion
                      tab_grafics("missing1", tools_general_grafics("radio", "note", "save", "cancel",
                                                                    "download", uiOutput("slider_range_range_amelia"),NULL))
             ),
             tabPanel(tab2,
                      #opcion2 de visualizacion
                      tab_grafics("missing2", tools_general_grafics("radio2", "note2", "save2", "cancel2",
                                                                    "download2", uiOutput("slider_range_range_option1"), NULL))
             ),
             tabPanel(tab3,
                      #opcion3 de visualizacion
                      tab_grafics("missing3", tools_general_grafics("radio3", "note3", "save3", "cancel3",
                                                                    "download3", uiOutput("slider_range_range_option2"), NULL))
             )
           ) 
    )
  )
}

#Vista correspondiente a eliminacion de ruido
noiseRemoval <- function(title){
  fluidRow(
    column(width = 12,
           #BreadCrumds de eliminación de ruido    
           HTML('
                <ul class="breadcrumb">
                <li>Preprocessing</li>
                <li>Noise removal</li>
                </ul>')
           ),
    tab_grafics("", tools_general_grafics("radio5", "note5", "save5", "cancel5",
                                          "download5", uiOutput("slider_range_range_nremoval"), NULL))
  )
  
}

tabsOutlier <- function(title, tab1, tab2, tab3, tab4) {
  fluidRow(
    column(width = 12,
           #BreadCrumds de outlier    
           HTML('
                <ul class="breadcrumb">
                <li>Analysis</li>
                <li>Outlier detection</li>
                </ul>'),
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
tools_general_grafics <- function(radio, note, save, cancel, download, slider_type, slider_type2){
  fluidRow(
    #Tipo de slider correspondiente (con rango o sin rango)
    slider_type,
    slider_type2,
    box(
      title = "Download image", width = 4, solidHeader = TRUE, status = "success",
      #background = "orange",
      radioButtons(radio,NULL,
                   choices = list("PNG" = 1, "SVG" = 2, "PDF" = 3), 
                   selected = 1),
      downloadButton(download, "Download")
    ),
    #box para los apuntes
    box(
      title = "Notes", width = 4, solidHeader = TRUE, status = "success",
      #background = "yellow",
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
               plotOutput(plot)
           ),
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
