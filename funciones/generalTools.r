
#Funcion contenedora de las herramientas de un grafico que contenga atributos y observaciones
#Recive como parametros los nombres de cada una de lass herramientas para cada una de las vistas
tools_general_grafics <- function(radio, note, save, cancel, download, slider_type, slider_type2){
  fluidRow(
    column(width = 12,
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

#funcion que retorna la vista de la paleta de colores
colors <- function(id1, id2, id3){
  fluidRow(
    column(width = 4,
           colourInput(id1, "Min color", "darkred")),
    column(width = 4,
           colourInput(id2, "Avg color", "yellow")),
    column(width = 4,
           colourInput(id3, "Max color", "darkgreen"))
  )
}
