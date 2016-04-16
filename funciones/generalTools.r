
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
             downloadButton(download, "Download", class = "btn-success")
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

#funcion que retorna el archivo que se va a descargar
downloadGeneral <- function(option, plot){
  switch (option,
          "1" = downloadHandler(
              filename = "guiniaPNG.png", #nombre de la imagen a descargar
              content = function(file) {
                png(file)
                print(plot)
                dev.off()
              }
          ),
          "2" = downloadHandler(
              filename = "guiniaSVG.svg", #nombre de la imagen a descargar
              content = function(file) {
                svg(file)
                print(plot)
                dev.off()
              }
          ),
          "3" = downloadHandler(
              filename = "guiniaPDF.pdf", #nombre del pdf a descargar
              content = function(file) {
                pdf(file = file, width=12, height=8)
                print(plot)
                dev.off()
              }
          )
 )
}

#funcion contenedora de dos Slider
twoSlider <- function(x, y, file, xname, yname){
  box(
    width = 6, status = "success",
    h4("Range"),
    sliderInput(x, label = xname, min = 1, 
                max = dim(file)[2], value = c(1, 4)),
    sliderInput(y, label = yname, min = 1, 
                max = dim(file)[1], value = c(1, (dim(file)[1])/2))
  )
}

#funcion contenedora de tres slider
treeSlider <- function(x, y, z, file, xname, yname, zname){
  box(
    width = 6, status = "success",
    h4("Range"),
    sliderInput(x, label = xname, min = 1, 
                max = dim(file)[2], value = c(1,4)),
    sliderInput(y, label = yname, min = 1, 
                max = dim(file)[2], value = 3),
    sliderInput(z, label = zname, min = 1, 
                max = dim(file)[1], value = c(1, dim(file)[1]))
  )
}

slider_modelSimple <- function(x,data){
  renderUI({
    numVariables <- dim(data)[2]
    #predictors <- reduceDimensionality()[, !names(reduceDimensionality()) %in% input$pls_response]
    namesVariables <- names(data)
    selectInput(x, label = h4("Response variable"), 
                choices = namesVariables, selected = names(data)[numVariables])
  })
}

slider_modelMultiple <- function(y, data){
  renderUI({
    selectInput(y, label = h4("Predictor variables"), 
                choices = names(data), multiple = TRUE)
  })
}