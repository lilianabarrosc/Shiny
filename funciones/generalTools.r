
#Funcion contenedora de las herramientas de un grafico que contenga atributos y observaciones
#Recive como parametros los nombres de cada una de lass herramientas para cada una de las vistas
tools_general_grafics <- function(radio, note, save, cancel, download, slider_type, slider_type2){
  fluidRow(
    column(width = 12,
           #Tipo de slider correspondiente (con rango o sin rango)
           slider_type,
           slider_type2,
           downloadPlot(radio, download)
           # #box para los apuntes
           # box(
           #   title = "Notes", width = 4, solidHeader = TRUE, status = "success",
           #   #background = "yellow",
           #   tags$div( class='form-group shiny-input-container',
           #             tags$textarea("notes...", class="form-control shiny-bound-input", style="resize: none")
           #   )
           # )
      )
  )
}

#funcion que retorna a vista para descargar un grafico
downloadPlot <- function(idradio, iddownload){
  box(
    title = "Download image", width = 6, solidHeader = TRUE, status = "success",
    #background = "orange",
    radioButtons(idradio,NULL,
                 choices = list("PNG" = 1, "SVG" = 2, "PDF" = 3), 
                 selected = 1),
    downloadButton(iddownload, "Download", class = "btn-success")
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
downloadGeneral <- function(option, plot, name){
  switch (option,
          "1" = downloadHandler(
              filename = paste(name, '.png', sep=''), #nombre de la imagen a descargar
              content = function(file) {
                png(file)
                print(plot)
                dev.off()
              }
          ),
          "2" = downloadHandler(
              filename = paste(name, '.svg', sep=''), #nombre de la imagen a descargar
              content = function(file) {
                svg(file)
                print(plot)
                dev.off()
              }
          ),
          "3" = downloadHandler(
              filename = paste(name, '.pdf', sep=''), #nombre del pdf a descargar
              content = function(file) {
                pdf(file = file, width=12, height=8)
                print(plot)
                dev.off()
              }
          )
 )
}

downloadTwoPlot <- function(option, plot1, plot2, name){
  switch (option,
          "1" = downloadHandler(
            filename = paste(name, '.png', sep=''), #nombre de la imagen a descargar
            content = function(file) {
              png(file)
              grid.arrange( plot1, plot2, ncol=2)
              dev.off()
            }
          ),
          "2" = downloadHandler(
            filename = paste(name, '.svg', sep=''), #nombre de la imagen a descargar
            content = function(file) {
              svg(file)
              par(mfrow=c(1,2))
              grid.arrange( plot1, plot2, ncol=2)
              dev.off()
            }
          ),
          "3" = downloadHandler(
            filename = paste(name, '.pdf', sep=''), #nombre del pdf a descargar
            content = function(file) {
              pdf(file = file, width=10, height=8)
              par(mfrow=c(1,2))
              grid.arrange( plot1, plot2, ncol=2)
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
    sliderInput(x, label = xname, min = 1, step = 1,
                max = ncol(file), value = c(1, 4)),
    sliderInput(y, label = yname, min = 1, step = 1,
                max = nrow(file), value = c(1, (nrow(file))))
  )
}

#funcion contenedora de tres slider
treeSlider <- function(x, y, z, file, xname, yname, zname, MS){
  box(
    width = 6, status = "success",
    h4("Range"),
    if(MS){
      sliderInput(x, label = xname, min = 1, step = 1,
                  max = ncol(file), value = c(1,4))
    }else{
      sliderInput(x, label = xname, min = 1, step = 1,
                  max = ncol(file)-1, value = c(1,4)) 
    },
    sliderInput(y, label = yname, min = 1, step = 1,
                max = ncol(file), value = 3),
    sliderInput(z, label = zname, min = 1, step = 1,
                max = nrow(file), value = c(1, nrow(file)))
  )
}

sizeAndAlpha <- function(a,b, aname, bname){
  column(12,
    sliderInput(a, label = aname, min = 0.5, 
                max = 5, value = 2),
    sliderInput(b, label = bname, min = 0.01, 
                max = 0.99, value = 0.20)
  )
}

#Selectores para los modelos: variables predictoras y variable a predecir
select_model <- function(x,y,data){
    #predictors <- reduceDimensionality()[, !names(reduceDimensionality()) %in% input$pls_response]
    namesVariables <- names(data)
    column(12,
       selectInput(x, label = h4("Response variable"), 
                   choices = namesVariables, selected = namesVariables[ncol(data)]),
       selectInput(y, label = h4("Predictor variables"), 
                   choices = namesVariables, multiple = TRUE)
       )
}

#Formula para aplicar a un determinado modelo
formula_model <- function(x,y){
  if(is.null(x)){ #se predice con todas las variables
    (fmla <- as.formula(paste(paste(y, " ~ "), ".")))
  }
  #Se predice con las variable seleccionadas
  else{
    (fmla <- as.formula(paste(paste(y, " ~ ."), "-", paste(x, collapse= "-"))))
  }
  return(fmla)
}

#funcion que retorna la cantidad de variables del data set que contienen
#valores nominales
cantCol_nominal <- function(data){
  aux <- sapply(data, is.numeric) #valida si los valores de data son numericos con T/F
  return(sum(aux == FALSE)) #cantidad de columnas nominales
}
