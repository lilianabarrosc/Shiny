source('funciones/preprocessing.r')

data_sets <- list("iris" = 1, "airquality" = 2, "cars" = 3, "new" = 4)
#vista que muestra el listado de data set utilizados
viewData <- function() {
  fluidPage(
    column(width = 12,
           #BreadCrumds de reduccion de la dimencionalidad     
           HTML('
                <ul class="breadcrumb">
                <li>Data</li>
                <li>Source</li>
                </ul>')
    ),
    box(width = 12, status = "success",
      radioButtons("select_file", label = h3("Select data Set"), selected = 2,
                           choices = data_sets
      ),
        # Salida de componente dinamico (subir archivo)
        uiOutput("ui")

      #dataTableOutput('contents')
    ),
      tabBox(width = 12,
      tabPanel("STR", verbatimTextOutput("str_data")),
      tabPanel("Summary", verbatimTextOutput("summary_data")) #dataTableOutput(outputId="summary_data")
      )          
      #  )
    #HTML("</div>")
  )
}

#tabs de la vista de visualización, resibe como parametros el titulo del contenedor y los tabs 
tabsVisualization <- function(title, tab1, tab2) {
  fluidRow(
    column(width = 12,
           #BreadCrumds de visualizacion     
           HTML('
                <ul class="breadcrumb">
                <li>Data</li>
                <li>Visualization</li>
                </ul>'),
           tabBox(
             title = title,
             width = 12,
             id = "tabset1",
             tabPanel(tab1,
                      colors("col1", "col2", "col3"),
                      #contenido del tab1 = Scatter plot 1
                      tab_grafics("scatter1",  tools_general_grafics("radio0", "note0", "save0", "cancel0",
                                                                     "download0", 
                                                                     #slider_range_int("x1","y1","z1")
                                                                     uiOutput("slider_range_range_density"), NULL
                      ))
             ),
             tabPanel(tab2, 
                      #contenido del tab2 = parallel grafics
                      tab_grafics("parallel", tools_general_grafics("radio1", "note1", "save1", "cancel1",
                                                                    "download1", uiOutput("slider_range_range_parallel"), uiOutput("slider_range_range_parallel2")))
             )
           ) 
           )
  )
}

colors <- function(id1, id2, id3){
  fluidRow(
    column(width = 4,
           colourInput(id1, "Choose colour", "darkred")),
    column(width = 4,
           colourInput(id2, "Choose colour", "yellow")),
    column(width = 4,
           colourInput(id3, "Choose colour", "darkgreen"))
  )
}