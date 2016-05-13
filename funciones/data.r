source('funciones/generalTools.r')

data_sets <- list("iris" = 1, "airquality" = 2, "sleep" = 3, "Upload file" = 4, "URL file" = 5)
#vista que muestra el listado de data set utilizados
viewData <- function() {
  fluidRow(
    column(width = 12,
           #BreadCrumds de reduccion de la dimencionalidad     
           HTML('
                <ul class="breadcrumb">
                <li>Data</li>
                <li>Source</li>
                </ul>')
    ),
    box(width = 12, status = "success",
      bsAlert("alertUpload"),
      bsAlert("alertRUL"),
      radioButtons("select_file", label = h3("Select data Set"), selected = 2,
                           choices = data_sets),
        # Salida de componente dinamico (subir archivo)
        uiOutput("data_extern")
    ),
    tabBox(width = 12,
        tabPanel("STR", verbatimTextOutput("str_data")),
        tabPanel("Summary", verbatimTextOutput("summary_data"))
    )          
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
           box( width = 12, title = "Change color palette", solidHeader = TRUE,
                collapsible = TRUE, collapsed = TRUE,
                colors("col1","col2","col3")
           ),
           tabBox(
             title = title,
             width = 12,
             id = "tabset1",
             tabPanel(tab1,
                      #contenido del tab1 = Scatter plot 1
                      bsAlert("alertScatter"),
                      tab_grafics("scatter_plot",  tools_general_grafics("radio_Scatterplot", "note0", "save0", "cancel0",
                                                                     "download_Scatterplot", uiOutput("slider_Scatterplot"), 
                                                                     NULL))
             ),
             tabPanel(tab2, 
                      #contenido del tab2 = parallel grafics
                      bsAlert("alertParallel"),
                      tab_grafics("parallel", tools_general_grafics("radio_parallelplot", "note1", "save1", "cancel1",
                                                                    "download_parallelplot", uiOutput("slider_parallelPlot"), 
                                                                    uiOutput("slider_parallelPlot2")))
             )
           ) 
      )
  )
}

#funcion que contiene las URL tentativas de otros "csv"
urls <- function(){
     box(width = 12,
         optionReader("sep_url", "dec_url", "quote_url", "header_url", "na_url"),
         textInput("url", label = "URL (only csv) ", value = "https://dl.dropboxusercontent.com/u/12599702/autosclean.csv"),
         "Others URL:",br(),
         tags$ul(tags$li("http://www.stat.wisc.edu/~gvludwig/fall_2012/midterm2_problem1.csv")),
         tags$ul(tags$li("https://raw.githubusercontent.com/trinker/dummy/master/data/gcircles.csv")),
         bsButton("upload", label = "Upload", style = "success")
        )
}

optionReader <- function(idSep, idDec, idQuote, idHeader, idNA){
  column(12,
    column(3, selectInput(idSep, "Separator:", c(";",",","\t"))),
    column(3, selectInput(idDec, "Decimal:", c(".",","))),
    column(3, selectInput(idHeader, "Header:", c("FALSE","TRUE"))),
    column(3, selectInput(idQuote, "Quote:", c("' '",'"',"'"))),
    column(6, textInput(idNA, "Missing values:", value = "NA"))
  )
}