source('funciones/generalTools.r')

#data_sets <- list("iris" = 1, "airquality" = 2, "sleep" = 3, "Upload file" = 4, "URL file" = 5)
data <- list("My data set" = 1, "New data set" = 2)
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
    box(width = 12, title = "Select data Set", status = "success",
      bsAlert("alertData"),
      bsAlert("alertUpload"),
      bsAlert("alertRUL"),
      radioButtons("dataSet", label = NULL, choices = data),
      uiOutput("view_data"),
      conditionalPanel(
        condition = "input.dataSet == '2' && input.select_newfile == '1'",
        box(width = 12,
            optionReader("sep_upload", "dec_upload", "quote_upload", "header_upload","na_upload"),
            fileInput('file1', 'Choose CSV File', 
                      accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv')),
            bsButton("saveFileUpload", label = "Save data set", style = "success")
        )
      ),
      conditionalPanel(
        condition = "input.dataSet == '2' && input.select_newfile == '2'",
        urls()
      ),
      column(12,
        bsAlert("alertSaveData")
      )
    ),
    tabBox(width = 12,
        tabPanel("STR", verbatimTextOutput("str_data")),
        tabPanel("Summary", verbatimTextOutput("summary_data")),
        #panel contenedor de herramientas como eliminar los valores nominales, eliminar
        #alguna columna.
        tabPanel("Edit data set",
            radioButtons("dataSetEdit", label = NULL, choices = list("Delete column" = 1,
                                                                 "Delete nominal values" = 2)),
            #uiOutput("selectVarDelete"),
            conditionalPanel(
              condition = "input.dataSetEdit == '1'",
              uiOutput("selectVarDelete")
            ),hr(),
            bsAlert("alertEditFile")
        )
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
         bsButton("upload", label = "Upload", style = "success"),
         bsButton("saveFileURL", label = "Save data set", style = "success")
        )
}

#opciones de lectura para archivos (vista)
optionReader <- function(idSep, idDec, idQuote, idHeader, idNA){
  column(12,
    column(3, selectInput(idSep, "Separator:", c(";",",","\t"))),
    column(3, selectInput(idDec, "Decimal:", c(",","."))),
    column(3, selectInput(idHeader, "Header:", c("TRUE","FALSE"))),
    column(3, selectInput(idQuote, "Quote:", c('""','"',"'"))),
    column(6, textInput(idNA, "Missing values:", value = "NA"))
  )
}
