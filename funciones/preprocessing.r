source('funciones/generalTools.r')

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
           box(width = 12, title = title, solidHeader = TRUE, status = "success",
               tags$div( class = 'col-sm-2',
                         bsButton("deleteMS", label = "Delete MS",style = "success")
               )
           ),
           tabBox(
             width = 12,
             id = "tabset2",
             tabPanel(tab1,
                      #opcion1 de visualizacion
                      tab_grafics("missing1", tools_general_grafics("radioboxplot", "note", "save", "cancel",
                                                                    "downloadPlotboxplot", uiOutput("slider_range_range_amelia"),NULL))
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
           #BreadCrumds de eliminacion de ruido    
           HTML('
                <ul class="breadcrumb">
                <li>Preprocessing</li>
                <li>Noise removal</li>
                </ul>'),
           box(width = 12, title = title, solidHeader = TRUE, status = "success",
               column(6,
                      sliderInput("limitNoise", label = "Limit noise", min = 0.01, 
                                  max = 0.99, value = 0.15)
                      ),
               column(6,
                      #h4("The total number of..."),
                      verbatimTextOutput("columnsNoise"),
                      tags$div( class = 'col-sm-2',
                                bsButton("rnoise", label = "Remove noise", style = 'success')
                      )
                      )
           ),
           tab_grafics("nremoval", tools_general_grafics("radio5", "note5", "save5", "cancel5",
                                                 "download5", uiOutput("slider_range_range_nremoval"), uiOutput("slider_range_range_nremoval2")))
           )
    )
  
}

#vista correspondiente a local outlier factor
localOutlier <- function(title){
  fluidRow(
    column( width = 12,
            #BreadCrumds de local outlier factor    
            HTML('
                <ul class="breadcrumb">
                <li>Preprocessing</li>
                <li>Local outlier factor</li>
                </ul>')
    ),
    box(width = 12, title = title, solidHeader = TRUE, status = "success",
        column(width = 6,
               plotOutput("densityPlot")
        ),
        column(width = 6,
               plotOutput("densityPlotResult")
        ),
        column(width = 6,
          h4("The total number of outliers"),
          verbatimTextOutput("howManyOutliers"),
          hr(),
          #numericInput("threshold", label = h4("Threshold"), value = 1.25, step = 0.01)
          uiOutput("sliderLOF")
        ),
        column(width = 6,
          h4("The positions of the outliers"),
          verbatimTextOutput("posOutliers")
        ),
        column(width = 12,
           h4("Data without outliers"),
           verbatimTextOutput("strWithoutOutliers")
        )
    )
  )
}