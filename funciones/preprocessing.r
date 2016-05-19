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
               column(4,
                    tags$div( class = 'col-sm-2',
                              bsButton("deleteMS", label = "Delete MS",style = "success")
                      )
               ),
               column(8, uiOutput("sumMV"))
     ),
     tabBox(
       width = 12,
       id = "tabset2",
       tabPanel(tab1,
                #opcion1 de visualizacion
                bsAlert("alertMissing1"),
                tab_grafics("boxplot", tools_general_grafics("radio_boxplot", "note", "save", "cancel",
                                                              "download_boxplot", uiOutput("slider_missingValues"),NULL))
       ),
       tabPanel(tab2,
                #opcion2 de visualizacion
                bsAlert("alertMissing2"),
                tab_grafics("histogramPlot", tools_general_grafics("radio2", "note2", "save2", "cancel2",
                                                              "download2", uiOutput("slider_histogramPlot"), NULL))
       ),
       tabPanel(tab3,
                #opcion3 de visualizacion
                bsAlert("alertMissing3"),
                tab_grafics("missingScatterPlot", tools_general_grafics("radio3", "note3", "save3", "cancel3",
                                                              "download3", uiOutput("slider_missingScatter"), NULL))
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
           bsAlert("alertNoise"),
           box(width = 12, title = title, solidHeader = TRUE, status = "success",
               column(6,
                      sliderInput("limitNoise", label = "Limit noise", min = 0.01, 
                                  max = 0.99, value = 0.15)
                      ),
               column(6,
                      #h4("The total number of..."),
                      uiOutput("columnsNoise"),
                      tags$div( class = 'col-sm-2',
                                bsButton("rnoise", label = "Remove noise", style = 'success')
                      )
                      )
           ),
           tab_grafics("nremoval", tools_general_grafics("radio5", "note5", "save5", "cancel5",
                                                 "download5", uiOutput("slider_nremoval"), uiOutput("slider_nremoval2")))
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
                </ul>'),
            bsAlert("alertlof")
    ),
    box(width = 12, title = title, solidHeader = TRUE, status = "success",
        column(width = 6,
               bsAlert("alertlof1"),
               plotOutput("densityPlot")
        ),
        column(width = 6,
               bsAlert("alertlof2"),
               plotOutput("densityPlotResult")
        ),
        column(width = 6,
          h4("The total number of outliers"),
          verbatimTextOutput("howManyOutliers"),
          hr(),
          #numericInput("threshold", label = h4("Threshold"), value = 1.25, step = 0.01)
          uiOutput("sliderLOF"),
          tags$div(class = 'col-sm-2', bsButton("delete_lof", label="Delete Outliers", style = "success"))
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