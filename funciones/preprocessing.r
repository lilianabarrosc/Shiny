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
                         actionButton("deleteMS", label = "Delete MS",href="")
               )
           ),
           tabBox(
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
           #BreadCrumds de eliminacion de ruido    
           HTML('
                <ul class="breadcrumb">
                <li>Preprocessing</li>
                <li>Noise removal</li>
                </ul>'),
           tab_grafics("", tools_general_grafics("radio5", "note5", "save5", "cancel5",
                                                 "download5", uiOutput("slider_range_range_nremoval"), NULL))
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
            plotOutput("densityPlot")
    )
  )
}