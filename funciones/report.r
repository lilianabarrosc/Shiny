
#Funcionalidad encragada crear la visualizacion para el reporte
report <- function(){
  fluidRow(
    column(width = 12,
           #BreadCrumds de outlier    
           HTML('
                <ul class="breadcrumb">
                <li>Report</li>
                <li>Report</li>
                </ul>')
           ),
        box(width = 12, title = "Report", solidHeader = TRUE, status = "success",
            box(width = 12, title = "Data", status = "success",
                uiOutput("dataReport"),
                verbatimTextOutput("data_variables"),
                uiOutput("edit_data")
            ),
                #textOutput("data_report")),
            box(width = 12, title = "Preprocessing", status = "success",
                uiOutput("preprocessing")
            ),
            box(width = 12, title = "Transformation", status = "success",
                uiOutput("transformation")
            ),
            box(width = 12, title = "Regression", status = "success",
                uiOutput("regression")
            )
        )
    )
}

