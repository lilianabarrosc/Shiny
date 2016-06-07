
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
                uiOutput("data_report")),
                #textOutput("data_report")),
            box(width = 12, title = "Preprocessing", status = "success"),
            box(width = 12, title = "Transformation", status = "success"),
            box(width = 12, title = "Regression", status = "success"),
            box(width = 12, title = "Linear model evaluation", status = "success")
        )
    #     box(width = 12, title = "Your can be applied here:", solidHeader = TRUE, status = "success",
    #         tags$ul(tags$li(
    #           p(style = "text-align:justify;","If the largest colinearity test is greater than 10 then 
    #             there is cause for concern (Bowerman & O'Connell. 1990; Myres, 1990).")
    #         )),
    #         tags$ul(tags$li(
    #           p(style = "text-align:justify;","If the average colinearity test is substantially greater 
    #             than 1 then the regression may be biased ( Bowerman & O'Connell. 1990; Myres, 1990).")               
    #         )),
    #         tags$ul(tags$li(
    #           p(style = "text-align:justify;","Tolerance below 0.1 indicates a serious problem.")               
    #         )),
    #         tags$ul(tags$li(
    #           p(style = "text-align:justify;","Tolerance below 0.2 indicates a potencial problem
    #             (Menard, 1995).")               
    #         ))
    #     )
    )
}