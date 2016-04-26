source('funciones/generalTools.r')

tabsDiagnosticP <- function(title, tab1, tab2, tab3, tab4) {
  fluidRow(
    column(width = 12,
           #BreadCrumds de outlier    
           HTML('
                <ul class="breadcrumb">
                <li>Linear model evaluation</li>
                <li>Diagnostic Plots</li>
                </ul>'),
           tabBox(
             title = title, width = 12, id = "tabset2",
             "Once you have the actual value and the one predicted by the learning model, it is possible to plot 
             the so-called diagnostic pĺots. These plots permit the detection of outliers using a visual tool 
             that relates the predicted value versus the actual value. 
             ",
             tabPanel(tab1,
                      bsAlert("alertRF"),
                      #residual vs fitted
                      #opcion para ver los puntos en el grafico
                      plotOutput("ResidualsFitted",
                                 brush = brushOpts(
                                   id = "ResidualsFitted_brush"
                                 )
                      ),
                      tools_general_grafics("radioRF", "note7", "save7", "cancel7",
                                            "downloadPlotRF", verbatimTextOutput("ResidualsFitted_brushInfo"),NULL)
             ),
             tabPanel(tab2,
                      #scale-location
                      bsAlert("alertSL"),
                      #opcion para ver los puntos en el grafico
                      plotOutput("StResidualsFitted",
                                 brush = brushOpts(
                                   id = "StResidualsFitted_brush"
                                 )
                      ),
                      tools_general_grafics("radioSF", "note21", "save21", "cancel21",
                                           "downloadPlotSF", verbatimTextOutput("StResidualsFitted_brushInfo"),NULL)
             ),
             tabPanel(tab3, 
                      #normal Q-Q
                      bsAlert("alertQQ"),
                      #opcion para ver los puntos en el grafico
                      plotOutput("NormalQQ"
#                                  brush = brushOpts(
#                                    id = "NormalQQ_brush"
#                                  )
                      ),
                      tools_general_grafics("radioQQ", "note22", "save22", "cancel22",
                                           "downloadPlotQQ", verbatimTextOutput("NormalQQ_brushInfo"),NULL)
             ),
             tabPanel(tab4,
                      #residual vs leverage
                      bsAlert("alertRL"),
                      #opcion para ver los puntos en el grafico
                      plotOutput("StResidualsLeverange",
                                 brush = brushOpts(
                                   id = "StResidualsLeverange_brush"
                                 )
                      ),
                      tools_general_grafics("radioRL", "note23", "save23", "cancel23",
                                          "downloadPlotRL", verbatimTextOutput("StResidualsLeverange_brushInfo"),NULL)
             )
           ) 
      )
  )
}

colinearityTest <- function(){
  fluidRow(
    column(width = 12,
           #BreadCrumds de outlier    
           HTML('
                <ul class="breadcrumb">
                <li>Linear model evaluation</li>
                <li>Colinearity Test</li>
                </ul>')
    )
#     box(width = 12, title = "Colinearity Test", solidHeader = TRUE, status = "success",
#       h4("Result"),
#       verbatimTextOutput("colinearity_result"),
#       h4("Tolerance"),
#       verbatimTextOutput("colinearity_tolerance"),
#       h4("Means"),
#       verbatimTextOutput("colinearity_mean")
#     ),
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