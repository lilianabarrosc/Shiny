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
           helpText("The disgnostic plots are a visualization tool for analyzing <b>linear models</b>.
                    They focus on how residuals could show how poorly a model represents the data."),
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