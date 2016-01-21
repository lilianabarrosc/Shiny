source('funciones/preprocessing.r')

tabsDiagnosticP <- function(title, tab1, tab2, tab3, tab4) {
  fluidRow(
    column(width = 12,
           #BreadCrumds de outlier    
           HTML('
                <ul class="breadcrumb">
                <li>Outlier</li>
                <li>Diagnostic Plots</li>
                </ul>'),
           tabBox(
             title = title, width = 12, id = "tabset2",
             "Once you have the actual value and the one predicted by the learning model, it is possible to plot 
             the so-called diagnostic pĺots. These plots permit the detection of outliers using a visual tool 
             that relates the predicted value versus the actual value. 
             ",
             
             tabPanel(tab1,
                      #residual vs fitted
                      plotOutput("ResidualsFitted",
                                 brush = brushOpts(
                                   id = "ResidualsFitted_brush"
                                 )
                      ),
                      tools_general_grafics("radio7", "note7", "save7", "cancel7",
                                            "download7", verbatimTextOutput("ResidualsFitted_brushInfo"),NULL)
             ),
             tabPanel(tab2,
                      #scale-location
                      plotOutput("StResidualsFitted",
                                 brush = brushOpts(
                                   id = "StResidualsFitted_brush"
                                 )
                      ),
                      tools_general_grafics("radio21", "note21", "save21", "cancel21",
                                           "download21", verbatimTextOutput("StResidualsFitted_brushInfo"),NULL)
             ),
             tabPanel(tab3, 
                      #normal Q-Q
                      plotOutput("NormalQQ",
                                 brush = brushOpts(
                                   id = "NormalQQ_brush"
                                 )
                      ),
                      tools_general_grafics("radio22", "note22", "save22", "cancel22",
                                           "download22", verbatimTextOutput("NormalQQ_brushInfo"),NULL)
             ),
             tabPanel(tab4,
                      #residual vs leverage
                      plotOutput("StResidualsLeverange",
                                 brush = brushOpts(
                                   id = "StResidualsLeverange_brush"
                                 )
                      ),
                      tools_general_grafics("radio23", "note23", "save23", "cancel23",
                                          "download23", verbatimTextOutput("StResidualsLeverange_brushInfo"),NULL)
             )
           ) 
           )
  )
}