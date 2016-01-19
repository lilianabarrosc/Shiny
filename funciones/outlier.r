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
             the so-called diagnostic pĺots. These plots permit the detection of outliers using a visual tool that relates the predicted value versus the actual value. 
             ",
             
             tabPanel(tab1,
                      #residual vs fitted
                      tab_grafics("ResidualsFitted", tools_general_grafics("radio7", "note7", "save7", "cancel7",
                                                                    "download7", uiOutput("slider_outlier_residualF"),NULL))
             ),
             tabPanel(tab2
                      #scale-location
             ),
             tabPanel(tab3 
                      #normal Q-Q
             ),
             tabPanel(tab4
                      #residual vs leverage
             )
           ) 
           )
  )
}