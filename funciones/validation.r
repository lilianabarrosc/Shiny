
#Funcion que muestra la vista de la validacion Ten fold cross
# tenFoldCross <- function() {
#   fluidRow(
#     column(width = 12,
#            HTML('
#                 <ul class="breadcrumb">
#                 <li>Validation</li>
#                 <li>Ten fold cross</li>
#                 </ul>')
#     ),
#     box(
#       width = 12, title = "Ten fold cross", solidHeader = TRUE, status = "success",
#       plotOutput("crossPlot"),
#       verbatimTextOutput("validationTFC")
#     )
#   )
# }
# 
# #funcion uqe muetra la vista de la validacion train/test
# trainTest <- function(){
#   fluidRow(
#     column(width = 12,
#            HTML('
#                 <ul class="breadcrumb">
#                 <li>Validation</li>
#                 <li>Test/traning</li>
#                 </ul>')
#            ),
#     tabBox(
#       title = "Test/traning", width = 12, id = "tabset2",
#       tabPanel("Porcent",
#            numericInput("porcentTest", "Train size in %", 0.75, 
#                         min = 0.0, max = 1, step = 0.02),
#            h4('Train file'),
#            verbatimTextOutput("validationTT"),
#            h4('Test file'),
#            verbatimTextOutput("validationTT2"),
#            h4('Results'),
#            verbatimTextOutput("predict")
#        )
# #       tabPanel("File",
# #            fileInput('fileTest', 'Test File',
# #                 accept=c('text/csv', 'text/comma-separated-values,text/plain','.csv'))
# #            #verbatimTextOutput("validationTT")
# #       )
#     )
#   )
# }