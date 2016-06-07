#lectura de librerias (funcion contenida en generalTools)
source('funciones/libraries.r')
LoadLibraries()

source('funciones/Server.r')
source('funciones/opcionesDashboard.r')
source('funciones/preprocessing.r')
source('funciones/transformation.r')
source('funciones/data.r')
source('funciones/regresion.r')
source('funciones/linearModelEvaluation.r')
source('funciones/home.r')
source('funciones/report.r')

#titulo de la pagina
dbHeader <- dashboardHeader()
dbHeader$children[[2]]$children <- imageOutput("logo") #tags$img(src='images/logo.jpg', height='30', width='70')

#Cuerpo de la pagina
body <- dashboardBody(includeCSS("css/styles.css"),
                    tabItems(
                        #Tab del home
                        tabItem(tabName = "home",
                                fluidPage(
                                  column(6,
                                         titlePanel("Welcome to Güiña!"),
                                         wellPanel(
                                           p(style = "text-align:justify;", "The Güiña is a small cat that is endemic from the evergreen forest of southern 
                                             Chile. This smart predator relies on its senses to identify and capture the prey, 
                                             usually sheltered in the dense and obscure forest."),
                                           p(style = "text-align:justify;", "This clever feline served us as inspiration to build a data mining tools for 
                                             visualizing an analyzing data. From our perspective, the data miner acts as a 
                                             furtive predator of precious information hidden in the dark data forest."),
                                           p(style = "text-align:justify;", "Coincidentally the name Güiña begins with the three letters GUI which also 
                                             stands for the acronym for Graphical User Interface (GUI).")
                                           )
                                        # imageOutput("home")
                                           ),
                                  column(6,
                                         hr(), hr(),
                                         #uiOutput("signIn")
                                         imageOutput("home")
                                  )
                              )
                          ),
                        #Inicio tabs Data (funcionalidades en el archivo data.r)
                        tabItem(tabName = "source",
                                viewData()
                        ),
                        tabItem(tabName = "visualization",
                                tabsVisualization("Visualization", "Scatter plot", "Parallel plot")
                        ),
                        #Inicio tabs Preprosesamiento (funcionalidades en el archivo preprocessing.r)
                        tabItem(tabName = "mvalues",
                                tabsMissingValues("Missing values", "Box plot", "Histogram","Scatter plot")
                        ),
                        tabItem(tabName = "outlier",
                                localOutlier("Local outlier factor")
                        ),
                        tabItem(tabName = "nremoval",
                                noiseRemoval("")
                        ),
                        #Inicio tabs Transformacion (funcionalidades en el archivo transformation.r)
                        tabItem(tabName = "normalization",
                                normalizations("Normalization")
                        ),
                        tabItem(tabName = "pca",
                                pca("")
                        ),
                        tabItem(tabName = "svd",
                                svd2("")
                        ),
                        tabItem(tabName = "attributeS",
                                attributeSelection()
                        ),
                        #Inicio tabs Regresion (funcionalidades en el archivo regresion.r)
                        tabItem(tabName = "lm",
                                linearRegression()
                        ),
                        tabItem(tabName = "pls",
                                pls()
                        ),
                        tabItem(tabName = "ridge",
                                ridge()
                        ),
                        tabItem(tabName = "rglm",
                                rglm()
                        ),
                        #Inicio tabs Evalucion de modelos (funcionalidades en el archivo linearModelEvaluation.r)
                        tabItem(tabName = "diagnosticP",
                                tabsDiagnosticP("Diagnostic Plots", "Residual vs Fitted", "Scale-location",
                                                "Normal Q-Q", "Residual vs leverage")
                        ),
                        #Inicio tabs Reporte (funcionalidades en el archivo report.r)
                        tabItem(tabName = "report",
                                report()
                        )
                  )
)
#--------------------Cliente-------------------
#head() y sidebar() son funciones contenidas en el archivo opcionesDashboard.r
ui <- dashboardPage(skin = "green", 
                    dbHeader, 
                    dashboardSidebar(sidebar(TRUE, 'USER')),#sidebarMenuOutput("side")),
                    body #dashboardBody()
                    )

#App
shinyApp(ui, server)