#install.packages('shiny')
library('shiny')
#install.packages('shinydashboard')
library('shinydashboard')
#install.packages("devtools")
#library('devtools')
#install_github("mariytu/RegressionLibs") #Para usar esto hay que tener instalado devtools
library('RegressionLibs')
#install.packages("Amelia") or sudo apt-get install r-cran-amelia
#install.packages("Amelia", repos="http://r.iq.harvard.edu", type = "source")
#http://gking.harvard.edu/amelia/ 
#library('Amelia')
#install.packages("VIM") #sudo apt-get install r-cran-rcppeigen para amazon
library('VIM')
#install.packages("clusterSim")
library('clusterSim')
#devtools::install_github("daattali/shinyjs") #libreria para los colores
library('shinyjs')
#install.packages("Rlof")
library('Rlof') #Outlier detection library
#install.packages("plyr")
library("plyr") ##required for count()
#install.packages('shinyBS')
library("shinyBS") #libreria utilizada para los alert y dialog
#install.packages('DAAG')
library('DAAG') #libreria para cross validation
#install.packages("RPostgreSQL")
library('RPostgreSQL') #Libreria para postgress

source('funciones/Server.r')
source('funciones/opcionesDashboard.r')
source('funciones/preprocessing.r')
source('funciones/transformation.r')
source('funciones/data.r')
source('funciones/regresion.r')
source('funciones/outlier.r')
source('funciones/home.r')

#variable global que con el color de los slider
dataset <- NULL #Nombre del data set seleccionado, el cual no contiene valores nominales.

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
                                           p("The Güiña is a small cat that is endemic from the evergreen forest of southern 
                                             Chile. This smart predator relies on its senses to identify and capture the prey, 
                                             usually sheltered in the dense and obscure forest."),
                                           p("This clever feline served us as inspiration to build a data mining tools for 
                                             visualizing an analyzing data. From our perspective, the data miner acts as a 
                                             furtive predator of precious information hidden in the dark data forest."),
                                           p("Coincidentally the name Güiña begins with the three letters GUI which also 
                                             stands for the acronym for Graphical User Interface (GUI).")
                                           ),
                                         imageOutput("home")
                                           ),
                                  column(6,
                                         hr(), hr(),
                                         uiOutput("signIn")
                                  )
                              )
                          ),
                        #Tab del data
                        tabItem(tabName = "source",
                                viewData()
                        ),
                        #Inicio tabs Analisis exploratorio (funcionalidades en el archivo analisisExploratorio.r)
                        tabItem(tabName = "visualization",
                                tabsVisualization("Visualization", "Scatter plot", "Parallel plot")
                        ),
                        tabItem(tabName = "mvalues",
                                tabsMissingValues("Missing values", "Box plot", "Histogram","Scatter plot")
                        ),
                        tabItem(tabName = "nremoval",
                                noiseRemoval("")
                        ),
                        tabItem(tabName = "outlier",
                                localOutlier("Local outlier factor")
                        ),
                        tabItem(tabName = "normalization",
                                normalizations("Normalization")
                        ),
                        tabItem(tabName = "pca",
                                pca("")
                        ),
                        tabItem(tabName = "svd",
                                svd2("")
                        ),
                        tabItem(tabName = "lm",
                                linearRegression()
                        ),
                        tabItem(tabName = "diagnosticP",
                                tabsDiagnosticP("Diagnostic Plots", "Residual vs Fitted", "Scale-location",
                                                "Normal Q-Q", "Residual vs leverage")
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