#funcion encargada de instalar la libreria en caso de no ser instalada
check_and_install <- function( packname ) { # given package name, check installation, install if not found
  if ( packname %in% rownames(installed.packages()) == FALSE ) {
    install.packages( packname )
  }
}
# check_and_install("ggplot2")
# library(ggplot2)


#Funcion encargada de leer las librerias para la app
LoadLibraries <- function(){
  #install.packages('shiny')
  check_and_install('shiny')
  library('shiny')
  #install.packages('shinydashboard')
  check_and_install('shinydashboard')
  library('shinydashboard')
  #install.packages("devtools")
  #library('devtools')
  #install_github("mariytu/RegressionLibs") #Para usar esto hay que tener instalado devtools
  library('RegressionLibs')
  #install.packages("Amelia") or sudo apt-get install r-cran-amelia 
  #install.packages("Amelia", repos="http://r.iq.harvard.edu", type = "source")
  #http://gking.harvard.edu/amelia/ 
  #library('Amelia') // ya no se utiliza
  #install.packages("VIM") #sudo apt-get install r-cran-rcppeigen para amazon
  check_and_install('VIM')
  library('VIM')
  #install.packages("clusterSim")
  check_and_install('clusterSim')
  library('clusterSim')
  #devtools::install_github("daattali/shinyjs") #libreria para los colores
  library('shinyjs')
  #install.packages("Rlof")
  check_and_install('Rlof')
  library('Rlof') #Outlier detection library
  #install.packages("plyr")
  check_and_install('plyr')
  library("plyr") ##required for count()
  #install.packages('shinyBS')
  check_and_install('shinyBS')
  library("shinyBS") #libreria utilizada para los alert y dialog
  #install.packages('DAAG')
  #library('DAAG') #libreria para cross validation // ya no se utiliza
  #install.packages("RPostgreSQL")
  check_and_install('RPostgreSQL')
  library('RPostgreSQL') #Libreria para postgress 
  #install.packages("car")
  check_and_install('car')
  library('car')
  #install.packages("plsdepot")
  #library('plsdepot') #Libreria para utilizar pls
  #install.packages("pls")
  check_and_install('pls')
  library('pls')
  #install.packages("glmnet")
  check_and_install('glmnet')
  library('glmnet') #Libreria para utilizar validacion cruzada en ridge
  #install.packages("MASS")
  check_and_install('MASS')
  library('MASS') #Libreria para utilizar ridge
  #install.packages("bootstrap")
  check_and_install('bootstrap')
  library('bootstrap') #Libreria para utilizar validación boostrap
  #install.packages("randomGLM")
  check_and_install('randomGLM')
  library('randomGLM') #libreria para utilizar RGML
  #install.packages("FSelector")
  check_and_install('FSelector')
  library('FSelector') #libreria para attribute selector
  #install.packages("gridExtra")
  check_and_install('gridExtra')
  library('gridExtra') #libreria para ver varios graficos ggplot en una pagina
  #install.packages("rmarkdown")
  check_and_install('rmarkdown')
  library(rmarkdown) #libreria para descargar el reporte
  print("The libraries have been loaded.")
}