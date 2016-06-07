#Funcion encargada de leer las librerias para la app
LoadLibraries <- function(){
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
  #library('Amelia') // ya no se utiliza
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
  #library('DAAG') #libreria para cross validation // ya no se utiliza
  #install.packages("RPostgreSQL")
  library('RPostgreSQL') #Libreria para postgress 
  #install.packages("car")
  library('car')
  #install.packages("plsdepot")
  #library('plsdepot') #Libreria para utilizar pls
  #install.packages("pls")
  library('pls')
  #install.packages("glmnet")
  library('glmnet') #Libreria para utilizar validacion cruzada en ridge
  #install.packages("MASS")
  library('MASS') #Libreria para utilizar ridge
  #install.packages("bootstrap")
  library('bootstrap') #Libreria para utilizar validación boostrap
  #install.packages("randomGLM")
  library('randomGLM') #libreria para utilizar RGML
  #install.packages("FSelector")
  library('FSelector') #libreria para attribute selector
  print("The libraries have been loaded.")
}