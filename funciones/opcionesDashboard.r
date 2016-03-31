
#titulo de la pagina
head <- function() {
  dbHeader <- dashboardHeader()
  dbHeader$children[[2]]$children <-  tags$a(href='#',
                                             tags$img(src='images/gato.jpg',height='60',width='200'))
}

sidebar <- function(isLogged, username) {
    if(isLogged == FALSE){ #opciones del sidebar para todo usuario (pagina principal o de inicio)
      sidebarMenu(
        menuItem("Home", tabName = "home", icon = icon("home"))
      )
    }
    else if(isLogged == TRUE){ #opciones del sidebar para usuario logeado
      sidebarMenu(
              sidebarUserPanel(username,
                               subtitle = a(href = "#", icon("circle", class = "text-success"), "Online"),
                               # Imagen contenida en la carpeta www
                               image = "logo.png"
              ),
             menuItem("Home", tabName = "home", icon = icon("home")),
             menuItem("Data", tabName = "data",
                      menuSubItem("Source", tabName = "source", icon = shiny::icon("angle-double-right")),
                      menuSubItem("Visualization", tabName = "visualization", icon = shiny::icon("angle-double-right"))
             ),
             menuItem("Preprocessing", tabName = "preprocessing", icon("cog", lib = "glyphicon"),
                      menuSubItem("Missing Values", tabName = "mvalues", icon = shiny::icon("angle-double-right")),
                      menuSubItem("Local outlier factor", tabName = "outlier", icon = shiny::icon("angle-double-right")),
                      menuSubItem("Noise removal", tabName = "nremoval", icon = shiny::icon("angle-double-right"))
             ),
             menuItem("Transformation", tabName = "transformation", icon("cog", lib = "glyphicon"),
                      menuSubItem("Normalization", tabName = "normalization", icon = shiny::icon("angle-double-right")),
                      menuSubItem("PCA", tabName = "pca", icon = shiny::icon("angle-double-right")),
                      menuSubItem("SVD", tabName = "svd", icon = shiny::icon("angle-double-right")),
                      menuSubItem("Colinearity Test", tabName = "colinearityT", icon = shiny::icon("angle-double-right")),
                      menuSubItem("Attribute selection", tabName = "attributeS", icon = shiny::icon("angle-double-right"))
             ),
             menuItem("Regression", tabName = "regression", icon("cog", lib = "glyphicon"),
                      menuSubItem("Linear Regression", tabName = "lm", icon = shiny::icon("angle-double-right"))
             ),
             menuItem("Outlier detection", tabName = "odetection", icon("cog", lib = "glyphicon"),
                      menuSubItem("Diagnostic Plots", tabName = "diagnosticP", icon = shiny::icon("angle-double-right"))
             ),
             menuItem("Report", tabName = "report")
          )
    }
}