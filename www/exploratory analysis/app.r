#install.packages('shiny')
#install.packages("shinydashboard")
library('shinydashboard')
library('shiny')

source("../head.r")

header <- head()
#header <- dashboardHeader(title = ":)")

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Dashboard", tabName = "Dsh", icon = icon("dashboard")),
    menuItem("Visualization", tabName = "Vs", icon("cog", lib = "glyphicon"),
      menuSubItem("Scatter Plot", tabName = "Scatter", icon = shiny::icon("angle-double-right")),
      menuSubItem("Density Plot", tabName = "Density", icon = shiny::icon("angle-double-right")),
      menuSubItem("Histogram", tabName = "Histogram", icon = shiny::icon("angle-double-right"))),
    menuItem("Missing Values", tabName = "MV"),
    menuItem("Noise removal", tabName = "NR"),
    menuItem("Normalization", tabName = "Norm", icon("cog", lib = "glyphicon"),
      menuSubItem("Scale Standardization", tabName = "Ss", icon = shiny::icon("angle-double-right")),
      menuSubItem("Normalization 0-1", tabName = "01", icon = shiny::icon("angle-double-right"))),
    menuItem("Dimensionality reduction", tabName = "Rd", icon("cog", lib = "glyphicon"),
      menuSubItem("PCA", tabName = "pca", icon = shiny::icon("angle-double-right")),
      menuSubItem("SVD", tabName = "svd", icon = shiny::icon("angle-double-right")),
      menuSubItem("colineality test", tabName = "test", icon = shiny::icon("angle-double-right")),
      menuSubItem("Selecting attributes", tabName = "atributes", icon = shiny::icon("angle-double-right"))),
    menuItem("Outlier detection", tabName = "OD")
  ))

graficOption <- tools_general_grafics()

option <- box_option_grafics(graficOption)
rem_option <- removal_option()
missing_option <- box_option_grafics(rem_option)

#contenido
body <- dashboardBody(
  tabItems( 
    #Tab del Dashboard
    tabItem(tabName = "Dsh",
            h2("Dashboard tab content"),
            navbarPage("App Title",
                       tabPanel("Plot"),
                       tabPanel("Summary"),
                       tabPanel("Table"))
            ),
    # visualizacion
    tabItem(tabName = "Scatter",
            # opciones del grafico scatter
            tab_grafics("Scatter Plot", option)
    ),
    tabItem(tabName = "Density",
            # opciones del grafico de desnsidad
            tab_grafics("Density Plot", option)
    ),
   
    tabItem(tabName = "Histogram",
      # opciones del grafico histograma
      tab_grafics("Histogram", option)
    ),
    #fin visualizacion
    # Tab de missing values
    tabItem(tabName = "MV",
      tab_grafics("Missing Values", option)
    ),
    #Tab de eliminacion de ruido
    tabItem(tabName = "NR",
      tab_grafics("Noise removal", missing_option)
    ),
    # tab de normalizaciones
    tabItem(tabName = "Ss",
            normalizations("Scale Standardization")
    ),
    tabItem(tabName = "01",
            normalizations("Normalization 0-1")
    ),
    ##fin normalizacion
    #Reduccion de la dimencionalidad
    tabItem(tabName = "pca",
            tab_grafics("PCA", note_download())
    ),
    tabItem(tabName = "svd",
            tab_grafics("SVD", note_download())
    ),
    tabItem(tabName = "test",
            colinearity_options()
    ),
    tabItem(tabName = "atributes"
            
    ),
    #fin reduccion de la dimencionalidad
    tabItem(tabName = "OD",
           oulier_option("value"),
           tab_grafics("Graphics", note_download())
    )
  ))

ui <- dashboardPage( title = "Exploratory analysis", skin = "purple",
  header, #titulo
  sidebar, #menu lateral
  body #cuerpo
)

server <- function(input, output) {
  output$value <- renderPrint({input$numgraphics})
}

shinyApp(ui, server)


#---------------------



