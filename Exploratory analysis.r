#install.packages('shiny')
#install.packages("shinydashboard")
library('shinydashboard')
library('shiny')

header <- dashboardHeader(title = ":)")

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Dashboard", tabName = "Dsh", icon = icon("dashboard")),
    menuItem("Visualization", tabName = "Vs", icon("cog", lib = "glyphicon"),
      menuSubItem("Scatter Plot", tabName = "Scatter", icon = shiny::icon("angle-double-right")),
      menuSubItem("Density Plot", tabName = "Density", icon = shiny::icon("angle-double-right")),
      menuSubItem("Histogram", tabName = "Histogram", icon = shiny::icon("angle-double-right"))),
    menuItem("Missing Values", tabName = "MV"),
    menuItem("Noise removal", tabName = "Er"),
    menuItem("Normalization", tabName = "Norm", icon("cog", lib = "glyphicon"),
      menuSubItem("Scale Standardization", tabName = "Ss", icon = shiny::icon("angle-double-right")),
      menuSubItem("Normalization 0-1", tabName = "Ss", icon = shiny::icon("angle-double-right"))),
    menuItem("Dimensionality reduction", tabName = "Rd", icon("cog", lib = "glyphicon"),
      menuSubItem("PCA", tabName = "pca", icon = shiny::icon("angle-double-right")),
      menuSubItem("SVD", tabName = "svd", icon = shiny::icon("angle-double-right")),
      menuSubItem("colineality test", tabName = "test", icon = shiny::icon("angle-double-right")),
      menuSubItem("Selecting atributes", tabName = "atributes", icon = shiny::icon("angle-double-right"))),
    menuItem("Outlier detection", tabName = "outlier")
  ))

body <- dashboardBody(
  tabItems( 
    #Tab del Dashboard
    tabItem(tabName = "Dsh",
            h2("Dashboard tab content"),
            navbarPage("App Title",
                       tabPanel("Plot"),
                       tabPanel("Summary"),
                       tabPanel("Table"))),
    # Tab de la visualizacion
    tabItem(tabName = "Histogram",
      # ordeno el contenido por columna
      fluidRow(
        column(width = 12,box(
          title = "Histograma",status = "primary",
          plotOutput("plot1", height = 300))),
        column(width = 10,box(
                 title = "Herramientas", status = "warning", collapsible = TRUE,
                 sliderInput("slider", "Número de observaciones:", 1, 100, 50)
               ))   
      ))
  ))

ui <- dashboardPage( title = "Exploratory analysis", skin = "purple",
  header, #titulo
  sidebar, #menu lateral
  body #cuerpo
)

server <- function(input, output) {
  set.seed(122)
  histdata <- rnorm(500)
  #nombre del grafico para llamarlo en la vista
  output$plot1 <- renderPlot({
    data <- histdata[seq_len(input$slider)]
    hist(data)
  })
}

shinyApp(ui, server)


#---------------------



