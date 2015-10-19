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

graficOption <- fluidRow(
          box(
            title = "Range", width = 6, solidHeader = TRUE,
            background = "aqua",
            sliderInput("atributes", label = "Atributes", min = 0, 
                        max = 100, value = c(40, 60)),
            sliderInput("observation", label = "Observation", min = 0, 
                        max = 100, value = c(40, 60))
          ),
          box(
            title = "Download image", width = 4, solidHeader = TRUE,
            background = "blue",
            radioButtons("radio","",
                         choices = list("PNG" = 1, "SVG" = 2, "PDF" = 3), 
                         selected = 1),
            actionButton("download", label = "Download")
          ),
          box(
            title = "Notes", width = 3, solidHeader = TRUE,
            background = "light-blue",
            textInput("notes", label = "", value = "notes...")
          )
        )

option <- box( width = 12, title = "Option", solidHeader = TRUE,
               collapsible = TRUE,
               graficOption
)

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
    tabItem(tabName = "Scatter",
            # ordeno el contenido por columna
            fluidRow(
              column(width = 12,
                     box(width = 12,
                        title = "Scatter Plot",status = "primary"
                                #plotOutput("plot1", height = 300)
              )),
              column(width = 12, 
                     option
              )
            )
    ),
    tabItem(tabName = "Density",
            # ordeno el contenido por columna
            fluidRow(
              column(width = 12,
                     box(width = 12,
                         title = "Density Plot",status = "primary"
                        #plotOutput("plot1", height = 300)
              )),
              column(width = 12, 
                     option
              )
            )
    ),
    # Tab de la visualizacion
    tabItem(tabName = "Histogram",
      # ordeno el contenido por columna
      fluidRow(
        column(width = 12,box(width = 12,
          title = "Histogram",status = "primary"
          #plotOutput("plot1", height = 300)
          )),
        column(width = 12, 
               option
        )
      )
    )
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



