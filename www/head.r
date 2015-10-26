library('shiny')
library('shinydashboard')

head <- function() {
  header <- dashboardHeader(title = ":)")
  return(header)
}

tools_general_grafics <- function(x){
  fluidRow(
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
    #box para los apuntes
    box(
      title = "Notes", width = 4, solidHeader = TRUE,
      background = "light-blue",
      tags$div( class='form-group shiny-input-container',
                tags$textarea("notes...", class="form-control shiny-bound-input", style="resize: none")
      )
    )
  )
}

box_option_grafics <- function(graficOption){
  box( width = 12, title = "Options", solidHeader = TRUE,
       collapsible = TRUE,
       graficOption,
       tags$div( class = 'col-sm-1'),
       tags$div( class = 'col-sm-2',
                 actionButton("save", label = "Save chages",href="")
       ),
       tags$div( class = 'col-sm-2',
                 actionButton("cancel", label = "Cancel",href="")
       )
  )
}

tab_grafics <- function(x, option){
  fluidRow(
    column(width = 12,
           box(width = 12,
               title = x, status = "primary"
               #plotOutput("plot1", height = 300)
           )),
    column(width = 12, 
           option
    )
  )
}

removal_option <- function(){
  fluidRow(
    box(
      title = "Range", width = 6, solidHeader = TRUE,
      background = "aqua",
      sliderInput("atributes", label = "Atributes", min = 0, 
                  max = 100, value = c(40, 60)),
      sliderInput("observation", label = "Observation", min = 0, 
                  max = 100, value = c(40, 60)),
      actionButton("remove", label = "Remove noise")
    ),  
    box(
      title = "Download image", width = 4, solidHeader = TRUE,
      background = "blue",
      radioButtons("radio","",
                   choices = list("PNG" = 1, "SVG" = 2, "PDF" = 3), 
                   selected = 1),
      actionButton("download", label = "Download")
    ),
    #box para los apuntes
    box(
      title = "Notes", width = 4, solidHeader = TRUE,
      background = "light-blue",
      tags$div( class='form-group shiny-input-container',
                tags$textarea("notes...", class="form-control shiny-bound-input", style="resize: none")
      )
    )
  )
}

normalizations <- function(title){
  fluidRow(
      box( width = 12, title = title, solidHeader = TRUE, status = "primary",
         tags$div( class = 'col-sm-4',
                   checkboxInput("x", label = "Normalize X", value = FALSE)
         ),
         tags$div( class = 'col-sm-4',
                   checkboxInput("y", label = "Normalize Y", value = FALSE)
         ),
         tags$div( class = 'col-sm-4',
                   actionButton("normalize", label = "Normalize")
         )
      ), 
      box(width = 12, title ="Maximum by column:", solidHeader = TRUE,
          verbatimTextOutput("summary")
          ),
      box(width = 12, title ="Minimum by column:", solidHeader = TRUE,
          verbatimTextOutput("summary")
      ),
      box(width = 12, title ="Data set:", solidHeader = TRUE,
          verbatimTextOutput("summary")    
      ),
      box(width = 12, title ="Options", solidHeader = TRUE,
          tags$div( class='form-group shiny-input-container',
                    tags$textarea("notes...", class="form-control shiny-bound-input", style="resize: none")
          ),
          tags$div( class = 'col-sm-2',
                    actionButton("save", label = "Save chages",href="")
          ),
          tags$div( class = 'col-sm-2',
                    actionButton("cancel", label = "Cancel",href="")
          )
      )
  )
}

note_download <- function(){
  fluidRow(
    column(width = 12,
        box(width = 12, title ="Results:", solidHeader = TRUE, collapsible = TRUE,
            verbatimTextOutput("sumary")    
        ),
        box(
          title = "Options", width = 12, solidHeader = TRUE,
          background = "blue",
          tags$div( class = 'col-sm-4',
              radioButtons("radio","Download image",
                           choices = list("PNG" = 1, "SVG" = 2, "PDF" = 3), 
                           selected = 1),
              actionButton("download", label = "Download")
          ),
          tags$div( class = 'col-sm-6',
              tags$div( class='form-group shiny-input-container',
                        tags$p("Notes"),
                        tags$textarea("notes...",class="form-control shiny-bound-input", style="resize: none")
              ),
              tags$hr(),
              tags$div( class = 'col-sm-4',
                        actionButton("save", label = "Save chages",href="")
              ),
              tags$div( class = 'col-sm-2',
                        actionButton("cancel", label = "Cancel",href="")
              )
          )
        )
    )
  )
}

colinearity_options <- function(){
  fluidRow(
    box(width = 12, title ="Data set:", solidHeader = TRUE,
        verbatimTextOutput("summary")
    ),
    box(width = 12, title ="Results:", solidHeader = TRUE,
        verbatimTextOutput("summary")    
    ),
    box(width = 12, title ="Options", solidHeader = TRUE,
        tags$div( class='form-group shiny-input-container',
                  tags$textarea("notes...", class="form-control shiny-bound-input", style="resize: none")
        ),
        tags$div( class = 'col-sm-2',
                  actionButton("save", label = "Save chages",href="")
        ),
        tags$div( class = 'col-sm-2',
                  actionButton("cancel", label = "Cancel",href="")
        )
    )
  )
}

oulier_option <- function(value){
  fluidRow(
    column(width = 12,
           box( width = 12, title = "Outlier detection", solidHeader = TRUE,
                tags$div( class = 'col-sm-4',
                          radioButtons("numgraphics", label = "Graphis",
                                       choices = list("Individual" = 1, "All" = 2), 
                                       selected = 1)
                ),
                if(value == 1)
                  tags$div( class = 'col-sm-4',
                            selectInput("select", label = "Residual", 
                                        choices = list("Pearson" = 1, "Standalized" = 2, "Studentized" = 3, 
                                                       "Residue" = 4), 
                                        selected = 1)
                  )
           )
    )
  )
}

