
home <- function(){
  fluidRow(
    column(12,
       #imageOutput("home")
       titlePanel("Welcome to Güiña!"),
       sidebarLayout(
         sidebarPanel(p("The Güiña is a small cat that is endemic from the evergreen forest of southern 
                        Chile. This smart predator relies on its senses to identify and capture the prey, 
                        usually sheltered in the dense and obscure forest."),
                      p("This clever feline served us as inspiration to build a data mining tools for 
                        visualizing an analyzing data. From our perspective, the data miner acts as a 
                        furtive predator of precious information hidden in the dark data forest."),
                      p("Coincidentally the name Güiña begins with the three letters GUI which also 
                        stands for the acronym for Graphical User Interface (GUI).")
                      ),
         mainPanel(
           fluidRow(
             imageOutput("home"),
             uiOutput("signIn")
           )
         )
      )
    )
  )
}

loginRegister <- function(){
  tabBox(
    title = "Login",
    width = 12,
    # id = "tabset",
    #tab con el inicio de sesion
    tabPanel("login",
             textInput("userName", "Username"),
             passwordInput("passwd", "Password"),
             br(),actionButton("Login","Log in")
    ),
    #tab con el registro de usuario
    tabPanel("Register",
             textInput("newUserName", "Username"),
             textInput("email", "Email Address"),
             passwordInput("newPasswd", "Password"),
             passwordInput("confirmPasswd", "Confirm Password"),
             br(),submitButton("Register Now")
    )
  )
}

get_role <- function(user){
  if(user=="test") {
    return("TEST")
  }else{
    return("ADMIN")
  }
}
