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
