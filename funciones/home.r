loginRegister <- function(){
  tabBox(
    title = "Login",
    width = 12,
    # id = "tabset",
    #tab con el inicio de sesion
    tabPanel("login",
             bsAlert("alertLogin"),
             textInput("userName", "Username"),
             passwordInput("passwd", "Password"),
             br(),actionButton("Login","Log in")
    ),
    #tab con el registro de usuario
    tabPanel("Register",
             bsAlert("alertRegister"),
             textInput("newUserName", "*Username"),
             textInput("name", "*Name"),
             textInput("lastName", "*Last name"),
             textInput("email", "*Email Address"),
             passwordInput("newPasswd", "*Password"),
             passwordInput("confirmPasswd", "*Confirm Password"),
             br(),actionButton("register","Register Now") #submitButton("Register Now")
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
