loginRegister <- function(){
  tabBox(
    title = "",
    width = 12,
    # id = "tabset",
    #tab con el inicio de sesion
    tabPanel("login",
             bsAlert("alertLogin"),
             textInput("userName", "Username"),
             passwordInput("passwd", "Password"),
             br(), bsButton("Login","Log in",  style = "success")
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
             br(),bsButton("register","Register Now", style = "success") #submitButton("Register Now")
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
