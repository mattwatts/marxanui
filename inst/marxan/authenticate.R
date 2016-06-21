#### Log in module ###
USER <- reactiveValues(Logged = Logged)

passwdInput <- function(inputId, label)
{
    tagList(
        tags$label(label),
        tags$input(id = inputId, type="password", value="")
    )
}

output$uiBanner <- renderUI({
    if (USER$Logged == FALSE)
    {
        headerPanel(
            HTML("<p><a href='http://marxan.org' target='_blank'><img style='margin: 0px 20px' src='http://marxan.net/logos/MARXANLOGO.png' alt='Marxan logo'></a> <a href='http://www.ceed.edu.au/' target='_blank'><img src='http://marxan.net/logos/CEEDLOGO.png' alt='ARC CEED logo'></a></p> <h3>Marxan.net: Cloud infrastructure for systematic conservation planning</h3>")
            ,windowTitle="Marxan.net"
        )
    }
})

output$uiLogin <- renderUI({
    if (USER$Logged == FALSE)
    {
        wellPanel(
            #textInput("userName", "Username:"),
            #passwdInput("passwd", "Password:"),
            #br(),
            #actionButton("Login", "Log in")
        )
    }
})

output$pass <- renderText({
    if (USER$Logged == FALSE)
    {
        # attempt to authenticate user with session key
        query <- parseQueryString(session$clientData$url_search)
      
        sUrlSessionKey <<- query[1]
      
        cat(paste0("sUrlSessionKey ",sUrlSessionKey,"\n"))
      
        if (AuthenticateUserSession(sUrlSessionKey))
        {
            USER$Logged <- TRUE
            AuthenticateUser("marxan")
            paste0("session >",sUrlSessionKey,"< authenticated")
        } else {
            paste0("session >",sUrlSessionKey,"< invalid")
        }
    }
})
