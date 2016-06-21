# marxan.io

inputUserid <- function(inputId, value='')
{
    #   print(paste(inputId, "=", value))
    tagList(
        singleton(tags$head(tags$script(src = "js/md5.js", type='text/javascript'))),
        singleton(tags$head(tags$script(src = "js/shinyBindings.js", type='text/javascript'))),
        tags$body(onload="setvalues()"),
        tags$input(id = inputId, class = "userid", value=as.character(value), type="text", style="display:none;")
    )
}

inputIp <- function(inputId, value=''){
    tagList(
        singleton(tags$head(tags$script(src = "js/md5.js", type='text/javascript'))),
        singleton(tags$head(tags$script(src = "js/shinyBindings.js", type='text/javascript'))),
        tags$body(onload="setvalues()"),
        tags$input(id = inputId, class = "ipaddr", value=as.character(value), type="text", style="display:none;")
    )
}

shinyUI(bootstrapPage(
    # Add custom CSS & Javascript;
    tagList(
        tags$head(
            tags$link(rel="stylesheet", type="text/css",href="style.css"),
            tags$script(type="text/javascript", src = "js/md5.js"),
            tags$script(type="text/javascript", src = "js/passwdInputBinding.js")
        )
    ),

    # displays banner icons and text
    div(class = "banner",
        uiOutput("uiBanner")
    ),

    ## Login module;
    div(class = "login",
        uiOutput("uiLogin"),
        textOutput("pass")
    ),

    tags$script('document.title = "Import";'),
    #tags$script(close_jscode),

    div(class = "span4",uiOutput("sidebarui")),
    div(class = "span8", uiOutput("mainui")),

    conditionalPanel(condition = "input.prop == -1",
        inputIp("ipid"),
        inputUserid("fingerprint"),
        numericInput("showacceptcontrols","Accept Controls",0),
        numericInput("showacceptfeedback","Accept Feedback",0),
        numericInput("selectpuid","Select puid",0),
        numericInput("parseinputfiles","Parse input files",0),
        numericInput("parsepolygons","Parse input files",0),
        numericInput("updateusermessages","Update input messages",0),
        numericInput("updatepuidchoices","Update puid choices",0)
    )
))
