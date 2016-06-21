# marxan.io

inputUserid <- function(inputId, value='')
{
    tagList(
        singleton(tags$head(tags$script(src = "js/md5.js", type='text/javascript'))),
        singleton(tags$head(tags$script(src = "js/shinyBindings.js", type='text/javascript'))),
        tags$body(onload="setvalues()"),
        tags$input(id = inputId, class = "userid", value=as.character(value), type="text", style="display:none;")
    )
}

inputIp <- function(inputId, value='')
{
    tagList(
        singleton(tags$head(tags$script(src = "js/md5.js", type='text/javascript'))),
        singleton(tags$head(tags$script(src = "js/shinyBindings.js", type='text/javascript'))),
        tags$body(onload="setvalues()"),
        tags$input(id = inputId, class = "ipaddr", value=as.character(value), type="text", style="display:none;")
    )
}

shinyUI(fluidPage(
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
    
    tags$script('document.title = "Run Marxan";'),
    tags$script(height_jscode),
    tags$script(width_jscode),
    tags$script(resize_jscode),

    uiOutput("sidebarui"),
    uiOutput("mainui"),

    conditionalPanel(condition = "input.prop == -1",
        inputIp("ipid"),
        inputUserid("fingerprint"),
        numericInput("refreshmap", "Refresh Input", 0),
        numericInput("refreshtable", "Refresh Input", 0),
        numericInput("refreshcluster", "Refresh Input", 0),
        numericInput("displayleaflet","Display leaflet",0),
        numericInput("enableleafletmap","Enable leaflet",0),
        numericInput("displaymap","Display map",0),
        numericInput("refreshaspectwidth","Refresh aspect width",0),
        numericInput("refreshaspectheight","Refresh aspect height",0),
        numericInput("refreshscreenheight","Refresh aspect height",0),
        numericInput("refreshmarxanmap","Refresh marxanmap",0),
        numericInput("setscreenheight","setscreenheight",0),
        numericInput("databasechange","Database Change",0)
    )
))
