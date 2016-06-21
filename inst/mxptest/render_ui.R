    # user authenticated
    output$sidebarui <- renderUI({
      sidebarPanel(
          #textOutput("usermessage"),
          #br(),
          a("marxan.io user guide", href=paste0("http://marxan.net/downloads/",sUserGuide), target="_blank"),
          br(),
          br(),
          selectInput("database","Dataset:",
                      choices = c(list.dirs(sAppHome)),
                      selected = sSelectDb),
          textOutput("paramtestfeedback"),
          br(),
          actionButton("mptrun","Run"), 
          br(),
          br(),
          radioButtons("displaywhat","Display:",
                       list("Graph","Map")),
          selectInput("whichparam", "Parameter to test:",
                      choices = c("BLM Calibration","SPF Calibration","Target Sensitivity")),
          conditionalPanel(condition = "input.whichparam == 'SPF Calibration' | input.whichparam == 'Target Sensitivity'",
              numericInput("userblm", "BLM:",0,min=0)
          ),
          conditionalPanel(condition = "input.whichparam == 'BLM Calibration' | input.whichparam == 'Target Sensitivity'",
              numericInput("userspf", "SPF:",1,min=0)
          ),
          conditionalPanel(condition = "input.whichparam == 'BLM Calibration' | input.whichparam == 'SPF Calibration'",
              numericInput("usertarg", "Target:",0.3,min=0,max=1)
          ),
          conditionalPanel(condition = "input.whichparam == 'BLM Calibration'",
              numericInput("rampBLMmin", "BLM min:",0,min=0),
              numericInput("rampBLMmax", "BLM max:",10000000000000,min=0)
          ),
          conditionalPanel(condition = "input.whichparam == 'SPF Calibration'",
              numericInput("rampSPFmin", "SPF min:",0.0001,min=0),
              numericInput("rampSPFmax", "SPF max:",10000000000000,min=0)
          ),
          conditionalPanel(condition = "input.whichparam == 'Target Sensitivity'",
              numericInput("targetmin", "Target min:",0,min=0,max=1),
              numericInput("targetmax", "Target max:",1,min=0,max=1)
          ),
          br(),
          selectInput("whichmap", "Value to display:",
              choices = as.character(rep(1:iCores))),
          conditionalPanel(condition = "input.displaywhat == 'Map'",
              selectInput("whichrun", "Map to display:",
                  choices = c("Best Solution",paste0("Run ",rep(1:iRepsPerCore)),"Selection Frequency")),
              # map legends
              conditionalPanel(condition = "input.whichrun == 'Selection Frequency'",
                  HTML("Selection frequency"),
                  br(),
                  HTML("<img src='http://marxan.io/images/blue_5.png' /></a>"),
                  HTML("100"),
                  br(),
                  HTML("<img src='http://marxan.io/images/blue_4.png' /></a>"),
                  HTML("70-99"),
                  br(),
                  HTML("<img src='http://marxan.io/images/blue_3.png' /></a>"),
                  HTML("30-69"),
                  br(),
                  HTML("<img src='http://marxan.io/images/blue_2.png' /></a>"),
                  HTML("1-29"),
                  br(),
                  HTML("<img src='http://marxan.io/images/white.png' /></a>"),
                  HTML("0"),
                  br(),
                  HTML("<img src='http://marxan.io/images/turquoise.png' /></a>"),
                  HTML("Existing Reserves"),
                  br(),
                  HTML("<img src='http://marxan.io/images/grey.png' /></a>"),
                  HTML("Excluded")
              ),
              conditionalPanel(condition = paste0("input.whichrun == 'Best Solution'",paste0(paste0(" | input.whichrun == 'Run ",rep(1:iRepsPerCore),"'"), collapse = '')),
                  HTML("<img src='http://marxan.io/images/white.png' /></a>"),
                  HTML("Available"),
                  br(),
                  HTML("<img src='http://marxan.io/images/blue_5.png' /></a>"),
                  HTML("Selected"),
                  br(),
                  HTML("<img src='http://marxan.io/images/turquoise.png' /></a>"),
                  HTML("Existing Reserve"),
                  br(),
                  HTML("<img src='http://marxan.io/images/grey.png' /></a>"),
                  HTML("Excluded")
              )
          ),
          conditionalPanel(condition = "input.displaywhat == 'Graph'",
              checkboxInput("plotvalues", "Plot Parameter Values", value = TRUE)
          )
      ) # sidebarPanel
    }) # renderUI
    output$mainui <- renderUI({
        mainPanel(
            tableOutput('pttable'),
            conditionalPanel(condition = "input.displaywhat == 'Graph'",
                plotOutput('ptplot')
            ),
            conditionalPanel(condition = "input.displaywhat == 'Map'",
                plotOutput('ptmap')
            )
      )
    })
