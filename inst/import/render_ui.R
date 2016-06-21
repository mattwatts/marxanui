    # user authenticated
    output$sidebarui <- renderUI({
        sidebarPanel(
            #textOutput("usermessage"),
            #br(),
            a("marxan.io user guide", href=paste0("http://marxan.net/downloads/",sUserGuide), target="_blank"),
            br(),
            br(),
            fileInput('file1', 'Choose Marxan zip file to import',accept = c('.zip')),
            HTML("Import a zip file containing your Marxan dataset and planning unit shapefile."),
            br(),
            HTML("When you import a file, there is a delay while the dataset is processed. A blue info box appears in top right of screen during processing. When it's ready an information grid will appear.")#,
            #actionButton("stop_app","Close app")
        ) # sidebarPanel
    }) # renderUI
    output$mainui <- renderUI({
        mainPanel(
            tableOutput('fileupload'),
            tableOutput('usermessages'),
            conditionalPanel(condition="input.selectpuid == 1",
                selectInput("puid","Choose PUID field",choices=puid_field_choices()),
                actionButton("selectpuidbtn","Select PUID"),
                actionButton("cancelpuidbtn","Cancel")
            ),
            conditionalPanel(condition = "input.showacceptcontrols == 1",
                br(),
                textInput("uploadname","Database Name:",value=""),
                actionButton("acceptupload","Accept Database"),
                actionButton("cancelupload","Cancel"),
                br(),
                br(),
                HTML("Give your analysed database a name and accept it for it to stored and made available for use."),
                textOutput("feedbackupload")
            )
        )
    })
