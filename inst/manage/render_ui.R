# user authenticated
output$sidebarui <- renderUI({
    sidebarPanel(
        a("marxan.io user guide", href=paste0("http://marxan.net/downloads/",sUserGuide), target="_blank"),
        br(),
        br(),
        #conditionalPanel(condition = "input.tabs == 'My data'",
            selectInput("database","Dataset:",
                        choices = c(list.dirs(sMarxanHome),list.dirs(sMarZoneHome)),
                        selected = sSelectDb),
            conditionalPanel(condition = "input.renamemydata == '0'",
                actionButton("renameData","Rename")
            ),
            conditionalPanel(condition = "input.renamemydata == '1'",
                textInput("renameName","New name for dataset"),
                actionButton("acceptName","Ok"),
                actionButton("cancelName","Cancel")
            ),
            br(),
            downloadButton('downloadData', 'Export'),
            checkboxInput("windowseoln", "Windows text files", value = fWindows),
            br(),
            conditionalPanel(condition = "input.areyousure == '1'",
                actionButton("yesimsure","Delete: are you sure?"),
                actionButton("cancelDelete","Cancel")
            ),
            conditionalPanel(condition = "input.areyousure == '0'",
                actionButton("deletedb","Delete")
            ),
            br()
        #)
    ) # sidebarPanel
}) # renderUI
output$mainui <- renderUI({
    mainPanel(
        #tabsetPanel(id="tabs",
        #    tabPanel("My data",tableOutput("mydatatable"))
        #)
        tableOutput("mydatatable")
    )
})
