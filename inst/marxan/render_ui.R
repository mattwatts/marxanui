output$sidebarui <- renderUI({
    sidebarPanel(
        id = "inPanel",
        style = paste0("width:300px; float:left; overflow-y:scroll; max-height: ",generate_screen_height(),"px; position:relative;"),
        #textOutput("usermessage"),
        #br(),
        a("marxan.io user guide",href=paste0("http://marxan.net/downloads/",sUserGuide), target="_blank"),
        br(),
        br(),
        selectInput("database","Dataset:",choices=c(list.dirs(sAppHome)),selected=sSelectDb),
        textOutput("textfeedback"),
        br(),
        actionButton("mrun","Run"),
        br(),
        br(),
        numericInput("blm","BLM:",sBLM,min=0),
        br(),
        radioButtons("displaywhat", "Display:",list("Map"="map","Table"="table","Cluster"="cluster")),
        conditionalPanel(condition = "input.displaywhat == 'table'",
            radioButtons("table", "Table:",
                         list("Conservation Features"="spec","Summary"="sumtable",
                              "Best solution Missing values"="mvbesttable","Solution M Missing values"="mvNtable")),
            conditionalPanel(condition = "input.table == 'spec'",
                br(),
                actionButton("saveBtn", "Save")
            )
        ),
        conditionalPanel(condition = "input.displaywhat == 'map'",
            checkboxInput("enablemap","Enable Map",value=fEnableMap),
            conditionalPanel(condition = "input.displaymap == 0",
                HTML("Warning: map rendering is slow for large polygon sets")
            ),
            conditionalPanel(condition = "input.displaymap == 1",
                conditionalPanel(condition = "input.displayleaflet == 1",
                    checkboxInput("enableleaflet","Enable Leaflet",value=fEnableLeaflet),
                    conditionalPanel(condition = "input.enableleafletmap == 0",
                        HTML("Warning: leaflet map rendering is slower for large polygon sets")
                    ),
                    conditionalPanel(condition="input.enableleafletmap == 1",
                        actionButton("zoomtoextent","Zoom extent"),
                        actionButton("zoomtoprev","Zoom previous"),
                        radioButtons("map_service","Map service:",list("Open Street Map"="OpenStreetMap","ESRI World Imagery"="ESRI")),
                        sliderInput("opacity", "Opacity:",value=0.6,min=0,max=1)
                    )
                ),
                radioButtons("map","Map:",list("Best solution"="bestmap","Solution M"="runMmap","Selection frequency"="ssolnNmap")),
                conditionalPanel(condition = "(input.map == 'bestmap' | input.map == 'runMmap')",
                    HTML("<img src='http://marxan.io/images/blue_5.png' /></a>"),
                    HTML("Selected"),
                    br(),
                    HTML("<img src='http://marxan.io/images/turquoise.png' /></a>"),
                    HTML("Existing Reserve"),
                    br(),
                    HTML("<img src='http://marxan.io/images/grey.png' /></a>"),
                    HTML("Excluded")
                ),
                conditionalPanel(condition = "input.map == 'ssolnNmap'",
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
                    conditionalPanel(condition = "input.displayleaflet == 0",
                        br(),
                        HTML("<img src='http://marxan.io/images/white.png' /></a>"),
                        HTML("0")
                    ),
                    br(),
                    HTML("<img src='http://marxan.io/images/turquoise.png' /></a>"),
                    HTML("Existing Reserves"),
                    br(),
                    HTML("<img src='http://marxan.io/images/grey.png' /></a>"),
                    HTML("Excluded")
                )
            )
        ),
        conditionalPanel(condition = "input.displaywhat == 'cluster'",
            radioButtons("cluster","Cluster:",list("NMDS"="cluster2ds","Dendogram"="clusterdendogram"))
        ),
        conditionalPanel(condition = "(input.map == 'runMmap' & input.displaywhat == 'map') | (input.table == 'mvNtable' & input.displaywhat == 'table')",
            br(),
            sliderInput("m","Solution M:",value=1,min=1,max=100,step=1)
        ),
        br(),
        HTML("Please report all problems to <a href='mailto:m.watts@uq.edu.au?subject=marxan.io problems'>Matt Watts</a>.")
    ) # sidebarPanel
}) # renderUI
output$mainui <- renderUI({
    mainPanel(
        id = "outPanel",
        style = paste0("overflow-y:scroll; max-height: ",generate_table_height(),"px; position:relative;"),
        conditionalPanel(condition = "input.displaywhat == 'table' & input.table == 'spec'",
            rHandsontableOutput("hot")
        ),
        conditionalPanel(condition = "input.displaywhat == 'table' & input.table != 'spec'",
            tableOutput("marxantable")
        ),
        conditionalPanel(condition = "input.displaywhat == 'cluster'",
            plotOutput("marxanplot",width=generate_plot_width(),height=generate_plot_height())
        ),
        conditionalPanel(condition="input.displaywhat == 'map'",
            conditionalPanel(condition="input.displaymap == 1",
                conditionalPanel(condition="(input.displayleaflet == 1) & (input.enableleafletmap == 1)",
                    leafletOutput("leafletmap",width="100%",height=generate_screen_height())
                ),
                conditionalPanel(condition="(input.displayleaflet == 0) | (input.enableleafletmap == 0)",
                    plotOutput("marxanmap",width=generate_aspect_width(),height=generate_aspect_height())
                )
            )
        )
    )
})
