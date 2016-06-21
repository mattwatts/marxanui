# marxan.io

library(doParallel)
library(foreach)
require(foreign)
library(iptools)
require(labdsv)
library(leaflet)
require(maptools)
require(PBSmapping)
library(png)
library(rhandsontable)
library(rjson)
library(shiny)
require(sp)
require(sqldf)
require(vegan)
require(xtable)

shinyServer(function(input, output, session, clientData) {

    observe({
        sDatabase <<- input$database
    })

    observe({
        sUserIP <<- as.character(input$ipid)
        cat(paste0("sUserIP ",sUserIP,"\n"))
    })

    observe({

        if (!is.null(input$GetScreenWidth))
        if (!is.null(input$GetScreenHeight))
        {
            iHeight <<- input$GetScreenHeight
            iWidth <<- input$GetScreenWidth
            iMapWidth <<- iWidth - 414
            #input$GetResize

            iAspectHeight <<- round(iMapWidth/iAspectX*iAspectY)
            iAspectWidth <<- iMapWidth

            if (iAspectHeight > iHeight)
            {
                iAspectWidth <<- round(iHeight/iAspectY*iAspectX)
                iAspectHeight <<- iHeight
            }

            iPlotWidth <<- iMapWidth
            iPlotHeight <<- iPlotWidth
            if (iPlotWidth > iHeight)
            {
                iPlotHeight <<- iHeight
                iPlotWidth <<- iHeight
            }

            cat(paste0("iAspectX ",iAspectX," iAspectY ",iAspectY,"\n"))
            cat(paste0("aspect width ",iAspectWidth," aspect height ",iAspectHeight,"\n"))

            if (!(fEnableLeaflet & fLeafletRdata))
            {
                # refresh aspect width and height
                irefreshaspectheight <<- irefreshaspectheight + 1
                updateNumericInput(session, "refreshaspectheight", value = irefreshaspectheight)
                irefreshaspectwidth <<- irefreshaspectwidth + 1
                updateNumericInput(session, "refreshaspectwidth", value = irefreshaspectwidth)
            }
        }
    })

    observe({
    
        if (!is.null(input$GetScreenWidth))
        {
            if (iWidthChange == 0)
            {
                iWidth <<- input$GetScreenWidth
                iMapWidth <<- iWidth - 414
                cat(paste0("screen width ",iWidth," map width ",iMapWidth,"\n"))

                iWidthChange <<- 1
            }
        }
    })

    observe({

        if (!is.null(input$GetScreenHeight))
        {
            if (iHeightChange == 0)
            {
                iHeight <<- input$GetScreenHeight
                cat(paste0("screen height ",iHeight,"\n"))

                updateNumericInput(session, "setscreenheight", value = 1)
                iHeightChange <<- 1
            }
        }
    })

    observe({

        if (!is.null(input$GetResize))
        {
            #iWidth <<- input$GetResize$width
            #iMapWidth <<- iWidth - 394
            #iHeight <<- input$GetResize$height
            #cat(paste0("resize screen height ",iHeight," screen width ",iWidth," map width ",iMapWidth,"\n"))
        }
    })

    generate_screen_height <- reactive({

        #input$GetScreenHeight
        #input$GetResize
        input$setscreenheight

        return(iHeight)
    })

    generate_table_height <- reactive({

        #input$GetScreenHeight
        #input$GetResize
        input$setscreenheight

        return(iHeight)
    })

    generate_aspect_height <- reactive({

        input$GetScreenHeight
        #input$GetResize
        input$refreshaspectheight

        cat(paste0("generate_aspect_height iAspectHeight ",iAspectHeight,"\n"))

        iRefreshAspectHeight <<- iAspectHeight

        return(iAspectHeight)
    })

    generate_plot_width <- reactive({

        #input$GetScreenWidth
        input$setscreenheight
        return(iPlotWidth)
    })

    generate_plot_height <- reactive({

        #input$GetScreenHeight
        input$setscreenheight
        return(iPlotHeight)
    })

    generate_aspect_width <- reactive({

        input$GetScreenWidth
        #input$GetResize
        input$refreshaspectwidth

        cat(paste0("generate_aspect_width iAspectWidth ",iAspectWidth,"\n"))

        iRefreshAspectWidth <<- iAspectWidth

        return(iAspectWidth)
    })

    generate_mainPanel_style <- reactive({

        mainPanel_style <<- ""
        if (input$displaywhat == "table")
        {
            mainPanel_style <<- "overflow-y:scroll; "
        }
        return(mainPanel_style)
    })

    observe({
        sFingerprint <<- as.character(input$fingerprint)
        cat(paste0("sFingerprint ",sFingerprint,"\n"))
    })

    values = list()
    setHot = function(x) values[["hot"]] <<- x  

    #source(paste0(sShinySourcePath,"/server_pre_marxan.R"),  local = TRUE)
    marxanui_start("marxan")

    autoInvalidate <- reactiveTimer(2000,session=session)

    observe({

        autoInvalidate()

        # we detect if there are new folders in the users directory, indicating a new database import
        CurrentImportTime <- max(file.info(c(list.dirs(sAppHome,full.names = TRUE)))$ctime)
        if (!(CurrentImportTime == ImportTime))
        {
            # user has imported a new dataset
            cat(paste0("new dataset detected","\n"))
            ImportTime <<- CurrentImportTime

            # update the list of datasets to include the new one(s)
            updateSelectInput(session, "database",
                              choices = c(list.dirs(sAppHome)),
                              selected = sSelectDb)
        }
    })

    observe({

        # render the user interface
        source(paste0(sAppDir,"/render_ui.R"),  local = TRUE)
    })

    session$onSessionEnded(function() {
        stopApp()
    })

    observe({

        input$saveBtn

        if (!is.null(values[["hot"]]))
        {
            # if there are extra rows that have been added by "dragging" the control, remove them
            specdat_edit <- values[["hot"]]
            iEditRows <- nrow(specdat_edit)
    
            if (iSpecDatRows > 0)
            {
                if (iEditRows > iSpecDatRows)
                {
                    # delete the extra rows before saving
                    iRowsToDelete <- iEditRows - iSpecDatRows
                    for (i in 1:iRowsToDelete)
                    {
                        specdat_edit <- specdat_edit[-c(nrow(specdat_edit)),]
                    }
                }
            }

            write.csv(specdat_edit, paste0(sMarxanDir,"/input/spec.dat"),row.names=F,quote=FALSE)
        }
    })

    observe({

        if (!is.null(input$database))
        {
            cat(paste0("change database start ",sSelectDb,"\n"))
            # select this database from the list of databases
            sSelectDb <<- input$database
            cat(paste0("sSelectDb ",sSelectDb,"\n"))
            sPrevious <- sMarxanDir
            sMarxanDir <<- paste0(sAppHome,"/",sSelectDb)
            cat(paste0("sMarxanDir ",sMarxanDir,"\n"))
            AppendLogFile(sLogFile,paste0("sSelectDb ",sSelectDb))
            AppendLogFile(sLogFile,paste0("sMarxanDir ",sMarxanDir))

            if (sPrevious != sMarxanDir)
            {
                if (sSelectDb != "")
                {
                    ChangeDatabase("marxan",session)
                    
                    # compute map aspect ratio 
                    iAspectHeight <<- round(iMapWidth/iAspectX*iAspectY)
                    iAspectWidth <<- iMapWidth
                    if (iAspectHeight > iHeight)
                    {
                        iAspectWidth <<- round(iHeight/iAspectY*iAspectX)
                        iAspectHeight <<- iHeight
                    }
                    cat(paste0("iAspectX ",iAspectX," iAspectY ",iAspectY,"\n"))
                    cat(paste0("aspect width ",iAspectWidth," aspect height ",iAspectHeight,"\n"))

                    # zoom to limits when change database
                    fZoomToLimits <<- TRUE
                    iZoom <<- 0
                    iBounds <<- 0

                    # trigger a refresh of the marxan UI
                    # update the relevant UI components
                    if (fLeafletRdata)
                    {
                        updateNumericInput(session,"displayleaflet",value=1)
                    } else {
                        updateNumericInput(session,"displayleaflet",value=0)
                    }
                    if (!(fEnableLeaflet & fLeafletRdata))
                    {
                        if ((iAspectHeight == iRefreshAspectHeight) & (iAspectWidth == iRefreshAspectWidth))
                        {
                            # refresh marxanmap
                            irefreshmarxanmap <<- irefreshmarxanmap + 1
                            updateNumericInput(session, "refreshmarxanmap", value = irefreshmarxanmap)
                        } else {
                            # refresh aspect width and height
                            irefreshaspectheight <<- irefreshaspectheight + 1
                            updateNumericInput(session, "refreshaspectheight", value = irefreshaspectheight)
                            irefreshaspectwidth <<- irefreshaspectwidth + 1
                            updateNumericInput(session, "refreshaspectwidth", value = irefreshaspectwidth)
                        }
                    }
                    # update BLM
                    updateNumericInput(session,"blm",value=as.numeric(sBLM))
                    # trigger a refresh of the map
                    irefreshmap <<- irefreshmap + 1
                    updateNumericInput(session, "refreshmap", value = irefreshmap)
                    # trigger a refresh of the table
                    irefreshtable <<- irefreshtable + 1
                    updateNumericInput(session, "refreshtable", value = irefreshtable)
                    # trigger a refresh of the cluster
                    irefreshcluster <<- irefreshcluster + 1
                    updateNumericInput(session, "refreshcluster", value = irefreshcluster)
                }
            }
            cat(paste0("change database end ",sSelectDb,"\n"))
        }
    })

    output$hot = renderRHandsontable({

        cat(paste0("hot renderRHandsontable sSelectDb ",sSelectDb,"\n"))

        if (!is.null(input$database))
        {
            if (!is.null(input$hot))
            {
                DF = hot_to_r(input$hot)
            } else {
                DF = read.csv(paste0(sMarxanDir,"/input/spec.dat"),stringsAsFactors=FALSE)
                iSpecDatRows <<- nrow(DF)
                DF$spf <- as.numeric(DF$spf)
                fSaveHot <<- TRUE
                if (iSpecDatRows > 900)
                {
                    fSaveHot <<- FALSE
                    DF <- as.data.frame(c("table too large"))
                }
            }

            setHot(DF)
            rhandsontable(DF, readOnly = T) %>%
            hot_table(highlightCol = TRUE, highlightRow = TRUE) %>%
            hot_col(c("prop","spf"), readOnly = FALSE) %>%
            hot_cols(renderer = "
                function (instance, td, row, col, prop, value, cellProperties)
                {
                    Handsontable.renderers.TextRenderer.apply(this, arguments);
                    if (col == 1 && (value > 1.0 || value < 0.0))
                    {
                        td.style.background = 'red';
                    }
                }"
            )
        }
    })

    observe({

        cat("m\n")

        if (!is.null(input$m))
        if (input$m > 0)
        {
            iM <<- input$m
            cat(paste0("iM ",iM,"\n"))
            AppendLogFile(sLogFile,paste0("input$m ",input$m))
        }
    })

    observe({

        cat("blm\n")

        if (!is.null(input$blm))
        {
            rblm <<- input$blm
            cat(paste0("rblm ",rblm,"\n"))
            AppendLogFile(sLogFile,paste0("input$blm ",input$blm))
        }
    })

    runclicked <- reactive({

        cat(paste0("mrun\n"))

        if (!is.null(input$mrun))
        if (input$mrun > 0)
        {
            #fMarxanRunning <<- TRUE
            ptm <- proc.time()

            cat(paste0("click mrun ",input$mrun,"\n"))

            RunMarxan_app()

            # trigger a refresh of the UI
            if (!(fEnableLeaflet))
            {
                # refresh marxanmap
                irefreshmarxanmap <<- irefreshmarxanmap + 1
                updateNumericInput(session, "refreshmarxanmap", value = irefreshmarxanmap)
            }
            # trigger a refresh of the map
            irefreshmap <<- irefreshmap + 1
            updateNumericInput(session, "refreshmap", value = irefreshmap)
            # trigger a refresh of the cluster
            irefreshcluster <<- irefreshcluster + 1
            updateNumericInput(session, "refreshcluster", value = irefreshcluster)
            # trigger a refresh of the table
            irefreshtable <<- irefreshtable + 1
            updateNumericInput(session, "refreshtable", value = irefreshtable)

            AppendLogFile(sLogFile,paste0("input$mrun ",input$mrun," elapsed ",(proc.time() - ptm)[3]))
            #fMarxanRunning <<- FALSE
        }
        return(0)
    })

    output$marxanplot <- renderPlot({

        input$refreshcluster

        if (!is.null(input$refreshcluster))
        {
            AppendLogFile(sLogFile,paste0("output$marxancluster ",sdisplaywhat," ",input$cluster))

            withProgress(message="Rendering cluster",value=0,
           {
                if (input$cluster == "cluster2ds") { cluster_2ds("marxan",FALSE) }
                if (input$cluster == "clusterdendogram") { cluster_dendogram("marxan") }
           })
        }
    }) # renderPlot

    observe({
        if (!is.null(input$enableleaflet))
        {
            fEnableLeaflet <<- input$enableleaflet
            cat(paste0("fEnableLeaflet ",fEnableLeaflet,"\n"))
            if (fEnableLeaflet)
            {
                updateNumericInput(session,"enableleafletmap",value=1)
            } else {
                updateNumericInput(session,"enableleafletmap",value=0)
                # refresh aspect width and height
                irefreshaspectheight <<- irefreshaspectheight + 1
                updateNumericInput(session, "refreshaspectheight", value = irefreshaspectheight)
                irefreshaspectwidth <<- irefreshaspectwidth + 1
                updateNumericInput(session, "refreshaspectwidth", value = irefreshaspectwidth)
            }
        }
    })

    observe({
        if (!is.null(input$enablemap))
        {
            fEnableMap <<- input$enablemap
            cat(paste0("fEnableMap ",fEnableMap,"\n"))
            if (fEnableMap)
            {
                updateNumericInput(session,"displaymap",value=1)
                if (!fEnableLeaflet)
                {
                    # refresh aspect width and height
                    irefreshaspectheight <<- irefreshaspectheight + 1
                    updateNumericInput(session, "refreshaspectheight", value = irefreshaspectheight)
                    irefreshaspectwidth <<- irefreshaspectwidth + 1
                    updateNumericInput(session, "refreshaspectwidth", value = irefreshaspectwidth)
                }
            } else {
                updateNumericInput(session,"displaymap",value=0)
            }
        }
    })

    observe({
        input$zoomtoextent

        cat(paste0("input$zoomtoextent ",input$zoomtoextent,"\n"))

        if (!is.null(input$zoomtoextent))
        if (input$zoomtoextent > 0)
        {
            if (fLeafletRdata)
            if (fLeafletGenerated)
            {
                # zoom to limits
                leafletProxy("leafletmap", session) %>% setView(((first_bounds$east+first_bounds$west)/2),
                                                                ((first_bounds$north+first_bounds$south)/2),
                                                                zoom = iFirstZoom)
            }
        }
    })

    observe({
        input$zoomtoprev

        cat(paste0("input$zoomtoprev ",input$zoomtoprev,"\n"))

        if (!is.null(input$zoomtoprev))
        if (input$zoomtoprev > 0)
        if (iZoom > 0)
        {
            if (fLeafletRdata)
            if (fLeafletGenerated)
            {
                # zoom to previous
                leafletProxy("leafletmap", session) %>% setView(((prev_bounds$east+prev_bounds$west)/2),
                                                                ((prev_bounds$north+prev_bounds$south)/2),
                                                                zoom = iPrevZoom)
            }
        }
    })

    output$marxanmap <- renderPlot({

        input$refreshmarxanmap
        input$m

        cat("marxanmap\n")

        fDisplay <- TRUE

        if (fLeafletRdata)
        {
            if (input$enableleaflet) { fDisplay <- FALSE }
        }

        if (!input$enablemap) { fDisplay <- FALSE }

        if (fDisplay)
        {
            AppendLogFile(sLogFile,paste0("output$marxanplot ",sdisplaywhat," ",input$map," ",input$m))

            withProgress(message="Rendering map",value=0,
            {
                if (input$map == "ssolnNmap") { map_ssolnNmap() }
                if (input$map == "bestmap") { map_bestmap() }
                if (input$map == "runMmap") { map_runMmap() }

                if (!is.na(puoutline))
                {
                    addLines(puoutline,col="black")
                }
            })
        } else {
            plot(1,1,xlab="",ylab="",col="white",axes=FALSE)
        }
    }) # renderPlot

    observeEvent(input$leafletmap_bounds, {

        if (iBounds > 0) { prev_bounds <<- map_bounds }

        map_bounds <<- input$leafletmap_bounds
        cat(paste0("bounds north ",map_bounds$north," east ",map_bounds$east," south ",map_bounds$south," west ",map_bounds$west,"\n"))

        if (iBounds == 0)
        {
            first_bounds <<- map_bounds
            prev_bounds <<- map_bounds
        }

        iBounds <<- iBounds + 1
    })

    observeEvent(input$leafletmap_zoom, {

        if (iZoom > 0) { iPrevZoom <<- map_zoom }

        map_zoom <<- input$leafletmap_zoom
        cat(paste0("zoom ",map_zoom,"\n"))

        if (iZoom == 0)
        {
            iFirstZoom <<- map_zoom
            iPrevZoom <<- map_zoom # initialise this so no error if user clicks prev zoom
        }
        
        iZoom <<- iZoom + 1
    })

    output$leafletmap <- renderLeaflet({

        input$refreshmap
        input$m

        cat(paste0("leafletmap input$refreshmap ",input$refreshmap,"\n"))

        if (!is.null(input$refreshmap))
        {
            fDisplay <- FALSE

            if (fLeafletRdata)
            {
                if (input$enableleaflet) { fDisplay <- TRUE }
            }

            if (!input$enablemap) { fDisplay <- FALSE }

            if (fDisplay)
            {
                AppendLogFile(sLogFile,paste0("output$leafletmap ",sdisplaywhat," ",input$map," ",input$m))

                withProgress(message="Rendering map",value=0,
                {
                    if (input$map == "ssolnNmap") { map_ssolnNmap_leaflet() }
                    if (input$map == "bestmap") { map_bestmap_leaflet() }
                    if (input$map == "runMmap") { map_runMmap_leaflet() }

                    if (fZoomToLimits)
                    {
                        sZoomToLimits <<- "always"
                    } else {
                        sZoomToLimits <<- "first"
                    }
                    fZoomToLimits <<- FALSE
                
                    fLeafletGenerated <<- TRUE
                    if (input$map_service == "ESRI")
                    {
                        leaflet() %>%
                        addTiles(urlTemplate="http://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}") %>%
                        addPolygons(data=leaflet_proj10,stroke=FALSE,fill=TRUE,fillColor=leaflet_colours,fillOpacity=input$opacity) %>%
                        mapOptions(zoomToLimits = sZoomToLimits)
                    } else {
                        leaflet() %>%
                        addTiles() %>%
                        addPolygons(data=leaflet_proj10,stroke=FALSE,fill=TRUE,fillColor=leaflet_colours,fillOpacity=input$opacity) %>%
                        mapOptions(zoomToLimits = sZoomToLimits)
                    }
                })
            }
        }
    }) # renderLeaflet

    output$marxantable <- renderTable({

        input$refreshtable

        cat(paste0("marxantable sSelectDb ",sSelectDb,"\n"))

        if (input$table == "sumtable")
        {
            sFilename <- paste(sMarxanDir,"/output/output_sum.csv",sep="")
            thetable <- read.csv(sFilename)
            thetable <- round(sqldf("SELECT Score, Cost, Planning_Units, Penalty, Shortfall from thetable"))
            iBest <- which.min(thetable[,1])
            Run <- c()
            for (j in 1:nrow(thetable))
            {
                if (j == iBest)
                {
                    Run <- c(Run,"Best")
                } else {
                    Run <- c(Run,j)
                }
            }

            thetable <- cbind(Run,thetable)#,
            thetable$Run <- as.character(thetable$Run)
            thetable$Run <- as.character(thetable$Run)
        }
        if ((input$table == "mvbesttable") | (input$table == "mvNtable"))
        {
            if (input$table == "mvbesttable")
            {
                sFilename <- paste(sMarxanDir,"/output/output_sum.csv",sep="")
                thetable <- read.csv(sFilename)
                iTable <- which.min(thetable$Score)[1]
            }
            if (input$table == "mvNtable")
            {
                iTable <- input$m
            }

            sFilename <- paste0(sMarxanDir,"/output/output_mv",PadInt(iTable),".csv")
            thetable <- read.csv(sFilename,stringsAsFactors=FALSE)
            # sort the table the way spec.dat is ordered
            tableorder <- seq.int(from=nrow(thetable),to=1)
            thetable <- thetable[tableorder,]
            # select just fields we want
            colnames(thetable)[4] <- "AmountHeld"
            colnames(thetable)[9] <- "TargetMet"
            thetable <- sqldf(paste0("SELECT Target, AmountHeld, TargetMet from thetable"))
            # load feature name from spec.dat
            specdat <- read.csv(paste0(sMarxanDir,"/input/spec.dat"),stringsAsFactors=FALSE)
            if ('name' %in% specdat)
            {
                name <- sqldf(paste0("SELECT name from specdat"))
            } else {
                name <- sqldf(paste0("SELECT id from specdat"))
                colnames(name)[1] <- "name"
            }
            # join the name
            thetable <- cbind(name,thetable)
            # select the fields we want from total areas file
            tafile <- read.csv(paste0(sMarxanDir,"/core1/MarOptTotalAreas.csv"))
            tafile <- tafile[tableorder,]
            tafile <- sqldf(paste0("SELECT totalarea,reservedarea from tafile"))
            # compute target gap
            targetgap <- rep(0,each=nrow(thetable))
            for (i in 1:nrow(thetable))
            {
                if (thetable$Target[i] > 0)
                {
                    if (thetable$AmountHeld[i] < thetable$Target[i])
                    {
                        targetgap[i] <- thetable$Target[i] - thetable$AmountHeld[i]
                    }
                }
            }
            # join and tidy the table
            thetable <- cbind(thetable,tafile,targetgap)
            thetable <- sqldf(paste0("SELECT name, totalarea, reservedarea, Target, AmountHeld, TargetMet, targetgap from thetable"))
            colnames(thetable)[2] <- "Total"
            colnames(thetable)[3] <- "Reserved"
            colnames(thetable)[7] <- "TargetGap"
        }

        return(thetable)
    })

    output$textfeedback = renderText({
        runclicked()
        sprintf("Finished")
    })

    observe({
        sUserIP <<- as.character(input$ipid)
        UserGeoIP <<- freegeoip(sUserIP)
        Hostname <- ip_to_hostname(sUserIP)
        sUserHostname <<- Hostname[[1]]
    })

    output$userLocation <- renderText({
        paste0("Login from ",sUserHostname)
    })

    observe({

        # User has logged in. Record details about the HTTP session.
        query <- parseQueryString(session$clientData$url_search)
        sText <- paste0("fingerprint: ", input$fingerprint,"\n",
                        "ip: ", sUserIP,"\n",
                        "userhostname: ",sUserHostname,"\n",
                        "protocol: ", session$clientData$url_protocol, "\n",
                        "hostname: ", session$clientData$url_hostname, "\n",
                        "pathname: ", session$clientData$url_pathname, "\n",
                        "port: ",     session$clientData$url_port,     "\n",
                        "search: ",   session$clientData$url_search,   "\n",
                        "queries: ",paste(names(query), query, sep = "=", collapse=", "),"\n")

        AppendLogFile(sLogFile,sText)
        cat(paste0(sText,"\n"))
    })

    observe({

        cdata <- session$clientData

        fLeafletHidden <<- cdata$output_leafletmap_hidden
        fMarxanHidden <<- cdata$output_marxanmap_hidden

        #cnames <- names(cdata)

        #allvalues <- lapply(cnames, function(name)
        #{
        #    paste(name, cdata[[name]], sep=" = ")
        #}) 

        cat("session$clientData\n")
        #cat(paste0(paste(allvalues, collapse = "\n"),"\n"))
    })
})
