# marxan.io

library(doParallel)
library(foreach)
require(foreign)
library(gplots)
library(Hmisc)
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
        {
            iWidth <<- input$GetScreenWidth
            iMapWidth <<- iWidth - 414
            cat(paste0("screen width ",iWidth," map width ",iMapWidth,"\n"))
        }
    })

    observe({
    
        if (!is.null(input$GetScreenHeight))
        {
            iHeight <<- input$GetScreenHeight
            cat(paste0("screen height ",iHeight,"\n"))
        }
    })

    observe({
    
        if (!is.null(input$GetResize))
        {
            #iWidth <<- input$GetResize$width
            #iMapWidth <<- iWidth - 404
            #iHeight <<- input$GetResize$height
            #cat(paste0("resize screen height ",iHeight," screen width ",iWidth," map width ",iMapWidth,"\n"))
        }
    })

    observe({

        if (!is.null(input$GetScreenWidth))
        if (!is.null(input$GetScreenHeight))
        {
            input$GetScreenHeight
            input$GetScreenWidth
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
        
            updateNumericInput(session, "setscreenheight", value = 1)
            
            cat(paste0("iAspectX ",iAspectX," iAspectY ",iAspectY,"\n"))
            cat(paste0("aspect width ",iAspectWidth," aspect height ",iAspectHeight,"\n"))
            
            if (!(fEnableLeaflet & fLeafletRdata))
            {
                # refresh aspect width and height
                inc(irefreshaspectheight) <<- 1
                updateNumericInput(session, "refreshaspectheight", value = irefreshaspectheight)
                inc(irefreshaspectwidth) <<- 1
                updateNumericInput(session, "refreshaspectwidth", value = irefreshaspectwidth)
            }
        }
    })

    generate_screen_height <- reactive({

        input$GetScreenHeight
        #input$GetResize
        
        return(iHeight)
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

    generate_aspect_height <- reactive({

        input$GetScreenHeight
        #input$GetResize
        input$refreshaspectheight
        
        cat(paste0("generate_aspect_height iAspectHeight ",iAspectHeight,"\n"))
        
        iRefreshAspectHeight <<- iAspectHeight

        return(iAspectHeight)
    })

    generate_aspect_width <- reactive({

        input$GetScreenWidth
        #input$GetResize
        input$refreshaspectwidth
        
        cat(paste0("generate_aspect_width iAspectWidth ",iAspectWidth,"\n"))
        
        iRefreshAspectWidth <<- iAspectWidth

        return(iAspectWidth)
    })

    observe({
        sFingerprint <<- as.character(input$fingerprint)
        cat(paste0("sFingerprint ",sFingerprint,"\n"))
    })

    values = list()
    setHot = function(x) values[["hot"]] <<- x  
    values_zonecost = list()
    setHot_zonecost = function(x) values_zonecost[["hot_zonecost"]] <<- x  
    values_zonetarget = list()
    setHot_zonetarget = function(x) values_zonetarget[["hot_zonetarget"]] <<- x  
    values_zonecontrib = list()
    setHot_zonecontrib = function(x) values_zonecontrib[["hot_zonecontrib"]] <<- x  
    values_zoneboundcost = list()
    setHot_zoneboundcost = function(x) values_zoneboundcost[["hot_zoneboundcost"]] <<- x  

    #source(paste0(sShinySourcePath,"/server_pre_marxan.R"),  local = TRUE)
    marxanui_start("marzone")

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
            #updateSelectInput(session, "database",choices = c(list.dirs(sAppHome)),selected = sSelectDb)
            updateSelectInput(session, "database",choices = c(list.dirs(sAppHome)))
        }
    })

    observe({

        if (!is.null(input$database))
        {
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
                    ChangeDatabase("marzone",session)

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
                            inc(irefreshmarxanmap) <<- 1
                            updateNumericInput(session, "refreshmarxanmap", value = irefreshmarxanmap)
                        } else {
                            # refresh aspect width and height
                            inc(irefreshaspectheight) <<- 1
                            updateNumericInput(session, "refreshaspectheight", value = irefreshaspectheight)
                            inc(irefreshaspectwidth) <<- 1
                            updateNumericInput(session, "refreshaspectwidth", value = irefreshaspectwidth)
                        }
                    }
                    # trigger a refresh of the map
                    inc(irefreshmap) <<- 1
                    updateNumericInput(session, "refreshmap", value = irefreshmap)
                    # trigger a refresh of the table
                    inc(irefreshtable) <<- 1
                    updateNumericInput(session, "refreshtable", value = irefreshtable)
                    # trigger a refresh of the cluster
                    inc(irefreshcluster) <<- 1
                    updateNumericInput(session, "refreshcluster", value = irefreshcluster)
                }
            }
        }
    })

    generate_ssoln_html_legend <- reactive({

        # generates map legend for marzone selection frequency as HTML
        input$database

        legend_text <- c()
        for (i in 1:iZones)
        {
            legend_text <- c(legend_text,paste0("<img src='http://",sAppServer,"/images/rainbow_",iZones,"_",i,".png' /></a>"))
            legend_text <- c(legend_text,paste0("&nbsp;",ZoneNames[i]))
            if (i != iZones)
            {
                legend_text <- c(legend_text,"<br>")
            }
        }
        return(paste(legend_text,collapse=''))
    })

    generate_input_files_list <- reactive({
        input$database

        if (sMarxanDir == "")
        {
            c("feat","zones","costs","zonecost","zonebound","zonecontrib","zonetarget")
        } else {
            c(input_list)
        }
    })

    observe({

        # render the user interface
        source(paste0(sAppDir,"/render_ui.R"),  local = TRUE)
    }) # observe

    session$onSessionEnded(function() {
        stopApp()
    })

    observe({

        input$saveSpecBtn

        cat(paste0("input$saveSpecBtn ",input$saveSpecBtn,"\n"))

        if (!is.null(input$saveSpecBtn))
        if (input$saveSpecBtn > 0)
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

            cat(paste0("write.csv ",paste0(sMarxanDir,"/input/spec.dat"),"\n"))

            write.csv(specdat_edit, paste0(sMarxanDir,"/input/spec.dat"),row.names=F,quote=FALSE)
        }
    })

    observe({
        input$saveZoneCostBtn

        cat(paste0("input$saveZoneCostBtn ",input$saveZoneCostBtn,"\n"))

        if (!is.null(input$saveZoneCostBtn))
        if (input$saveZoneCostBtn > 0)
        if (!is.null(values_zonecost[["hot_zonecost"]]))
        {
            # if there are extra rows that have been added by "dragging" the control, remove them
            zonecostdat_edit <- values_zonecost[["hot_zonecost"]]
            iEditRows <- nrow(zonecostdat_edit)

            if (iZC_Rows > 0)
            {
                if (iEditRows > iZC_Rows)
                {
                    # delete the extra rows before saving
                    iRowsToDelete <- iEditRows - iZC_Rows
                    for (i in 1:iRowsToDelete)
                    {
                        zonecostdat_edit <- zonecostdat_edit[-c(nrow(zonecostdat_edit)),]
                    }
                }
            }

            # write the zonecost.dat file
            sZCfile <- paste0(sMarxanDir,"/input/zonecost.dat")
            cat(paste0("write file ",sZCfile,"\n"))
            cat(paste0("write file costs ",nrow(zonecostdat_edit)," zones ",ncol(zonecostdat_edit),"\n"))
            write("zoneid,costid,multiplier",file=sZCfile)
            for (i in 2:ncol(zonecostdat_edit))
            {
                for (j in 1:nrow(zonecostdat_edit))
                {
                    rMultiplier <- zonecostdat_edit[j,i]
                    if (rMultiplier > 0)
                    {
                        # we only save the portion of the matrix above and to the right of the identity row
                        # row,col
                        write(paste0((i-1),",",j,",",rMultiplier),file=sZCfile,append=TRUE)
                    }
                }
            }
        }
    })

    observe({
        input$saveZoneTargetBtn

        cat(paste0("input$saveZoneTargetBtn ",input$saveZoneTargetBtn,"\n"))

        if (!is.null(input$saveZoneTargetBtn))
        if (input$saveZoneTargetBtn > 0)
        if (!is.null(values_zonetarget[["hot_zonetarget"]]))
        {
            # if there are extra rows that have been added by "dragging" the control, remove them
            zonetargetdat_edit <- values_zonetarget[["hot_zonetarget"]]
            iEditRows <- nrow(zonetargetdat_edit)

            if (iZT_Rows > 0)
            {
                if (iEditRows > iZT_Rows)
                {
                    # delete the extra rows before saving
                    iRowsToDelete <- iEditRows - iZT_Rows
                    for (i in 1:iRowsToDelete)
                    {
                        zonetargetdat_edit <- zonetargetdat_edit[-c(nrow(zonetargetdat_edit)),]
                    }
                }
            }

            if (fZoneTarget)
            {
                # write the zonetarget.dat file
                sZTfile <- paste0(sMarxanDir,"/input/zonetarget.dat")
                cat(paste0("write file ",sZTfile,"\n"))
                cat(paste0("write file targets ",nrow(zonetargetdat_edit)," zones ",ncol(zonetargetdat_edit),"\n"))
                # Note: only works for targettype 1
                write("zoneid,featureid,target,targettype",file=sZTfile)
                for (i in 2:ncol(zonetargetdat_edit))
                {
                    for (j in 1:nrow(zonetargetdat_edit))
                    {
                        # row,col
                        rTarget <- zonetargetdat_edit[j,i]
                        if (rTarget > 0)
                        {
                            write(paste0((i-1),",",j,",",rTarget,",1"),file=sZTfile,append=TRUE)
                        }
                    }
                }
            }
            if (fZoneTarget2)
            {
                # write the zonetarget2.dat file
                sZTfile <- paste0(sMarxanDir,"/input/zonetarget2.dat")
                cat(paste0("write file ",sZTfile,"\n"))
                cat(paste0("write file zones ",ncol(zonetargetdat_edit),"\n"))
                # Note: only works for targettype 1
                write("zoneid,target,targettype",file=sZTfile)
                for (i in 2:ncol(zonetargetdat_edit))
                {
                    # row,col
                    rTarget <- zonetargetdat_edit[1,i]
                    if (rTarget > 0)
                    {
                        write(paste0((i-1),",",rTarget,",1"),file=sZTfile,append=TRUE)
                    }
                }
            }
        }
    })

    observe({
        input$saveZoneContribBtn

        cat(paste0("input$saveZoneContribBtn ",input$saveZoneContribBtn,"\n"))

        if (!is.null(input$saveZoneContribBtn))
        if (input$saveZoneContribBtn > 0)
        if (!is.null(values_zonecontrib[["hot_zonecontrib"]]))
        {
            # if there are extra rows that have been added by "dragging" the control, remove them
            zonecontribdat_edit <- values_zonecontrib[["hot_zonecontrib"]]
            iEditRows <- nrow(zonecontribdat_edit)

            if (iZoneContribRows > 0)
            {
                if (iEditRows > iZoneContribRows)
                {
                    # delete the extra rows before saving
                    iRowsToDelete <- iEditRows - iZoneContribRows
                    for (i in 1:iRowsToDelete)
                    {
                        zonecontribdat_edit <- zonecontribdat_edit[-c(nrow(zonecontribdat_edit)),]
                    }
                }
            }

            # write the zonecontrib.dat file
            if (fZoneContrib)
            {
                sZCfile <- paste0(sMarxanDir,"/input/zonecontrib.dat")
                cat(paste0("write file ",sZCfile,"\n"))
                cat(paste0("write file contribs ",nrow(zonecontribdat_edit)," zones ",ncol(zonecontribdat_edit)-1,"\n"))
                write("zoneid,featureid,fraction",file=sZCfile)
                for (i in 2:ncol(zonecontribdat_edit))
                {
                    for (j in 1:nrow(zonecontribdat_edit))
                    {
                        # row,col
                        rFraction <- zonecontribdat_edit[j,i]
                        if (rFraction > 0)
                        {
                            write(paste0((i-1),",",j,",",rFraction),file=sZCfile,append=TRUE)
                        }
                    }
                }
            }
            if (fZoneContrib2)
            {
                sZCfile <- paste0(sMarxanDir,"/input/zonecontrib2.dat")
                cat(paste0("write file ",sZCfile,"\n"))
                cat(paste0("write file contribs ",nrow(zonecontribdat_edit)," zones ",ncol(zonecontribdat_edit),"\n"))
                write("zoneid,fraction",file=sZCfile)
                for (i in 2:ncol(zonecontribdat_edit))
                {
                    # row,col
                    rFraction <- zonecontribdat_edit[1,i]
                    if (rFraction > 0)
                    {
                        write(paste0((i-1),",",rFraction),file=sZCfile,append=TRUE)
                    }
                }
            }
        }
    })

    observe({

        input$saveZoneBoundCostBtn

        cat(paste0("input$saveZoneBoundCostBtn ",input$saveZoneBoundCostBtn,"\n"))

        if (!is.null(input$saveZoneBoundCostBtn))
        if (input$saveZoneBoundCostBtn > 0)
        if (!is.null(values_zoneboundcost[["hot_zoneboundcost"]]))
        {
            # if there are extra rows that have been added by "dragging" the control, remove them
            zoneboundcostdat_edit <- values_zoneboundcost[["hot_zoneboundcost"]]
            iEditRows <- nrow(zoneboundcostdat_edit)

            if (iZBC_Rows > 0)
            {
                if (iEditRows > iZBC_Rows)
                {
                    # delete the extra rows before saving
                    iRowsToDelete <- iEditRows - iZBC_Rows
                    for (i in 1:iRowsToDelete)
                    {
                        zoneboundcostdat_edit <- zoneboundcostdat_edit[-c(nrow(zoneboundcostdat_edit)),]
                    }
                }
            }

            # write the zoneboundcost.dat file
            sZBCfile <- paste0(sMarxanDir,"/input/zoneboundcost.dat")
            cat(paste0("write file ",sZBCfile,"\n"))
            write("zoneid1,zoneid2,cost",file=sZBCfile)
            for (j in 1:nrow(zoneboundcostdat_edit))
            {
                for (i in 1:ncol(zoneboundcostdat_edit))
                {
                    if (i > j)
                    {
                        # we only save the portion of the matrix above and to the right of the identity row
                        # row,col
                        write(paste0(j,",",i,",",zoneboundcostdat_edit[j,i]),file=sZBCfile,append=TRUE)
                    }
                }
            }
        }
    })

    output$hot = renderRHandsontable({

        if (!is.null(input$database))
        {    
            if (!is.null(input$hot))
            {
                DF = hot_to_r(input$hot)
            } else {
                DF = read.csv(paste0(sMarxanDir,"/input/spec.dat"),stringsAsFactors=FALSE)
                if ("target" %in% colnames(DF))
                {
                    edit_spec_fields <<- c("prop","spf","target")
                } else {
                    edit_spec_fields <<- c("prop","spf")
                }
                iSpecDatRows <<- nrow(DF)
                DF$spf <- as.numeric(DF$spf)
            }

            setHot(DF)
            rhandsontable(DF, readOnly = T) %>%
            hot_table(highlightCol = TRUE, highlightRow = TRUE) %>%
            # if exists target, include that as well
            hot_col(edit_spec_fields, readOnly = FALSE) %>%
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

    output$hot_zonecost = renderRHandsontable({

        if (!is.null(input$database))
        {    
            if (!is.null(input$hot_zonecost))
            {
                DF = hot_to_r(input$hot_zonecost)
            } else {
                DF = gen_input_table_zonecost()
                edit_zc_fields <<- colnames(DF)[-1]
                iZC_Rows <<- nrow(DF)
            }

            setHot_zonecost(DF)
            rhandsontable(DF, readOnly = T) %>%
            hot_table(highlightCol = TRUE, highlightRow = TRUE) %>%
            # if exists target, include that as well
            hot_col(edit_zc_fields, readOnly = FALSE) %>%
            hot_cols(renderer = "
                function (instance, td, row, col, prop, value, cellProperties)
                {
                    Handsontable.renderers.TextRenderer.apply(this, arguments);
                    if (value < 0.0)
                    {
                        td.style.background = 'red';
                    }
                }"
            )
        }
    })

    output$hot_zonetarget = renderRHandsontable({

        if (!is.null(input$database))
        {    
            if (!is.null(input$hot_zonetarget))
            {
                DF = hot_to_r(input$hot_zonetarget)
            } else {
                DF = gen_input_table_zonetarget()
                edit_zt_fields <<- colnames(DF)[-1]
                iZT_Rows <<- nrow(DF)
            }

            setHot_zonetarget(DF)
            rhandsontable(DF, readOnly = T) %>%
            hot_table(highlightCol = TRUE, highlightRow = TRUE) %>%
            # if exists target, include that as well
            hot_col(edit_zt_fields, readOnly = FALSE) %>%
            hot_cols(renderer = "
                function (instance, td, row, col, prop, value, cellProperties)
                {
                    Handsontable.renderers.TextRenderer.apply(this, arguments);
                    if (value > 1.0 || value < 0.0)
                    {
                        td.style.background = 'red';
                    }
                }"
            )
        }
    })

    output$hot_zonecontrib = renderRHandsontable({

        if (!is.null(input$database))
        {    
            if (!is.null(input$hot_zonecontrib))
            {
                DF = hot_to_r(input$hot_zonecontrib)
            } else {
                DF = gen_input_table_zonecontrib()
                edit_zonecontrib_fields <<- colnames(DF)[-1]
                iZoneContribRows <<- nrow(DF)
            }

            setHot_zonecontrib(DF)
            rhandsontable(DF, readOnly = T) %>%
            hot_table(highlightCol = TRUE, highlightRow = TRUE) %>%
            # if exists target, include that as well
            hot_col(edit_zonecontrib_fields, readOnly = FALSE) %>%
            hot_cols(renderer = "
                function (instance, td, row, col, prop, value, cellProperties)
                {
                    Handsontable.renderers.TextRenderer.apply(this, arguments);
                    if (value > 1.0 || value < 0.0)
                    {
                        td.style.background = 'red';
                    }
                }"
            )
        }
    })

    output$hot_zoneboundcost = renderRHandsontable({

        if (!is.null(input$database))
        {    
            if (!is.null(input$hot_zoneboundcost))
            {
                DF = hot_to_r(input$hot_zoneboundcost)
            } else {
                DF = gen_input_table_zoneboundcost()
                edit_zbt_fields <<- colnames(DF)
                iZBC_Rows <<- nrow(DF)
            }

            setHot_zoneboundcost(DF)
            rhandsontable(DF, readOnly = T) %>%
            hot_table(highlightCol = TRUE, highlightRow = TRUE) %>%
            # if exists target, include that as well
            hot_col(edit_zbt_fields, readOnly = FALSE) %>%
            hot_cols(renderer = "
                function (instance, td, row, col, prop, value, cellProperties)
                {
                    Handsontable.renderers.TextRenderer.apply(this, arguments);
                    if (col <= row)
                    {
                        td.style.background = 'blue';
                    }
                    if (value < 0.0)
                    {
                        td.style.background = 'red';
                    }
                }"
            )
        }
    })

    observe({

        cat("input$m\n")

        if (!is.null(input$m))
        if (input$m > 0)
        {
            iM <<- input$m
            cat(paste0("iM ",iM,"\n"))
            AppendLogFile(sLogFile,paste0("input$m ",input$m))
        }
    })

    observe({

        cat("input$n\n")

        if (!is.null(input$n))
        if (input$n > 0)
        {
            iN <<- input$n
            cat(paste0("iN ",iN,"\n"))
            AppendLogFile(sLogFile,paste0("input$n ",input$n))
        }
    })

    runclicked <- reactive({

        #cat(paste0("mrun ",fMarxanRunning,"\n"))

        if (!is.null(input$mrun))
        if (input$mrun > 0)
        #if (fMarxanRunning == FALSE)
        {
            #fMarxanRunning <<- TRUE
            ptm <- proc.time()

            cat(paste0("click mrun ",input$mrun,"\n"))

            RunMarZone_app()

            # trigger a refresh of the UI
            if (!(fEnableLeaflet))
            {
                # refresh marxanmap
                inc(irefreshmarxanmap) <<- 1
                updateNumericInput(session, "refreshmarxanmap", value = irefreshmarxanmap)
            }
            # trigger a refresh of the map
            inc(irefreshmap) <<- 1
            updateNumericInput(session, "refreshmap", value = irefreshmap)
            # trigger a refresh of the cluster
            inc(irefreshcluster) <<- 1
            updateNumericInput(session, "refreshcluster", value = irefreshcluster)
            # trigger a refresh of the table
            inc(irefreshtable) <<- 1
            updateNumericInput(session, "refreshtable", value = irefreshtable)

            AppendLogFile(sLogFile,paste0("input$mrun ",input$mrun," elapsed ",(proc.time() - ptm)[3]))
            #fMarxanRunning <<- FALSE
        }
        return(0)
    })

    output$marzoneplot <- renderPlot({

        input$refreshcluster

        if (!is.null(input$refreshcluster))
        {
            AppendLogFile(sLogFile,paste0("output$marzoneplot ",input$refreshcluster," ",input$cluster))

            withProgress(message="Rendering cluster",value=0,
           {
                if (input$cluster == "cluster2ds") { cluster_2ds("marzone",FALSE) }
                if (input$cluster == "clusterdendogram") { cluster_dendogram("marzone") }
            })
        }
    }, height=600,width=600) # renderPlot
    
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
                inc(irefreshaspectheight) <<- 1
                updateNumericInput(session, "refreshaspectheight", value = irefreshaspectheight)
                inc(irefreshaspectwidth) <<- 1
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
                    inc(irefreshaspectheight) <<- 1
                    updateNumericInput(session, "refreshaspectheight", value = irefreshaspectheight)
                    inc(irefreshaspectwidth) <<- 1
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
    
    output$marzonemap <- renderPlot({

        input$refreshmarxanmap
        input$m
        input$n
        
        cat("marzonemap\n")

        fDisplay <- TRUE
        
        if (fLeafletRdata)
        {
            if (input$enableleaflet) { fDisplay <- FALSE }
        }
        
        if (!input$enablemap) { fDisplay <- FALSE }
        
        if (fDisplay)
        {
            AppendLogFile(sLogFile,paste0("output$marzonemap ",input$map))
        
            withProgress(message="Rendering map",value=0,
            {
                if (input$map == "ssolnNmap") { map_mz_ssolnNmap(input$n) }
                if (input$map == "bestmap") { map_mz_bestmap() }
                if (input$map == "runMmap") { map_mz_runMmap(input$m) }

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
        
        inc(iBounds) <<- 1
    })
    
    observeEvent(input$leafletmap_zoom, {

        if (iZoom > 0) { iPrevZoom <<- map_zoom }
        
        map_zoom <<- input$leafletmap_zoom
        cat(paste0("zoom ",map_zoom,"\n"))
        
        if (iZoom == 0)
        {
            iFirstZoom <<- map_zoom
            iPrevZoom <<- map_zoom
        }
        
        inc(iZoom) <<- 1
    })

    output$leafletmap <- renderLeaflet({

        input$refreshmap
        input$m
        input$n

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
                AppendLogFile(sLogFile,paste0("output$marzonemap ",input$map))

                withProgress(message="Rendering map",value=0,
                {
                    if (input$map == "ssolnNmap") { map_mz_ssolnNmap_leaflet(input$n) }
                    if (input$map == "bestmap") { map_mz_bestmap_leaflet() }
                    if (input$map == "runMmap") { map_mz_runMmap_leaflet(input$m) }

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

    output$marzoneinputtable <- renderTable({
    
        input$refreshtable

        if (input$table_i %in% c("zones","costs"))
        {
            thetable <- read.csv(paste0(sMarxanDir,"/input/",input$table_i,".dat"))
        }

        return(thetable)
    })

    output$marzoneoutputtable <- renderTable({
        if (input$table_o == "sumtable")
        {
            sTable <- "sumtable.dat"
            thetable <- read.csv(paste0(sMarxanDir,"/output/output_sum.csv"))
            # NOTE : sql select will fail if zone names are sql keywords
            thetable <- sqldf(paste0("SELECT Score, Cost, Shortfall from thetable"))
            iBest <- which.min(thetable$Score)
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

            thetable <- cbind(Run,thetable)
            thetable$Run <- as.character(thetable$Run)
        }
        if ((input$table_o == "mvbesttable") | (input$table_o == "mvNtable"))
        {
            if (input$table_o == "mvbesttable")
            {
                thetable <- read.csv(paste0(sMarxanDir,"/output/output_sum.csv"))
                iTable <- which.min(thetable$Score)[1]
            }
            if (input$table_o == "mvNtable")
            {
                iTable <- input$m
            }
            
            thetable <- return_mv_table(iTable)
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
        fMarxanHidden <<- cdata$output_marzonemap_hidden
      
        #cnames <- names(cdata)

        #allvalues <- lapply(cnames, function(name)
        #{
        #    paste(name, cdata[[name]], sep=" = ")
        #}) 
      
        cat("session$clientData\n")
        #cat(paste0(paste(allvalues, collapse = "\n"),"\n"))
    })

    output$zonename = renderText({
    
        input$database
        
        paste0("Selection frequency ",ZoneNames[input$n]," zone")
    })
})
