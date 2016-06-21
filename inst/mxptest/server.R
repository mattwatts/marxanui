# marxan.io

library(doParallel)
library(foreach)
require(foreign)
library(iptools)
require(labdsv)
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


iAspectX <<- 1
iAspectY <<- 1

shinyServer(function(input, output, session, clientData) {

    observe({
        sUserIP <<- as.character(input$ipid)
        cat(paste0("sUserIP ",sUserIP,"\n"))
    })
  
    observe({
        sFingerprint <<- as.character(input$fingerprint)
        cat(paste0("sFingerprint ",sFingerprint,"\n"))
    })

    values = list()
    setHot = function(x) values[["hot"]] <<- x  

    #source(paste0(sShinySourcePath,"/prepare_param_test.R"),  local = TRUE)
    #source(paste0(sShinySourcePath,"/server_pre_marxan.R"),  local = TRUE)

    marxanui_start("mxptest")

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
    }) # observe

    session$onSessionEnded(function() {
        stopApp()
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
            # update input file name for ptplot
            if (swhichparam == "BLM")
            {
                sAppendSummary <<- paste0(sMarxanDir,"/output/output_BLMsummary_SPF",ruserspf,"_Targ",rusertarg,".csv")
            }
            if (swhichparam == "SPF")
            {
                sAppendSummary <<- paste0(sMarxanDir,"/output/output_SPFsummary_BLM",ruserblm,"_Targ",rusertarg,".csv")
            }
            if (swhichparam == "Targ")
            {
                sAppendSummary <<- paste0(sMarxanDir,"/output/output_Targsummary_BLM",ruserblm,"_SPF",ruserspf,".csv")
            }
        
            if (sPrevious != sMarxanDir)
            {
                if (sSelectDb != "")
                {
                    ChangeDatabase("mxptest",session)

                    # update the relevant UI components
                    # trigger a refresh of the param testing UI
                    irefreshptinput <<- irefreshptinput + 1
                    if (irefreshptinput > 4)
                    {
                        updateNumericInput(session, "refreshptinput", value = irefreshptinput)
                    }

                    cat(paste0("database change irefreshptinput ",irefreshptinput,"\n"))
                }
            }
        }
    }) # observe

    runparamtest <- reactive({
        cat("runparamtest\n")
        if (input$mptrun == 0)
        {
            imptrun <<- 0
            cat("init mptrun\n")
        }
        else
        {
            if (input$mptrun > imptrun)
            {
                imptrun <<- input$mptrun
                cat("mptrun incremented\n")

                RunMarxan_paramtest_app(swhichparam)

                irefreshptinput <<- irefreshptinput + 1
                if (irefreshptinput > 4)
                {
                    updateNumericInput(session, "refreshptinput", value = irefreshptinput)
                }

                cat(paste0("mptrun change irefreshptinput ",irefreshptinput,"\n"))
            }
        }

        return(as.character(input$mptrun))
    })

    observe ({
        if (!is.null(input$whichparam))
        {
            cat("observe whichparam\n")
        
            if (input$whichparam == "BLM Calibration")
            {
                swhichparam <<- "BLM"
                sAppendSummary <<- paste0(sMarxanDir,"/output/output_BLMsummary_SPF",ruserspf,"_Targ",rusertarg,".csv")
            }
            if (input$whichparam == "SPF Calibration")
            {
                swhichparam <<- "SPF"
                sAppendSummary <<- paste0(sMarxanDir,"/output/output_SPFsummary_BLM",ruserblm,"_Targ",rusertarg,".csv")
            }
            if (input$whichparam == "Target Sensitivity")
            {
                swhichparam <<- "Targ"
                sAppendSummary <<- paste0(sMarxanDir,"/output/output_Targsummary_BLM",ruserblm,"_SPF",ruserspf,".csv")
            }
        
            sSummary <<- paste0(sMarxanDir,"/output/output_",swhichparam,"summary.csv")

            # do we need to do 1st Marxan run for this parameter?
            if (!file.exists(paste0(sMarxanDir,"/output/output_",swhichparam,"summary.csv")))
            {
                RunMarxan_paramtest(swhichparam)
            }

            irefreshptinput <<- irefreshptinput + 1
            if (irefreshptinput > 4)
            {
                updateNumericInput(session, "refreshptinput", value = irefreshptinput)
            }

            cat(paste0("whichparam change irefreshptinput ",irefreshptinput,"\n"))
        }
    })

    observe ({
        ruserblm <<- as.numeric(input$userblm)
        cat(paste0("ruserblm ",ruserblm,"\n"))
        if (swhichparam == "SPF")
        {
            sAppendSummary <<- paste0(sMarxanDir,"/output/output_SPFsummary_BLM",ruserblm,"_Targ",rusertarg,".csv")
        }
        if (swhichparam == "Targ")
        {
            sAppendSummary <<- paste0(sMarxanDir,"/output/output_Targsummary_BLM",ruserblm,"_SPF",ruserspf,".csv")
        }
    })
    
    observe ({
        ruserspf <<- as.numeric(input$userspf)
        cat(paste0("ruserspf ",ruserspf,"\n"))
        if (swhichparam == "BLM")
        {
            sAppendSummary <<- paste0(sMarxanDir,"/output/output_BLMsummary_SPF",ruserspf,"_Targ",rusertarg,".csv")
        }
        if (swhichparam == "Targ")
        {
            sAppendSummary <<- paste0(sMarxanDir,"/output/output_Targsummary_BLM",ruserblm,"_SPF",ruserspf,".csv")
        }
    })
    
    observe ({
        rusertarg <<- as.numeric(input$usertarg)
        cat(paste0("rusertarg ",rusertarg,"\n"))
        if (swhichparam == "BLM")
        {
            sAppendSummary <<- paste0(sMarxanDir,"/output/output_BLMsummary_SPF",ruserspf,"_Targ",rusertarg,".csv")
        }
        if (swhichparam == "SPF")
        {
            sAppendSummary <<- paste0(sMarxanDir,"/output/output_SPFsummary_BLM",ruserblm,"_Targ",rusertarg,".csv")
        }
    })
    
    observe ({
        if (!is.null(input$whichmap))
        {
            cat("observe whichmap\n")

            iwhichmap <<- input$whichmap
        
            irefreshptinput <<- irefreshptinput + 1
            if (irefreshptinput > 4)
            {
                updateNumericInput(session, "refreshptinput", value = irefreshptinput)
            }

            cat(paste0("whichmap change irefreshptinput ",irefreshptinput,"\n"))
        }
    })

    observe ({
        if (!is.null(input$whichrun))
        {
            cat("observe whichrun\n")

            if (input$whichrun == "Best Solution")
            {
                swhichrun <<- "best"
            } else {
                if (input$whichrun == "Selection Frequency")
                {
                    swhichrun <<- "ssoln"
                } else {
                   swhichrun <<- substrRight(input$whichrun, nchar(input$whichrun)-4)
                }
            }    
    
            # "Best Solution","Run 1","Run 2","Run 3","Run 4","Run 5",
            # "Run 6","Run 7","Run 8","Run 9","Run 10","Selection Frequency"

            irefreshptinput <<- irefreshptinput + 1
            ################## This is probably the line that is sending the double refresh message on startup. 
            if (irefreshptinput > 4)
            {
                updateNumericInput(session, "refreshptinput", value = irefreshptinput)
            }

            cat(paste0("whichrun change irefreshptinput ",irefreshptinput,"\n"))
        }
    })
    
    observe ({
        rRampBLMmin <<- input$rampBLMmin
        cat(paste0("rRampBLMmin ",rRampBLMmin,"\n"))
    })

    observe ({
        rRampBLMmax <<- input$rampBLMmax
        cat(paste0("rRampBLMmax ",rRampBLMmax,"\n"))
    })
    
    observe ({
        rRampSPFmin <<- input$rampSPFmin
        cat(paste0("rRampSPFmin ",rRampSPFmin,"\n"))
    })

    observe ({
        rRampSPFmax <<- input$rampSPFmax
        cat(paste0("rRampSPFmax ",rRampSPFmax,"\n"))
    })
    
    observe ({
        rtargetmin <<- input$targetmin
        cat(paste0("rtargetmin ",rtargetmin,"\n"))
    })

    observe ({
        rtargetmax <<- input$targetmax
        cat(paste0("rtargetmax ",rtargetmax,"\n"))
    })

    outputptmap <- reactive({
        cat("outputmap\n")
        
        input$refreshptinput
        
        withProgress(message="Rendering map",value=0,
        {
            map_pt()
        })
        
        #addLines(puoutline,col="black")
    })

    outputptplot <- reactive({
        cat("outputptplot\n")

        input$refreshptinput
        input$whichmap
        
        VALUEsummary <- read.csv(sAppendSummary)

        cat(paste0("sAppendSummary ",sAppendSummary,"\n"))

        VALUElabel <- sqldf(paste0("SELECT ",swhichparam," from VALUEsummary"))

        cat(paste0("colnames(VALUElabel)[1] ",colnames(VALUElabel)[1],"\n"))

        colnames(VALUElabel)[1] <- "label"
        VALUElabel <- unlist(VALUElabel$label)
        
        # make all plotlabels blank except for the last iNumberOfTests
        if (length(VALUElabel) > iCores)
        {
            VALUElabel[1:(length(VALUElabel)-iCores)] <- ""
        }
        
        if (swhichparam == "BLM")
        {
            colnames(VALUEsummary)[4] <- "boundary"
            VALUEsummary <- sqldf("SELECT cost, boundary from VALUEsummary")
        }
        if (swhichparam == "SPF")
        {
            colnames(VALUEsummary)[4] <- "shortfall"
            VALUEsummary <- sqldf("SELECT cost, shortfall from VALUEsummary")
        }
        if (swhichparam == "Targ")
        {
            colnames(VALUEsummary)[2] <- "target"
            VALUEsummary <- sqldf("SELECT cost, target from VALUEsummary")
        }
        colours <- rep("black",each=nrow(VALUEsummary))
        colours[length(VALUElabel)-iCores+as.numeric(iwhichmap)] <- "blue"
        plot(VALUEsummary,col=colours)
        if (input$plotvalues)
        {
            text(VALUEsummary,labels=VALUElabel,pos=4,col=colours)
        }
    })

    outputpttable <- reactive({
        cat("outputpttable\n")

        input$refreshptinput

        thetable <- read.csv(paste0(sMarxanDir,"/output/output_",swhichparam,"summary.csv"),stringsAsFactors=FALSE)
        if (swhichparam == "BLM")
        {
            colnames(thetable)[4] <- "boundary"
            thetable <- sqldf("SELECT BLM, cost, boundary from thetable")
            thetable$BLM <- as.character(thetable$BLM)
            iColumns <- 3
        }
        if (swhichparam == "SPF")
        {
            colnames(thetable)[4] <- "shortfall"
            thetable <- sqldf("SELECT SPF, cost, shortfall from thetable")
            thetable$SPF <- as.character(thetable$SPF)
            iColumns <- 3
        }
        if (swhichparam == "Targ")
        {
            colnames(thetable)[2] <- "target"
            thetable <- sqldf("SELECT target, cost from thetable")
            iColumns <- 2
        }
        for (i in (1:nrow(thetable)))
        {
            if (i == iwhichmap)
            {
                for (j in (1:iColumns))
                {
                    thetable[i,j] <- HTML(paste0("<FONT COLOR='blue'>",thetable[i,j],"</FONT>"))
                }
            }
        }
        return(thetable)
    })

    output$ptmap <- renderPlot({
  
        outputptmap()

        if (!is.na(puoutline))
        {
            addLines(puoutline,col="black")
        }

    }, height=600,width=round(600/iAspectY*iAspectX))

    output$ptplot <- renderPlot({
        outputptplot()
    })

    output$pttable <- renderTable({
        data.frame(outputpttable())
    }, sanitize.text.function = function(x) x)

    output$paramtestfeedback = renderText({
        runparamtest()
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
})
