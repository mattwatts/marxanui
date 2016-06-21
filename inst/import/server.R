# marxan.io upload

library(shiny)
library(sp)
library(maptools)
library(rgdal)
library(tools)
library(PBSmapping)
library(rgeos)
library(foreign)
library(sqldf)
library(vegan)
library(labdsv)
library(xtable)
library(foreach)
library(rhandsontable)
library(iptools)
library(png)
library(rjson)

# which platform are we running on?
detect_platform <- function()
{
    sPkgType <- .Platform$pkgType
    fWindows <<- (sPkgType == "win.binary")
    f64bit <<- T
    fLinux <<- (sPkgType == "source")
    fMac <<- !(fWindows|fLinux)
    if (fWindows) { f64bit <<- (Sys.getenv("R_ARCH") == "/x64") }
    if (fLinux) { f64bit <<- (.Machine$sizeof.pointer == 8) }
}
detect_platform()

if (fWindows)
{
    library(doParallel)
} else {
    library(doMC)
    registerDoMC(iCores)  # the number of CPU cores
}

# Set the file size limit for uploads here in megabytes
iMegabytes <- 200
options(shiny.maxRequestSize = iMegabytes*1024^2)


shinyServer(function(input, output, session, clientData) {

    observe({
        sUserIP <<- as.character(input$ipid)
        cat(paste0("sUserIP ",sUserIP,"\n"))
    })

    #source(paste0(sShinySourcePath,"/ingest_marxan_data.R"),  local = TRUE)
    #source(paste0(sShinySourcePath,"/server_pre_marxan.R"),  local = TRUE)

    marxanui_start("import")

    observe({
            # render the user interface
            source(paste0(sAppDir,"/render_ui.R"),  local = TRUE)
    }) # observe

    observe({
            sDatabase <<- input$uploadname
    }) # observe

    puid_field_choices <- reactive({

        input$updatepuidchoices
        
        cat(paste0("puid_choices ",puid_choices),"\n")

        puid_choices
    })

    output$fileupload <- renderTable({

        # input$file1 is the zip file containing the Marxan database.
        # summarytable lists info gleaned from parsing the Marxan database.

        # turn off accept controls
        updateNumericInput(session,"showacceptcontrols",value=0)
        input_messages_df <<- c()
        iupdateusermessages <<- iupdateusermessages + 1
        updateNumericInput(session,"updateusermessages",value=iupdateusermessages)

        inFile <- input$file1

        if (is.null(inFile))
          return(NULL)

        # create new user session for this upload
        if (fLocalUser)
        {
            sUserSession <<- CreateTempUploadDir(paste0(sShinyTempPath))
        } else {
            sUserSession <<- CreateTempUploadDir(paste0(sShinyTempPath,sUserName))
        }

        sDatasetName <<- basename(file_path_sans_ext(inFile$name))

        file.copy(as.character(inFile$datapath),paste0(sUserSession,"/",as.character(inFile$name)),overwrite=TRUE)

        ptm <- proc.time()
        ParseResult <- unzip_file(as.character(inFile$datapath),sUserSession,sShinyUserPath,sShinyDataPath,sUserName)

        sFileName <- paste0(basename(inFile$name))#,".",file_ext(inFile$name))
        sFileSize <- utils:::format.object_size(inFile$size, "auto")
        iElapsed <- (proc.time() - ptm)[3]

        summary_table <- as.data.frame(rbind(c("name",sFileName),
                                             c("size",sFileSize),
                                             c("unzip elapsed",paste0(iElapsed," s"))))
        colnames(summary_table) <- c("","")

        # turn on parseinputfiles
        updateNumericInput(session,"parseinputfiles",value=1)

        summary_table
    })

    observe({

        if (input$parseinputfiles > 0)
        {
            # parse the input files
            # the input messages automatically update as we go
            ptm <- proc.time()
            
            tryCatch(
            {
                read_input_files(sUserSession,sShinyDataPath)
                #sPuLayer <<- sPuLayer
            },
            error=function(cond)
            {
                error_msg(paste0("Error in read_input_files >",cond,"<"))
                stop()
            })
            
            iElapsed <- (proc.time() - ptm)[3]
            user_msg(paste0("read input elapsed,",iElapsed," s"))

            # turn off parseinputfiles
            updateNumericInput(session,"parseinputfiles",value=0)
            
            # are there errors? react appropriate to error condition
            if (length(ErrorMsg) == 0)
            {
                # no errors
                # do we need to ask user to select PUID field?
                if (iPUIDfield == 0)
                {
                    # we do need to ask user
                    # trigger update puid choices
                    iupdatepuidchoices <<- iupdatepuidchoices + 1
                    updateNumericInput(session,"updatepuidchoices",value=iupdatepuidchoices)
                    # turn on select PUID controls
                    updateNumericInput(session,"selectpuid",value=1)
                } else {
                    # we guessed which field is PUID
                    # turn on parsepolygons
                    updateNumericInput(session,"parsepolygons",value=1)
                }
            } else {
                # an error occurred
                # do nothing because error message(s) are already displayed
            }
        }
    })

    session$onSessionEnded(function() {
        stopApp()
    })

    observe({

        if (!is.null(input$selectpuidbtn))
        if (input$selectpuidbtn > 0)
        {
            # user has selected PUID field
            iPUIDfield <<- which(colnames(putable) == input$puid)

            # turn off select PUID controls
            updateNumericInput(session,"selectpuid",value=0)

            # turn on parsepolygons
            updateNumericInput(session,"parsepolygons",value=1)
        }
    })
    
    observe({

        if (!is.null(input$cancelpuidbtn))
        if (input$cancelpuidbtn > 0)
        {
            # user is cancelling upload
            updateNumericInput(session,"selectpuid",value=0)
            
            input_messages_df <<- c()
            iupdateusermessages <<- iupdateusermessages + 1
            updateNumericInput(session,"updateusermessages",value=iupdateusermessages)
        }
    })

    observe({

        if (!is.null(input$cancelupload))
        if (input$cancelupload > 0)
        {
            # user is cancelling upload
            updateNumericInput(session,"showacceptcontrols",value=0)
            
            input_messages_df <<- c()
            iupdateusermessages <<- iupdateusermessages + 1
            updateNumericInput(session,"updateusermessages",value=iupdateusermessages)
        }
    })

    observe({

        if (input$parsepolygons > 0)
        {
            # turn off parseinputfiles
            updateNumericInput(session,"parseinputfiles",value=0)
            
            # parse the polygons
            ptm <- proc.time()

            tryCatch(
            {
                read_polygons()
            },
            error=function(cond)
            {
                error_msg(paste0("Error in read_polygons >",cond,"<"))
                stop()
            })

            iElapsed <- (proc.time() - ptm)[3]
            user_msg(paste0("read polygons elapsed,",iElapsed," s"))
            # this line causes everything to fail
            #user_msg(paste0("read polygons elapsed,",elapsed_to_string((proc.time() - ptm)[3])))

            cat(paste0("length(ErrorMsg) ",length(ErrorMsg),"\n"))

            # are there errors? react appropriate to error condition
            if (length(ErrorMsg) == 0)
            {
                # no errors
                # turn on accept controls
                updateNumericInput(session,"showacceptcontrols",value=1)
                updateTextInput(session, "uploadname",value = SafeDbName(sDatasetName,sShinyUserPath,sUserName))
            } else {
                # an error occurred
                # do nothing because error message(s) are already displayed
            }
            
            # turn off parsepolygons
            updateNumericInput(session,"parsepolygons",value=0)
        }
    })

    autoInvalidateUserMessages <- reactiveTimer(1000,session=session)

    observe({

        autoInvalidateUserMessages()

        #cat(paste0("autoInvalidateUserMessages tick fUserMessagesFile ",fUserMessagesFile,"\n"))
        
        # detect new input messages and display them
        if (fUserMessagesFile)
        {
            #cat(paste0("file.exists(sUserMessagesFile) ",file.exists(sUserMessagesFile)," sUserMessagesFile ",sUserMessagesFile,"\n"))
            if (file.exists(sUserMessagesFile))
            {
                # if file has changed
                CurrentFileTime <- file.info(sUserMessagesFile)$ctime
                if (!(CurrentFileTime == LastFileTime))
                {
                    LastFileTime <<- file.info(sUserMessagesFile)$ctime
                    # load the file and cause it to be displayed
                    input_messages_df <<- read.csv(sUserMessagesFile,stringsAsFactors=F,header=F)
                    colnames(input_messages_df) <<- c("","")

                    # trigger update user messages
                    iupdateusermessages <<- iupdateusermessages + 1
                    updateNumericInput(session,"updateusermessages",value=iupdateusermessages)
                    
                    cat(paste0("iupdateusermessages ",iupdateusermessages,"\n"))
                }
            }
        } else {
            input_messages_df <<- c()
        }
    })

    output$usermessages <- renderTable({

        input$updateusermessages

        cat(paste0("output$usermessages input$updateusermessages ",input$updateusermessages,"\n"))

        # display parse Marxan input file messages

        input_messages_df
    })

    acceptclicked <- reactive({

        cat("acceptclicked\n")

        if (!is.null(input$acceptupload))
        if (input$acceptupload > 0)
        {
            AppendLogFile(sLogFile,paste0("input$acceptupload start ",input$acceptupload))

            cat(paste0("click acceptupload ",input$acceptupload,"\n"))

            # add this database to list of databases
            if (fMarZone)
            {
                sDatabasePath <- paste0(sUserHome,"/marzone/",sDatabase)
            } else {
                sDatabasePath <- paste0(sUserHome,"/marxan/",sDatabase)
            }
            cat(paste0("sDatabasePath ",sDatabasePath,"\n"))
            # create directory
            if (!file.exists(sDatabasePath))
            {
                AddDatabase(sDatabasePath)

                # turn off accept controls
                updateNumericInput(session,"showacceptcontrols",value=0)

                input_messages_df <<- c()
                iupdateusermessages <<- iupdateusermessages + 1
                updateNumericInput(session,"updateusermessages",value=iupdateusermessages)

                withProgress(message="Dataset accepted",value=0,min=0,max=20, { Sys.sleep(5) })

                # trigger a refresh of the relevant UI components
                return(1)
            } else {
                # duplicate database name detected
                withProgress(message="Duplicate dataset name",value=0,min=0,max=20, { Sys.sleep(5) })
                return(2)
            }

            AppendLogFile(sLogFile,paste0("input$acceptupload end ",input$acceptupload))

        } else {
            return(0)
        }
    })

    output$feedbackupload = renderText({
        acceptclicked()
        sprintf("")
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
                        "queries: ",paste(names(query), query, sep = "=", collapse=", "))
        AppendLogFile(sLogFile,sText)
        cat(paste0(sText,"\n"))
    })
})
