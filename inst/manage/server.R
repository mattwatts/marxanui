# marxan.io

library(shiny)
require(sp)
require(maptools)
require(PBSmapping)
require(foreign)
require(sqldf)
require(vegan)
require(labdsv)
require(xtable)
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

    #source(paste0(sShinySourcePath,"/server_pre_marxan.R"),  local = TRUE)
    marxanui_start("manage")

    autoInvalidate <- reactiveTimer(2000,session=session)

    observe({

        autoInvalidate()

        list_dirs <- c(list.dirs(sMarxanHome,full.names = TRUE),
                       list.dirs(sMarZoneHome,full.names = TRUE))

        # we detect if there are new folders in the users directory, indicating a new database import
        CurrentImportTime <- max(file.info(list_dirs)$ctime)
        if (!(CurrentImportTime == ImportTime))
        {
            # user has imported a new dataset
            cat(paste0("new dataset detected","\n"))
            ImportTime <<- CurrentImportTime

            # update the list of datasets to include the new one(s)
            updateSelectInput(session, "database",
                              choices = c(list.dirs(sMarxanHome),list.dirs(sMarZoneHome)),
                              selected = sSelectDb)
                              
            # trigger a refresh of the UI
            irefreshtable <<- irefreshtable + 1
            updateNumericInput(session, "refreshtable", value = irefreshtable)
            updateNumericInput(session,"areyousure",value=0)
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

        if (!is.null(input$publicdb))
        {
            # select this database from the list of databases
            sSelectPublicDb <<- input$publicdb
            cat(paste0("sSelectPublicDb ",sSelectPublicDb,"\n"))
            y <- strsplit(sSelectPublicDb,"/")
            sSelectPublicUser <<- y[[1]][1]
            sSelectPublicType <<- y[[1]][2]
            sSelectPublicDatabase <<- y[[1]][3]
            updateNumericInput(session,"copypublicdata",value=0)
        }
    })

    observe({

        if (!is.null(input$database))
        {
            # select this database from the list of databases
            sSelectDb <<- input$database
            cat(paste0("sSelectDb ",sSelectDb,"\n"))
            sPrevious <- sMarxanDir
            sMarxanDir <<- paste0(sMarxanHome,"/",sSelectDb)
            sZipWD <<- paste0(sMarxanHome)
            fMarxan <<- TRUE
            fMarZone <<- FALSE
            if (!file.exists(sMarxanDir))
            {
                sMarxanDir <<- paste0(sMarZoneHome,"/",sSelectDb)
                sZipWD <<- paste0(sMarZoneHome)
                fMarxan <<- FALSE
                fMarZone <<- TRUE
            }
            cat(paste0("sMarxanDir ",sMarxanDir,"\n"))
            AppendLogFile(sLogFile,paste0("sSelectDb ",sSelectDb))
            AppendLogFile(sLogFile,paste0("sMarxanDir ",sMarxanDir))

            if (sPrevious != sMarxanDir)
            {
                if (sSelectDb != "")
                {
                    #ChangeDatabase("marxan")

                    # update the relevant UI components
                    # trigger a refresh of the marxan UI
                    # trigger a refresh of the cluster
                    #irefreshcluster <<- irefreshcluster + 1
                    #updateNumericInput(session, "refreshcluster", value = irefreshcluster)
                    updateNumericInput(session,"areyousure",value=0)
                }
            }
        }
    })

    observe({
        fWindowsEOLN <<- input$windowseoln
    })

    output$downloadData <- downloadHandler(
        filename = function()
        {
            paste0(sSelectDb, '.zip')
        },
        content = function(file) {

            withProgress(message="Generating export",value=0,
            {
                # remove existing zip file
                sZipFile <- paste0(sAppHome,"/",sSelectDb,".zip")
                if (file.exists(sZipFile))
                {
                    file.remove(sZipFile)
                }

                # create temp directory
                sTempDir <- paste0(sShinyTempPath,"/",sUserName)
                dir.create(sTempDir)

                # copy files to temp directory
                system(paste0("rm -rf ",sTempDir,"/",sSelectDb))
                system(paste0("cp -rf ",sMarxanDir," ",sTempDir))
                system(paste0("cp -f ",sTempDir,"/",sSelectDb,"/core1/*.csv ",sTempDir,"/",sSelectDb))
                system(paste0("cp -f ",sTempDir,"/",sSelectDb,"/core1/*.txt ",sTempDir,"/",sSelectDb))

                # remove unnecessary files
                system(paste0("rm -rf ",sTempDir,"/",sSelectDb,"/core*"))
                system(paste0("rm ",sTempDir,"/",sSelectDb,"/BLM.csv"))
                system(paste0("rm ",sTempDir,"/",sSelectDb,"/SPF.csv"))
                system(paste0("rm ",sTempDir,"/",sSelectDb,"/Targ.csv"))
                system(paste0("rm ",sTempDir,"/",sSelectDb,"/input/specBLM*.dat"))
                system(paste0("rm ",sTempDir,"/",sSelectDb,"/input/specSPF*.dat"))
                system(paste0("rm ",sTempDir,"/",sSelectDb,"/input/specTarg*.dat"))
                for (i in 1:10)
                {
                    system(paste0("rm ",sTempDir,"/",sSelectDb,"/output/output",i,"*.csv"))
                    system(paste0("rm ",sTempDir,"/",sSelectDb,"/output/output",i,"*.dat"))
                    system(paste0("rm ",sTempDir,"/",sSelectDb,"/output/outputBLM",i,"*.csv"))
                    system(paste0("rm ",sTempDir,"/",sSelectDb,"/output/outputBLM",i,"*.dat"))
                    system(paste0("rm ",sTempDir,"/",sSelectDb,"/output/outputSPF",i,"*.csv"))
                    system(paste0("rm ",sTempDir,"/",sSelectDb,"/output/outputSPF",i,"*.dat"))
                    system(paste0("rm ",sTempDir,"/",sSelectDb,"/output/outputTarg",i,"*.csv"))
                    system(paste0("rm ",sTempDir,"/",sSelectDb,"/output/outputTarg",i,"*.dat"))
                    system(paste0("rm ",sTempDir,"/",sSelectDb,"/output/output_BLMsummary",i,".csv"))
                    system(paste0("rm ",sTempDir,"/",sSelectDb,"/output/output_SPFsummary",i,".csv"))
                    system(paste0("rm ",sTempDir,"/",sSelectDb,"/output/output_Targsummary",i,".csv"))
                    system(paste0("rm ",sTempDir,"/",sSelectDb,"/output/output_BLMsummary.csv"))
                    system(paste0("rm ",sTempDir,"/",sSelectDb,"/output/output_SPFsummary.csv"))
                    system(paste0("rm ",sTempDir,"/",sSelectDb,"/output/output_Targsummary.csv"))
                }

                # convert windows eoln
                if (fWindowsEOLN)
                {
                    system(paste0("unix2dos ",sTempDir,"/",sSelectDb,"/input.dat"))
                    system(paste0("unix2dos ",sTempDir,"/",sSelectDb,"/*.txt"))
                    system(paste0("unix2dos ",sTempDir,"/",sSelectDb,"/*.csv"))
                    system(paste0("unix2dos ",sTempDir,"/",sSelectDb,"/input/*"))
                    system(paste0("unix2dos ",sTempDir,"/",sSelectDb,"/output/*.csv"))
                    system(paste0("unix2dos ",sTempDir,"/",sSelectDb,"/output/*.dat"))
                }

                sWD <- getwd()
                setwd(sTempDir)

                # create new zip file
                system(paste0("zip -r ",sZipFile," ",sSelectDb))

                setwd(sWD)
            }) 

            file.copy(sZipFile,file)
        }
    )

    observe({
        if (!is.null(input$deletedb))
        {
            if (input$deletedb > 0)
            {
                # user has pressed delete
                cat(paste0("delete clicked ",input$deletedb,"\n"))
                updateNumericInput(session,"areyousure",value=1)
            }
        }
    })

    observe({
        if (!is.null(input$cancelDelete))
        {
            if (input$cancelDelete > 0)
            {
                updateNumericInput(session,"areyousure",value=0)
            }
        }
    })

    observe({
        # click rename
        if (!is.null(input$renameData))
        {
            if (input$renameData > 0)
            {
                cat("rename clicked\n")
                updateNumericInput(session,"renamemydata",value=1)
                updateTextInput(session,"renameName",value=isolate(sSelectDb))
            }
        }
    })

    observe({
        # click accept rename
        if (!is.null(input$acceptName))
        {
            if (input$acceptName > 0)
            {
                # does the new name already exist?
                if (fMarxan)
                {
                    sNewNameDb <- paste0(sMarxanHome,"/",sRenameName)
                } else {
                    sNewNameDb <- paste0(sMarZoneHome,"/",sRenameName)
                }
                if (file.exists(sNewNameDb))
                {
                    # can't rename. name already exists
                    withProgress(message=paste0("Can't rename. New name ",sRenameName," already exists"),value=0,min=0,max=20, { Sys.sleep(5) })
                } else {
                    # rename the dataset
                    file.rename(sMarxanDir,sNewNameDb)

                    sOldName <- sSelectDb
                    sSelectDb <<- sRenameName
                    if (fMarxan)
                    {
                        sMarxanDir <<- paste0(sMarxanHome,"/",sSelectDb)
                        sZipWD <<- paste0(sMarxanHome)
                    } else {
                        sMarxanDir <<- paste0(sMarZoneHome,"/",sSelectDb)
                        sZipWD <<- paste0(sMarZoneHome)
                    }
                    
                    # trigger a refresh of the "My data" table & "database"
                    irefreshtable <<- irefreshtable + 1
                    updateNumericInput(session, "refreshtable", value = irefreshtable)
                    updateSelectInput(session, "database",
                                      choices = c(list.dirs(sMarxanHome),list.dirs(sMarZoneHome)),
                                      selected = sSelectDb)

                    withProgress(message=paste0("Dataset ",sOldName," renamed to ",sSelectDb),value=0,min=0,max=20, { Sys.sleep(5) })
                    updateNumericInput(session,"renamemydata",value=0)
                }
                
            }
        }
    })

    observe({
        # click cancel rename
        if (!is.null(input$cancelName))
        {
            if (input$cancelName > 0)
            {
                updateNumericInput(session,"renamemydata",value=0)
            }
        }
    })

    observe({
        sRenameName <<- input$renameName
    })

    observe({
        if (!is.null(input$yesimsure))
        {
            if (input$yesimsure > 0)
            {
                # user has pressed yesimsure
                cat("yesimsure clicked\n")
                
                    cat(paste0("deleting ",sMarxanDir,"\n"))

                    # erase the database
                    system(paste0("rm -rf ",sMarxanDir))

                    system(paste0("touch ",sMarxanHome))
                    system(paste0("touch ",sMarZoneHome))

                    # refresh dataset list
                    list_dirs <- c(list.dirs(sMarxanHome,full.names = TRUE),
                                   list.dirs(sMarZoneHome,full.names = TRUE))
                    cat(paste0("new dataset detected","\n"))
                    ImportTime <<-max(file.info(list_dirs)$ctime)

                    a_choices <- c(list.dirs(sMarxanHome),list.dirs(sMarZoneHome))
                    # update the list of datasets to include the new one(s)
                    updateSelectInput(session, "database",
                                      choices = a_choices,
                                      selected = a_choices[1])

                    # trigger a refresh of the UI
                    irefreshtable <<- irefreshtable + 1
                    updateNumericInput(session, "refreshtable", value = irefreshtable)
                    updateNumericInput(session,"areyousure",value=0)

                    # display a message to user for 5 seconds
                    withProgress(message=paste0("Deleted ",sSelectDb),value=0,min=0,max=20, { Sys.sleep(5) })
            }
        }
    })

    output$mydatatable <- renderTable({

        input$removeOk

        input$refreshtable

        # parse the marxan and marzone databases, listing them in the grid
        col_names <- c("name","type","used","planning_units","features","polygons","leaflet","zones","costs","created","last_run")
        list_dirs_mx <- list.dirs(sMarxanHome,full.names=FALSE)
        list_dirs_mz <- list.dirs(sMarZoneHome,full.names=FALSE)
        for (i in 1:length(list_dirs_mx))
        {
            # read stats for this database
            sMarxanDir <- paste0(sMarxanHome,"/",list_dirs_mx[i],"/") 
            sName <- list_dirs_mx[i]
            sType <- "marxan"
            sPuRdataFile <- paste0(sMarxanDir,"/pulayer/pulayer.Rdata")
            sCreated <- as.character(file.info(sPuRdataFile)$ctime)
            sSumFile <- paste0(sMarxanDir,"/output/output_sum.csv")
            fUsed <- file.exists(sSumFile)
            if (fUsed)
            {
                sLastUsed <- as.character(file.info(sSumFile)$ctime)
            } else {
                sLastUsed <- ""
            }
            pudat <- read.csv(paste0(sMarxanDir,"/input/pu.dat"),stringsAsFactors=FALSE)
            sPlanningUnits <- nrow(pudat)
            specdat <- read.csv(paste0(sMarxanDir,"/input/spec.dat"),stringsAsFactors=FALSE)
            sFeatures <- nrow(specdat)

            putable <- read.dbf(paste0(sMarxanDir,"/pulayer/pulayer.dbf"))
            sPolygons <- nrow(putable)
            sZones <- ""
            sCosts <- ""

            fLeaflet <- file.exists(paste0(sMarxanDir,"/pulayer/leaflet.Rdata"))

            a_row <- c(sName,sType,as.character(fUsed),sPlanningUnits,sFeatures,sPolygons,as.character(fLeaflet),sZones,sCosts,sCreated,sLastUsed)
            if (i == 1)
            {
                the_table <- a_row
            } else {
                the_table <- rbind(the_table,a_row)
            }
        }
        for (i in 1:length(list_dirs_mz))
        {
            # read stats for this database
            sMarxanDir <- paste0(sMarZoneHome,"/",list_dirs_mz[i],"/") 
            sName <- list_dirs_mz[i]
            sType <- "marzone"
            sCreated <- as.character(file.info(sMarxanDir)$ctime)
            sSumFile <- paste0(sMarxanDir,"/output/output_sum.csv")
            fUsed <- file.exists(sSumFile)
            if (fUsed)
            {
                sLastUsed <- as.character(file.info(sSumFile)$ctime)
            } else {
                sLastUsed <- ""
            }
            pudat <- read.csv(paste0(sMarxanDir,"/input/pu.dat"),stringsAsFactors=FALSE)
            sPlanningUnits <- nrow(pudat)
            specdat <- read.csv(paste0(sMarxanDir,"/input/spec.dat"),stringsAsFactors=FALSE)
            sFeatures <- nrow(specdat)

            putable <- read.dbf(paste0(sMarxanDir,"/pulayer/pulayer.dbf"))
            sPolygons <- nrow(putable)
            zonesdat <- read.csv(paste0(sMarxanDir,"/input/zones.dat"),stringsAsFactors=FALSE)
            sZones <- nrow(zonesdat)
            costsdat <- read.csv(paste0(sMarxanDir,"/input/costs.dat"),stringsAsFactors=FALSE)
            sCosts <- nrow(costsdat)

            fLeaflet <- file.exists(paste0(sMarxanDir,"/pulayer/leaflet.Rdata"))

            a_row <- c(sName,sType,as.character(fUsed),sPlanningUnits,sFeatures,sPolygons,as.character(fLeaflet),sZones,sCosts,sCreated,sLastUsed)
            the_table <- rbind(the_table,a_row)
        }
        colnames(the_table) <- col_names
        rownames(the_table) <- rep("",nrow(the_table))

        return(the_table)
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
