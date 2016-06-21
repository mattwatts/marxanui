# marxan.io

# input is a zip file provided by user
# unzip to a temporary location
# detect pulayer
#   simplify geometry
#   detect field PUID or PU_ID
#   rename as PUID
#   drop all fields except PUID
#   save as pulayer.shp
# detect puoutline if exists
#   simplify geometry
#   save as puoutline.shp
# scan input.dat
#   detect pu.dat
#   detect spec.dat
#   detect bound.dat
#   detect puvsp.dat
# identify common errors
# convert matrix to sparse
# create puorder.dat and sporder.dat
# create pulayer.Rdata
# run Marxan
# create cluster.Rdata

# return error condition to Marxan web app
# if stop error condition, return useful error string
# return list of warning messages to Marxan web app

#' @export
smart_read <- function(sInFile)
{
    # are the files CSV, Tab, or comma delimeted?
    cat(paste0("smart_read reading file ",sInFile,"\n"))

    # automatically detect the delimeter type: comma, tab, or space
    sLine <- readLines(sInFile,1)

    if (grepl(",",sLine))
    {
        InTable <- read.csv(sInFile,stringsAsFactors=FALSE)
    }
    if (grepl("\t",sLine))
    {
        InTable <- read.delim(sInFile,stringsAsFactors=FALSE)
    }
    if (grepl(" ",sLine))
    {
        InTable <- read.table(sInFile,stringsAsFactors=FALSE,sep=" ")
    }

    cat(paste0("smart_read file read ",sInFile,"\n"))

    return(InTable)
}

#' @export
cat_log <- function(sMsg)
{
    # write to log file & write to console output
    write(paste0(sMsg," ",date()),file=sLogFile,append=TRUE)
    cat(paste0(sMsg,"\n"))
}

#' @export
unzip_file <- function(sInputZipFile,sTempPath,sShinyUserPath,sDataPath,sUserName)
{
    # unzip the zip file with the user dataset
    withProgress(message = "Please wait: processing dataset",value=0,
    {
        sPath <<- sTempPath
        
        iCountPU <<- 0
        iCountSpec <<- 0
        iCountBound <<- 0
        iCountMatrix <<- 0
        iCountZone <<- 0
        iCountPolygon <<- 0

        sLogFile <<- paste0(sPath,"/ParseMarxanZip.log")
        WarningMsg <<- c()
        ErrorMsg <<- c()

        write(paste0("ParseMarxanZip log start ",date()),file=sLogFile)
        cat_log(paste0("temp path ",sPath))

        withProgress(message="Reading zip",value=0,
        {
            incProgress(0.5,detail="Unzipping")
          
            #system(paste0("unzip ",sInputZipFile," -d ",sPath))
            unzip(sInputZipFile,overwrite=T,exdir=sPath)
          
            incProgress(1)
          
            cat(paste0("unzip done\n"))
      
        }) # with Progress Reading zip     
    })
}

#' @export
user_msg <- function(sMsg)
{
    # send a message to the user interface
    write(sMsg,file=sUserMessagesFile,append=TRUE)
}

#' @export
error_msg <- function(sMsg)
{
    # register an error message
    sErrorMsg <- paste0("ERROR: ",sMsg)
    user_msg(paste0("ERROR,",sMsg))
    cat_log(sErrorMsg)
    ErrorMsg <<- c(ErrorMsg,sErrorMsg)
}

#' @export
warning_msg <- function(sMsg)
{
    # register a warning message
    sWarningMsg <- paste0("Warning: ",sMsg)
    user_msg(paste0("Warning,",sMsg))
    cat_log(sWarningMsg)
    WarningMsg <<- c(WarningMsg,sWarningMsg)
}

#' @export
convert_eoln <- function(sInFile)
{
    if (fLinux)
    {
        # Linux
        system(paste0("dos2unix ",sInFile))
    } else {
        if (fWindows)
        {
            # Windows
            system2(paste0("type ",sInFile," | more /P > convert.dat"),wait=T)
            system2(paste0("cp convert.dat ",sInFile),wait=T)
        } else {
            # Mac
            system2(paste0("perl -pe 's/\r\n|\n|\r/\n/g' ",sInFile," > convert.dat"),wait=T)
            system2(paste0("cp convert.dat ",sInFile),wait=T)
        }
    }
}

#' @export
read_input_files <- function(sTempPath,sDataPath)
{
    # parse the Marxan input files in the user dataset
    withProgress(message = "Please wait: processing dataset",value=0,
    {
        sPath <<- sTempPath

        # init user messages
        sUserMessagesFile <<- paste0(sPath ,"/user_messages.txt")
        system(paste0("touch ",sUserMessagesFile))
        LastFileTime <<- file.info(sUserMessagesFile)$ctime
        fUserMessagesFile <<- TRUE

        withProgress(message="Input files",value=0,
        {
            # find input.dat
            sInputDat <- list.files(path = sPath, recursive = TRUE, pattern = "^input\\.dat$", ignore.case = TRUE, full.names = TRUE)
            if (length(sInputDat) == 0)
            {
                error_msg("input.dat not found")
                stop()
            }
            if (length(sInputDat) > 1)
            {
                error_msg(paste0("ERROR: more than 1 input.dat found: ",paste0(sInputDat,collapse=" ")))
                stop()
            }

            convert_eoln(sInputDat)

            cat_log(paste0("reading input.dat ",sInputDat))

            inputdat <- readLines(sInputDat)

            cat_log(paste0("input.dat read ",sInputDat))

            sPUNAME <- GetParamValue(inputdat,"PUNAME")
            sSPECNAME <- GetParamValue(inputdat,"SPECNAME")
            sPUVSPRNAME <- GetParamValue(inputdat,"PUVSPRNAME")
            sBOUNDNAME <- GetParamValue(inputdat,"BOUNDNAME")

            sZONESNAME <- GetParamValue(inputdat,"ZONESNAME")

            fMarZone <<- FALSE

            if (sZONESNAME != "")
            {
                fMarZone <<- TRUE
    
                # this is a MarZone dataset
                sCOSTSNAME <- GetParamValue(inputdat,"COSTSNAME")
                sZONECOSTNAME <- GetParamValue(inputdat,"ZONECOSTNAME")
                sZONEBOUNDCOSTNAME <- GetParamValue(inputdat,"ZONEBOUNDCOSTNAME")
                sZONECONTRIBNAME <- GetParamValue(inputdat,"ZONECONTRIBNAME")
                sZONECONTRIB2NAME <- GetParamValue(inputdat,"ZONECONTRIB2NAME")
                sZONECONTRIB3NAME <- GetParamValue(inputdat,"ZONECONTRIB3NAME")
                sZONETARGETNAME <- GetParamValue(inputdat,"ZONETARGETNAME")
                sZONETARGET2NAME <- GetParamValue(inputdat,"ZONETARGET2NAME")
                sPULOCKNAME <- GetParamValue(inputdat,"PULOCKNAME")
                sPUZONENAME <- GetParamValue(inputdat,"PUZONENAME")
            }

            if (fMarZone)
            {
                sOutDir <<- paste0(sPath,"/marzone")
                user_msg("MarZone dataset")
            } else {
                sOutDir <<- paste0(sPath,"/marxan")
                user_msg("Marxan dataset")
            }
  
            # create Marxan directories
            dir.create(sOutDir)
            dir.create(paste0(sOutDir,"/input"))
            dir.create(paste0(sOutDir,"/output"))
            dir.create(paste0(sOutDir,"/pulayer"))

            # parse the input file parameters
            if (sPUNAME == "")
            {
                error_msg("PUNAME not found in input.dat")
                stop()
            }
            if (sSPECNAME == "")
            {
                error_msg("SPECNAME not found in input.dat")
                stop()
            }
            if (sPUVSPRNAME == "")
            {
                error_msg("PUVSPRNAME not found in input.dat")
                stop()
            }
            cat_log("before fBoundDat")
            fBoundDat <<- TRUE
            if (sBOUNDNAME == "")
            {
                fBoundDat <<- FALSE
                warning_msg("BOUNDNAME not found in input.dat")
            }
            cat_log("after fBoundDat")

            if (fMarZone)
            {
                if (sCOSTSNAME == "")
                {
                    error_msg("COSTSNAME not found in input.dat")
                    stop()
                }
  
                if (sZONECOSTNAME == "")
                {
                    error_msg("ZONECOSTNAME not found in input.dat")
                    stop()
                }
            }

            # scan for the input files
            sPuDat <- list.files(path = sPath, recursive = TRUE, pattern = paste0("^",sPUNAME,"$"), ignore.case = TRUE, full.names = TRUE)
            sSpecDat <- list.files(path = sPath, recursive = TRUE, pattern = paste0("^",sSPECNAME,"$"), ignore.case = TRUE, full.names = TRUE)
            sPuvsprDat <- list.files(path = sPath, recursive = TRUE, pattern = paste0("^",sPUVSPRNAME,"$"), ignore.case = TRUE, full.names = TRUE)
            if (fBoundDat)
            {
                sBoundDat <- list.files(path = sPath, recursive = TRUE, pattern = paste0("^",sBOUNDNAME,"$"), ignore.case = TRUE, full.names = TRUE)
            }

            if (fMarZone)
            {
                fZoneBoundCost <- FALSE
                fZoneContrib <- FALSE
                fZoneContrib2 <- FALSE
                fZoneContrib3 <- FALSE
                fZoneTarget <- FALSE
                fZoneTarget2 <- FALSE
                fPuLock <- FALSE
                fPuZone <- FALSE

                sZonesDat <- list.files(path = sPath, recursive = TRUE, pattern = paste0("^",sZONESNAME,"$"), ignore.case = TRUE, full.names = TRUE)
                sCostsDat <- list.files(path = sPath, recursive = TRUE, pattern = paste0("^",sCOSTSNAME,"$"), ignore.case = TRUE, full.names = TRUE)
                sZoneCostDat <- list.files(path = sPath, recursive = TRUE, pattern = paste0("^",sZONECOSTNAME,"$"), ignore.case = TRUE, full.names = TRUE)
                if (sZONEBOUNDCOSTNAME != "")
                {
                    sZoneBoundCostDat <- list.files(path = sPath, recursive = TRUE, pattern = paste0("^",sZONEBOUNDCOSTNAME,"$"), ignore.case = TRUE, full.names = TRUE)
                    if (length(sZoneBoundCostDat) == 0)
                    {
                        error_msg(paste0("zoneboundcost.dat file ",sZONEBOUNDCOSTNAME," not found "))
                        stop()
                    }

                    if (length(sZoneBoundCostDat) > 1)
                    {
                        error_msg(paste0("more than 1 zoneboundcost.dat found: ",paste0(sZoneBoundCostDat,collapse=" ")))
                        stop()
                    }

                    fZoneBoundCost <- TRUE
                }
                if (sZONECONTRIBNAME != "")
                {
                    sZoneContribDat <- list.files(path = sPath, recursive = TRUE, pattern = paste0("^",sZONECONTRIBNAME,"$"), ignore.case = TRUE, full.names = TRUE)

                    if (length(sZoneContribDat) == 0)
                    {
                        error_msg(paste0("zonecontrib.dat file ",sZONECONTRIBNAME," not found "))
                        stop()
                    }

                    if (length(sZoneContribDat) > 1)
                    {
                        error_msg(paste0("more than 1 zonecontrib.dat found: ",paste0(sZoneContribDat,collapse=" ")))
                        stop()
                    }

                    fZoneContrib <- TRUE
                }

                if (sZONECONTRIB2NAME != "")
                {
                    sZoneContrib2Dat <- list.files(path = sPath, recursive = TRUE, pattern = paste0("^",sZONECONTRIB2NAME,"$"), ignore.case = TRUE, full.names = TRUE)

                    if (length(sZoneContrib2Dat) == 0)
                    {
                        error_msg(paste0("zonecontrib2.dat file ",sZONECONTRIB2NAME," not found "))
                        stop()
                    }

                    if (length(sZoneContrib2Dat) > 1)
                    {
                        error_msg(paste0("more than 1 zonecontrib2.dat found: ",paste0(sZoneContrib2Dat,collapse=" ")))
                        stop()
                    }

                    fZoneContrib2 <- TRUE
                }

                if (sZONECONTRIB3NAME != "")
                {
                    sZoneContrib3Dat <- list.files(path = sPath, recursive = TRUE, pattern = paste0("^",sZONECONTRIB3NAME,"$"), ignore.case = TRUE, full.names = TRUE)
    
                    if (length(sZoneContrib3Dat) == 0)
                    {
                        error_msg(paste0("zonecontrib3.dat file ",sZONECONTRIB3NAME," not found "))
                        stop()
                    }

                    if (length(sZoneContrib3Dat) > 1)
                    {
                        error_msg(paste0("more than 1 zonecontrib3.dat found: ",paste0(sZoneContrib3Dat,collapse=" ")))
                        stop()
                    }

                    fZoneContrib3 <- TRUE
                }

                if (sZONETARGETNAME != "")
                {
                    sZoneTargetDat <- list.files(path = sPath, recursive = TRUE, pattern = paste0("^",sZONETARGETNAME,"$"), ignore.case = TRUE, full.names = TRUE)

                    if (length(sZoneTargetDat) == 0)
                    {
                        error_msg(paste0("zonetarget.dat file ",sZONETARGETNAME," not found "))
                        stop()
                    }

                    if (length(sZoneTargetDat) > 1)
                    {
                        error_msg(paste0("more than 1 zonetarget.dat found: ",paste0(sZoneTargetDat,collapse=" ")))
                        stop()
                    }

                    fZoneTarget <- TRUE
                }

                if (sZONETARGET2NAME != "")
                {
                    sZoneTarget2Dat <- list.files(path = sPath, recursive = TRUE, pattern = paste0("^",sZONETARGET2NAME,"$"), ignore.case = TRUE, full.names = TRUE)

                    if (length(sZoneTarget2Dat) == 0)
                    {
                        error_msg(paste0("zonetarget2.dat file ",sZONETARGET2NAME," not found "))
                        stop()
                    }

                    if (length(sZoneTarget2Dat) > 1)
                    {
                        error_msg(paste0("more than 1 zonetarget2.dat found: ",paste0(sZoneTarget2Dat,collapse=" ")))
                        stop()
                    }

                    fZoneTarget2 <- TRUE
                }

                if (sPULOCKNAME != "")
                {
                    sPuLockDat <- list.files(path = sPath, recursive = TRUE, pattern = paste0("^",sPULOCKNAME,"$"), ignore.case = TRUE, full.names = TRUE)

                    if (length(sPuLockDat) == 0)
                    {
                        error_msg(paste0("pulock.dat file ",sPULOCKNAME," not found "))
                        stop()
                    }

                    if (length(sPuLockDat) > 1)
                    {
                        error_msg(paste0("more than 1 pulock.dat found: ",paste0(sPuLockDat,collapse=" ")))
                        stop()
                    }

                    fPuLock <- TRUE
                }

                if (sPUZONENAME != "")
                {
                    sPuZoneDat <- list.files(path = sPath, recursive = TRUE, pattern = paste0("^",sPUZONENAME,"$"), ignore.case = TRUE, full.names = TRUE)

                    if (length(sPuZoneDat) == 0)
                    {
                        error_msg(paste0("puzone.dat file ",sPUZONENAME," not found "))
                        stop()
                    }

                    if (length(sPuZoneDat) > 1)
                    {
                        error_msg(paste0("more than 1 puzone.dat found: ",paste0(sPuZoneDat,collapse=" ")))
                        stop()
                    }

                    fPuZone <- TRUE
                }
            }

            cat_log("paths searched")

            if (length(sPuDat) == 0)
            {
                error_msg(paste0("pu.dat file ",sPUNAME," not found"))
                stop()
            }
            if (length(sSpecDat) == 0)
            {
                error_msg(paste0("spec.dat file ",sSPECNAME," not found"))
                stop()
            }
            if (length(sPuvsprDat) == 0)
            {
                error_msg(paste0("puvspr.dat file ",sPUVSPRNAME," not found"))
                stop()
            }
            if (fBoundDat)
            {
                if (length(sBoundDat) == 0) { warning_msg(paste0("bound.dat file ",sBOUNDNAME," not found")) }
            }

            if (length(sPuDat) > 1)
            {
                error_msg(paste0("more than 1 pu.dat file ",sPUNAME," found"))
                stop()
            }
            if (length(sSpecDat) > 1)
            {
                error_msg(paste0("more than 1 spec.dat file ",sSPECNAME," found"))
                stop()
            }
            if (length(sPuvsprDat) > 1)
            {
                error_msg(paste0("more than 1 puvspr.dat file ",sPUVSPRNAME," found"))
                stop()
            }
            if (fBoundDat)
            {
                if (length(sBoundDat) > 1)
                {
                    error_msg(paste0("more than 1 bound.dat file ",sBOUNDNAME," found"))
                    stop()
                }
            }

            # convert eoln
            incProgress(0,detail="converting Marxan files eoln terminators")
            convert_eoln(sPuDat)
            convert_eoln(sSpecDat)
            convert_eoln(sPuvsprDat)
            if (fBoundDat)
            {
                if (length(sBoundDat) == 1) { convert_eoln(sBoundDat) }
            }
            if (fMarZone)
            {
                convert_eoln(sZonesDat)
                convert_eoln(sCostsDat)
                convert_eoln(sZoneCostDat)
                if (fZoneBoundCost) { convert_eoln(sZoneBoundCostDat) }
                if (fZoneContrib) { convert_eoln(sZoneContribDat) }
                if (fZoneContrib2) { convert_eoln(sZoneContrib2Dat) }
                if (fZoneContrib3) { convert_eoln(sZoneContrib3Dat) }
                if (fZoneTarget) { convert_eoln(sZoneTargetDat) }
                if (fZoneTarget2) { convert_eoln(sZoneTarget2Dat) }
                if (fPuLock) { convert_eoln(sPuLockDat) }
                if (fPuZone) { convert_eoln(sPuZoneDat) }
            }

            cat_log("reading input files")

            # read the default input.dat
            inputdat <- readLines(paste0(sDataPath,"/no_bound_input.dat"))

            # read the input files
            incProgress(0,detail="reading Marxan files")

            write(paste0("reading pu.dat ",date()),file=sLogFile,append=TRUE)
            pu.dat <<- smart_read(sPuDat)
            iCountPU <<- nrow(pu.dat)
            user_msg(paste0("planning units,",iCountPU))

            write(paste0("reading spec.dat ",date()),file=sLogFile,append=TRUE)
            spec.dat <- smart_read(sSpecDat)
            iCountSpec <<- nrow(spec.dat)
            user_msg(paste0("features,",iCountSpec))

            write(paste0("reading puvsp.dat ",date()),file=sLogFile,append=TRUE)
            puvspr.dat <- smart_read(sPuvsprDat)
            iCountMatrix <<- nrow(puvspr.dat)
            user_msg(paste0("matrix,",iCountMatrix))

            iCountBound <<- 0
            if (fBoundDat)
            {
                if (length(sBoundDat) == 1)
                {
                    write(paste0("reading bound.dat ",date()),file=sLogFile,append=TRUE)
                    bound.dat <- smart_read(sBoundDat)
                    iCountBound <<- nrow(bound.dat)
                    user_msg(paste0("connections,",iCountBound))
                } else {
                    fBoundDat <<- FALSE
                    warning_msg("bound.dat specified but file doesn't exist")
                }
            }
            cat_log("marxan files ok")

            # add the required input parameters
            inputdat <- c(inputdat,"")
            inputdat <- c(inputdat,"BOUNDNAME bound.dat")
            if (fMarZone)
            {
                # read and save the MarZone input files, adding required input parameters as we go
                inputdat <- c(inputdat,"")
                inputdat <- c(inputdat,"AVAILABLEZONE 1")
                inputdat <- c(inputdat,"ZONESNAME zones.dat")
                inputdat <- c(inputdat,"COSTSNAME costs.dat")
                inputdat <- c(inputdat,"ZONECOSTNAME zonecost.dat")

                incProgress(0,detail="reading MarZone files")

                zones.dat <- smart_read(sZonesDat)
                iCountZone <<- nrow(zones.dat)
                costs.dat <- smart_read(sCostsDat)
                iCountCost <<- nrow(costs.dat)
                zonecost.dat <- smart_read(sZoneCostDat)

                # process zone names in zones.dat : replace space with underscore
                ZoneNames <<- as.character(unlist(zones.dat$zonename))
                for (i in 1:length(ZoneNames))
                {
                    if (grepl(" ",ZoneNames[i]))
                    {
                        ZoneNames[i] <- paste(strsplit(ZoneNames[i],split=" ")[[1]],collapse="_")
                    }
                }
                zones.dat$zonename <- ZoneNames

                write.csv(zones.dat,paste0(sOutDir,"/input/zones.dat"),quote=FALSE,row.names=FALSE)
                write.csv(costs.dat,paste0(sOutDir,"/input/costs.dat"),quote=FALSE,row.names=FALSE)
                write.csv(zonecost.dat,paste0(sOutDir,"/input/zonecost.dat"),quote=FALSE,row.names=FALSE)

                if (fZoneBoundCost)
                {
                    zoneboundcost.dat <- smart_read(sZoneBoundCostDat)
                    write.csv(zoneboundcost.dat,paste0(sOutDir,"/input/zoneboundcost.dat"),quote=FALSE,row.names=FALSE)
                    inputdat <- c(inputdat,"ZONEBOUNDCOSTNAME zoneboundcost.dat")
                }
                if (fZoneContrib)
                {
                    zonecontrib.dat <- smart_read(sZoneContribDat)
                    write.csv(zonecontrib.dat,paste0(sOutDir,"/input/zonecontrib.dat"),quote=FALSE,row.names=FALSE)
                    inputdat <- c(inputdat,"ZONECONTRIBNAME zonecontrib.dat")
                }
                if (fZoneContrib2)
                {
                    zonecontrib2.dat <- smart_read(sZoneContrib2Dat)
                    write.csv(zonecontrib2.dat,paste0(sOutDir,"/input/zonecontrib2.dat"),quote=FALSE,row.names=FALSE)
                    inputdat <- c(inputdat,"ZONECONTRIB2NAME zonecontrib2.dat")
                }
                if (fZoneContrib3)
                {
                    zonecontrib3.dat <- smart_read(sZoneContrib3Dat)
                    write.csv(zonecontrib3.dat,paste0(sOutDir,"/input/zonecontrib3.dat"),quote=FALSE,row.names=FALSE)
                    inputdat <- c(inputdat,"ZONECONTRIB3NAME zonecontrib3.dat")
                }
                if (fZoneTarget)
                {
                    zonetarget.dat <- smart_read(sZoneTargetDat)
                    write.csv(zonetarget.dat,paste0(sOutDir,"/input/zonetarget.dat"),quote=FALSE,row.names=FALSE)
                    inputdat <- c(inputdat,"ZONETARGETNAME zonetarget.dat")
                }
                if (fZoneTarget2)
                {
                    zonetarget2.dat <- smart_read(sZoneTarget2Dat)
                    write.csv(zonetarget2.dat,paste0(sOutDir,"/input/zonetarget2.dat"),quote=FALSE,row.names=FALSE)
                    inputdat <- c(inputdat,"ZONETARGET2NAME zonetarget2.dat")
                }
                if (fPuLock)
                {
                    pulock.dat <- smart_read(sPuLockDat)
                    write.csv(pulock.dat,paste0(sOutDir,"/input/pulock.dat"),quote=FALSE,row.names=FALSE)
                    inputdat <- c(inputdat,"PULOCKNAME pulock.dat")
                }
                if (fPuZone)
                {
                    puzone.dat <- smart_read(sPuZoneDat)
                    write.csv(puzone.dat,paste0(sOutDir,"/input/puzone.dat"),quote=FALSE,row.names=FALSE)
                    inputdat <- c(inputdat,"PUZONENAME puzone.dat")
                }
            }

            # save the input.dat
            writeLines(inputdat,con=paste0(sOutDir,"/input.dat"))

            write(paste0("input files read ",date()),file=sLogFile,append=TRUE)
            cat("input files read\n")

            incProgress(0,detail="writing Marxan files")

            # save the Marxan input files
            write.csv(pu.dat,paste0(sOutDir,"/input/pu.dat"),quote=FALSE,row.names=FALSE)
            # if spec.dat has a name field, make the name field the last field
            # this will reduce errors where users include delimeter characters in the feature names
            cnames <- colnames(spec.dat)
            iName <- which(grepl("name",cnames))
            if (length(iName) > 0)
            {
                spec.dat <- spec.dat[c(cnames[-iName],"name")]
            }
            write.csv(spec.dat,paste0(sOutDir,"/input/spec.dat"),quote=FALSE,row.names=FALSE)
            if (fBoundDat)
            {
                write.csv(bound.dat,paste0(sOutDir,"/input/bound.dat"),quote=FALSE,row.names=FALSE)
            }

            incProgress(0,detail="converting matrix file")
          
            if (ncol(puvspr.dat) == 3)
            {
                # fix the column names if they are non-compliant
                # should be species,pu,amount
                col_names <- c("species","pu","amount")
                if (min((col_names == colnames(puvspr.dat))) == 0)
                {
                    # col names are wrong
                    warning_msg(paste0("Warning: puvspr.dat file ",sPuvsprDat," has wrong column names"))
                    colnames(puvspr.dat) <- col_names
                }
                # This is a sparse matrix. Ensure it is sorted in the correct order.
                puorder.dat <- puvspr.dat[order(puvspr.dat$pu),]
                write.csv(puorder.dat,paste0(sOutDir,"/input/puorder.dat"),quote=FALSE,row.names=FALSE)
                sporder.dat <- puvspr.dat[order(puvspr.dat$species),]
                write.csv(sporder.dat,paste0(sOutDir,"/input/sporder.dat"),quote=FALSE,row.names=FALSE)

                # If puorder.dat and puvspr.dat do not have pu in the same order, warn user their matrix wasn't sorted ok.
                if (sum(order(puvspr.dat$pu) != order(puorder.dat$pu)) > 0)
                {
                    warning_msg(paste0("Warning: puvspr.dat file ",sPuvsprDat," was not ordered from lowest to highest pu"))
                }
            } else {
                # we need to convert this to a sparse matrix
                # generate puorder.dat
                sPuOrder <- paste0(sOutDir,"/input/puorder.dat")
                write('species,pu,amount',file=sPuOrder)
                for (j in 1:nrow(matrix))
                {
                    for (i in 2:ncol(matrix))
                    {
                        rAmount <- matrix[j,i]

                        if (rAmount > 0)
                        {
                            iPUID <- matrix[j,1]
                            iSPID <- substring(colnames(matrix)[i],2)

                            write(paste(iSPID,iPUID,rAmount,sep=","),
                                  file=sPuOrder,append=TRUE)
                        }
                    }
                }
                # generate sporder.dat
                sSpOrder <- paste0(sOutDir,"/input/sporder.dat")
                write('species,pu,amount',file=sSpOrder)
                for (i in 2:ncol(matrix))
                {
                    for (j in 1:nrow(matrix))
                    {
                        rAmount <- matrix[j,i]

                        if (rAmount > 0)
                        {
                          iPUID <- matrix[j,1]
                          iSPID <- substring(colnames(matrix)[i],2)

                          write(paste(iSPID,iPUID,rAmount,sep=","),
                                file=sSpOrder,append=TRUE)
                        }
                    }
                }
                warning_msg(paste0("puvspr.dat file ",sPuvsprDat," was not in sparse matrix format"))
            }

            cat_log("marxan files processed")

        }) # withProgress Reading marxan files

        withProgress(message="Planning units",value=0,
        {
            # read the dbf table - so user can select PUID field
            incProgress(0,detail="finding shapefile")
            # find shapefiles
            ShapeFiles <- list.files(path = sPath, recursive = TRUE, pattern = "*.shp$", ignore.case = TRUE, full.names = TRUE)

            # ignore any .xlm files if they are present
            WhichXml <- regexpr(".xml",ShapeFiles) > 0
            FilterShapes <- c()
            for (i in 1:length(ShapeFiles))
            {
                if (WhichXml[i] == FALSE)
                {
                  FilterShapes <- c(FilterShapes,ShapeFiles[i])
                }
            }

            if (length(FilterShapes) > 0)
            {
                if (length(FilterShapes) == 1)
                {
                    # we have 1 shapefile and assume this is the planning unit layer
                    sPuLayer <<- FilterShapes[1]
                    sOutlineLayer <- ""
                } else {
                    if (length(FilterShapes) == 2)
                    {
                        # we have 2 shapefiles: assume one is planning unit layer and other is outline
                        # guess which is which
                        WhichShapefile <- regexpr("pulayer.shp",FilterShapes) > 0
                        if (sum(WhichShapefile) != 1)
                        {
                            error_msg("2 shapefiles found, can't guess which one is the pulayer")
                            stop()
                        }
                        if (WhichShapefile[1] == TRUE)
                        {
                            sPuLayer <<- FilterShapes[1]
                            sOutlineLayer <- FilterShapes[2]
                        } else {
                            sPuLayer <<- FilterShapes[2]
                            sOutlineLayer <- FilterShapes[1]
                        }
                    } else {
                        error_msg("more than 2 shapefiles found")
                        stop()
                    }
                }
            }

            incProgress(0,detail="reading dbf table")
            tryCatch(
            {
                sDirNamePuLayer <- dirname(sPuLayer)
                sDbfTable <- paste0(sDirNamePuLayer,"/",file_path_sans_ext(basename(sPuLayer)),".dbf")
                if (!file.exists(sDbfTable))
                {
                    sDbfTable <- paste0(sDirNamePuLayer,"/",file_path_sans_ext(basename(sPuLayer)),".DBF")
                }
                putable <- read.dbf(sDbfTable)
                iCountPolygon <<- nrow(putable)
                user_msg(paste0("polygons,",iCountPolygon))
            },
            error=function(cond)
            {
                error_msg(paste0("can't read dbf table >",cond,"<"))
                stop()
            })

            puid_choices <<- colnames(putable)

            # guess which field is PUID
            iPUIDfield <- which(colnames(putable) == "PUID")
            if (length(iPUIDfield) == 0)
            {
                iPUIDfield <- which(colnames(putable) == "PU_ID")
            }
            if (length(iPUIDfield) == 0)
            {
                iPUIDfield <- which(colnames(putable) == "puid")
            }
            if (length(iPUIDfield) == 0)
            {
                iPUIDfield <- which(colnames(putable) == "pu_id")
            }

            if (length(iPUIDfield) == 0)
            {
                # can't guess which field is PUID: need to ask the user which one it is
                warning_msg("can't guess which field is PUID")
                iPUIDfield <<- 0
            } else {
                iPUIDfield <<- iPUIDfield
            }
            colnames(putable)[iPUIDfield] <- "PUID"
            putable <<- putable

            cat_log("pulayer dbf read")

        }) # withProgress Reading marxan files
    })
}

#' @export
read_polygons <- function()
{
    # parse the planning unit polygons in the user dataset
    withProgress(message = "Please wait: processing dataset",value=0,
    {
        withProgress(message="Planning units",value=0,
        {
            user_msg("pulayer")
            incProgress(0,detail="reading shapefile")

            tryCatch(
            {
                # load the planning unit shapefile
                pushapefile <- readOGR(dirname(sPuLayer),file_path_sans_ext(basename(sPuLayer)))
            },
            error=function(cond)
            {
                error_msg(paste0("can't read shapefile >",cond,"<"))
                stop()
            })

            incProgress(0,detail="writing shapefile")

            tryCatch(
            {
                writeOGR(pushapefile,paste0(sOutDir,"/pulayer"),"pulayer",driver="ESRI Shapefile",overwrite_layer=TRUE)
            },
            error=function(cond)
            {
                error_msg(paste0("can't write shapefile >",cond,"<"))
                stop()
            })
            
            # make the boundary length file
            if (!fBoundDat)
            {
                incProgress(0,detail="generating bound.dat")
                
                library(marxan)
                blf <- calcBoundaryData(pushapefile)
                write.table(blf,paste0(sOutDir,"/input/bound.dat"),row.names=F,sep=",")
            }

            cat_log(paste0("pulayer read ",sPuLayer))

            incProgress(0,detail="querying dbf table")

            colnames(putable)[iPUIDfield] <- "PUID"
            putable <- sqldf("SELECT PUID FROM putable")

            incProgress(0,detail="writing dbf table")

            write.dbf(putable,paste0(sOutDir,"/pulayer/pulayer.dbf"))
  
            incProgress(0,detail="simplifying shapefile")

            tryCatch(
            {
                # simplify the planning unit layer
                pushapefile_simp10 <- gSimplify(pushapefile,tol=10,topologyPreserve=TRUE)
            },
            error=function(cond)
            {
                error_msg(paste0("ERROR: can't simplify shapefile >",cond,"<"))
                stop()
            })

            cat_log("planning units simplified")

            # if the .prj file is present for the planning units, prepare leaflet data
            sPrjFilename <- paste0(dirname(sPuLayer),"/",file_path_sans_ext(basename(sPuLayer)),".prj")
            fPrjExists <- file.exists(sPrjFilename)
            if (fPrjExists)
            {
                incProgress(0,detail="preparing leaflet polygons")
                
                proj4string(pushapefile_simp10) <- proj4string(pushapefile)
                proj10 <- spTransform(pushapefile_simp10, CRS("+proj=longlat +ellps=WGS84"))
                sLeafletRdata <- paste0(sOutDir,"/pulayer/leaflet.Rdata")
                save(proj10,file=sLeafletRdata)
            }

            incProgress(0,detail="dissolving shapefile")

            tryCatch(
            {
                # dissolve the planning unit layer
                pushapefile_dissolve <- gUnaryUnion(pushapefile_simp10)
            },
            error=function(cond)
            {
                error_msg(paste0("can't dissolve shapefile >",cond,"<"))
                stop()
            })

            cat_log("planning units dissolved")

            incProgress(0,detail="creating shapefile outlines")

            tryCatch(
            {
                # remove the holes from dissolved polygon
                outerRings <- Filter(function(f){f@ringDir==1},pushapefile_dissolve@polygons[[1]]@Polygons)
                outerBounds <- SpatialPolygons(list(Polygons(outerRings,ID=1)))
                puoutline <- SpatialPolygons2PolySet(outerBounds)
                fOutline <- TRUE
            },
            error=function(cond)
            {
                error_msg(paste0("can't create shapefile outlines >",cond,"<"))
                stop()
            })

            incProgress(0,detail="converting shapefile")

            tryCatch(
            {
                # create the pulayer.Rdata file
                if (!fMarZone) # marzone doesn't use status
                {
                    pustatus_ <- unlist(pu.dat$status)
                }
                pulayer_ <<- SpatialPolygons2PolySet(pushapefile_simp10)
                y_ <- bbox(pushapefile_simp10)[2,2] - bbox(pushapefile_simp10)[2,1]
                x_ <- bbox(pushapefile_simp10)[1,2] - bbox(pushapefile_simp10)[1,1]
            },
            error=function(cond)
            {
                error_msg(paste0("can't convert shapefile >",cond,"<"))
                stop()
            })

            incProgress(0,detail="saving pulayer.Rdata")

            tryCatch(
            {
                sRdata <- paste0(sOutDir,"/pulayer/pulayer.Rdata")
                if (fMarZone) # marzone doesn't use status
                {
                    save(fOutline,puoutline,pulayer_,x_,y_,file=sRdata)
                } else {
                    save(fOutline,puoutline,pulayer_,pustatus_,x_,y_,file=sRdata)
                }
            },
            error=function(cond)
            {
                error_msg(paste0("can't save pulayer.Rdata >",cond,"<"))
                stop()
            })

            cat_log(paste0("pulayer.Rdata created ",sRdata))
            cat_log("shapefiles processed")
            cat_log("ParseMarxanZip end")
        })
    })
}

#' @export
ReadParseErrors <- function(ParseResult)
{
    Warnings <<- c()
    Errors <<- c()
    if (length(ParseResult) > 2)
    {
        for (i in 3:length(ParseResult))
        {
            if (grepl("Warning",ParseResult[i]))
            {
                Warnings <<- c(Warnings,ParseResult[i])
            }
            if (grepl("ERROR",ParseResult[i]))
            {
                Errors <<- c(Errors,ParseResult[i])
            }
        }
    }
}

#' @export
IngestMarxanDatabase <- function(sPath,sShinyUserPath,sUserName,sDatabaseName)
{
    # copy the ingested Marxan dataset to the users home directory
    dir.create(paste0(sShinyUserPath,"/",sUserName))
    if (fMarZone)
    {
        dir.create(paste0(sShinyUserPath,"/",sUserName,"/marzone/",sDatabaseName))
        system(paste0("cp -r ",sPath,"/marzone/ ",sShinyUserPath,"/",sUserName,"/marzone/",sDatabaseName))
    } else {
        dir.create(paste0(sShinyUserPath,"/",sUserName,"/marxan/",sDatabaseName))
        system(paste0("cp -r ",sPath,"/marxan/ ",sShinyUserPath,"/",sUserName,"/marxan/",sDatabaseName))
    }
}

#' @export
SafeDbName <- function(sDbName,sShinyUserPath,sUserName)
{
    # sDbName is the name the user wants to use
    # if it conflicts with another existing database, automatically fix it
    UserDb <- list.dirs(paste0(sShinyUserPath,"/",sUserName))
    sFixName <- sDbName

    iMatch <- grep(paste0("^",sDbName,"$"),UserDb)
    if (length(iMatch) > 0)
    {
        # database exists, generate a replacement name
        for (i in 1:1000)
        {
            sFixName <- sprintf("%s%s",sDbName,round(runif(1)*1000))
            iMatch <- grep(paste0("^",sFixName,"$"),UserDb)
            if (length(iMatch) == 0)
            {
                break()
            }
        }
    }
    return(sFixName)
}
