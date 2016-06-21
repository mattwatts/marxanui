# marxan.io

# init some variables
rprop <<- 0.3
iM <<- 1
fMarxanRunning <<- FALSE
sUserInterface <<- "Select Database"
fAllow <<- FALSE

#' @export
PadInt <- function(iRunNumber)
{
    iPad <- 5 - nchar(as.character(iRunNumber))
    return(paste0(paste0(rep("0",iPad),collapse=""),iRunNumber))
}

#' @export
GetOutputFileext <- function(sMarxanDir,sParam)
{
    inputdat <- readLines(paste(sMarxanDir,"/input.dat",sep=""))
    iParam <- which(regexpr(sParam,inputdat)==1)

    iValue <- as.integer(unlist(strsplit(inputdat[iParam], split=" "))[2])

    if (iValue == 1) { return(".dat") }
    if (iValue == 2) { return(".txt") }
    if (iValue == 3) { return(".csv") }
}

# create a temporary directory in a specified directory
#' @export
CreateTempDir <- function(sTempPath)
{
    Gfold <- sprintf("%s",round(runif(1)*1000000))
    for (ii in 1:100000)
    {
        sPath <- sprintf("%s/%s",sTempPath,Gfold)
        if(!file.exists(sPath))
        {
            if (fWindows)
            {
                #system2("md",sPath)
                dir.create(sPath)
            } else {
                system(paste("mkdir ",sPath))
            }
            break()
        }
    }
    return(sPath)
}

#' @export
CreateTempUploadDir <- function(sTempPath)
{
    Gfold <- sprintf("%s",round(runif(1)*1000000))
    for (ii in 1:100000)
    {
        sPath <- sprintf("%s/upload_%s",sTempPath,Gfold)
        if(!file.exists(sPath))
        {
            if (fWindows)
            {
                #system2("md",sPath)
                dir.create(sPath)
            } else {
                system(paste("mkdir ",sPath))
            }
            break()
        }
    }
    return(sPath)
}

#' @export
GetParamValue <- function(inputdat,sParam)
{
    iParam <- which(regexpr(sParam,inputdat)==1)
    if (length(iParam) > 0)
    {
        return(sapply(strsplit(inputdat[iParam]," "),"[",2))
    } else {
        return("")
    }
}

#' @export
JoinParallelResults_MarZone <- function(sMarxanDir,iCores,iRepsPerCore,iZones)
{
    iSolutions <- round(iCores*iRepsPerCore)
    # combine the summary tables
    sumtable <- c()
    for (i in 1:iCores)
    {
        sumtable_ <- read.csv(paste0(sMarxanDir,"/output/output",i,"_sum.csv"))
        sumtable <- rbind(sumtable,sumtable_)
    }
    for (i in 1:iSolutions)
    {
        sumtable[i,1] <- i
    }
    write.csv(sumtable,
              paste0(sMarxanDir,"/output/output_sum.csv"),
              quote=FALSE,row.names=FALSE)

    # detect best solution
    iBest <- which(sumtable[,2]==min(sumtable[,2]))
    if (length(iBest) > 0)
    {
        iBest <- iBest[1]
    }
    
    # rename mv files and solution files
    iSol <- 0
    for (i in 1:iCores)
    {
        for (j in 1:iRepsPerCore)
        {
            iSol <- iSol + 1
        
            file.rename(paste0(sMarxanDir,"/output/output",i,"_mv",PadInt(j),".csv"),
                        paste0(sMarxanDir,"/output/output_mv",PadInt(iSol),".csv"))
        
            file.rename(paste0(sMarxanDir,"/output/output",i,"_r",PadInt(j),".csv"),
                        paste0(sMarxanDir,"/output/output_r",PadInt(iSol),".csv"))
        }
    }

    # copy _mvbest and _best files
    file.copy(paste0(sMarxanDir,"/output/output_mv",PadInt(iBest),".csv"),
              paste0(sMarxanDir,"/output/output_mvbest.csv"),
              overwrite=TRUE)
    file.copy(paste0(sMarxanDir,"/output/output_r",PadInt(iBest),".csv"),
              paste0(sMarxanDir,"/output/output_best.csv"),
              overwrite=TRUE)

    # join ssoln files
    ssolntable_ <- read.csv(paste0(sMarxanDir,"/output/output1_ssoln.csv"))
    selectionfreqs <- rep(0,nrow(ssolntable_))
    for (i in 2:iZones)
    {
      selectionfreqs <- cbind(selectionfreqs,rep(0,nrow(ssolntable_)))
    }

    for (j in 1:iZones)
    {
        selectionfreqs[,j] <- selectionfreqs[,j] + ssolntable_[,j+2]
    }
    for (i in 2:iCores)
    {
        ssolntable_ <- read.csv(paste0(sMarxanDir,"/output/output",i,"_ssoln.csv"))
        for (j in 1:iZones)
        {
            selectionfreqs[,j] <- selectionfreqs[,j] + ssolntable_[,j+2]
        }
    }
    ssolntable <- cbind(ssolntable_$planning.unit,(ssolntable_$number*10),selectionfreqs)
    colnames(ssolntable) <- c("planning.unit","number",ZoneNames)
    write.csv(ssolntable,
                  paste0(sMarxanDir,"/output/output_ssoln.csv"),
                  quote=FALSE,row.names=FALSE)

    # join cluster files: text parse
    outfile <- file(paste0(sMarxanDir,"/output/output_solutionsmatrix.csv"),"w")
    iRow <- 0
    for (i in 1:iCores)
    {
        infile <- file(paste0(sMarxanDir,"/output/output",i,"_solutionsmatrix.csv"),"r")
        # read header row
        sLine <- readLines(con=infile,n=1)
  
        # write header row if i == 1
        if (i == 1)
        {
            write(sLine,file=outfile)
        }
    
        for (j in 1:(iZones*iRepsPerCore))
        {
            sLine <- readLines(con=infile,n=1)
            write(sLine,file=outfile,append=TRUE)
        }
        close(infile)
    }
    close(outfile)
    # load the joined cluster file and fix the row names
    solutionsmatrix <- read.csv(paste0(sMarxanDir,"/output/output_solutionsmatrix.csv"))
    solutionsmatrix$SolutionsMatrix <- as.character(solutionsmatrix$SolutionsMatrix)
    iInc <- 0
    for (i in 1:iSolutions)
    {
      for (j in 1:iZones)
      {
         iInc <- iInc + 1
         solutionsmatrix$SolutionsMatrix[iInc] <- paste0("Z",j,"S",i)
      }
    }
    write.csv(solutionsmatrix,paste0(sMarxanDir,"/output/output_solutionsmatrix.csv"),quote=FALSE,row.names=FALSE)
}

#' @export
JoinParallelResults <- function(sMarxanDir,iCores,iRepsPerCore)
{
    iSolutions <- round(iCores*iRepsPerCore)
    # combine the summary tables
    sumtable <- c()
    for (i in 1:iCores)
    {
        sumtable_ <- read.csv(paste0(sMarxanDir,"/output/output",i,"_sum.csv"))
        sumtable <- rbind(sumtable,sumtable_)
    }
    for (i in 1:iSolutions)
    {
        sumtable[i,1] <- i
    }
    write.csv(sumtable,
              paste0(sMarxanDir,"/output/output_sum.csv"),
              quote=FALSE,row.names=FALSE)

    # detect best solution
    iBest <- which(sumtable$Score==min(sumtable$Score))
    if (length(iBest) > 0)
    {
        iBest <- iBest[1]
    }

    # rename mv files and solution files
    iSol <- 0
    for (i in 1:iCores)
    {
        for (j in 1:iRepsPerCore)
        {
            iSol <- iSol + 1

            file.rename(paste0(sMarxanDir,"/output/output",i,"_mv",PadInt(j),".csv"),
                        paste0(sMarxanDir,"/output/output_mv",PadInt(iSol),".csv"))

            file.rename(paste0(sMarxanDir,"/output/output",i,"_r",PadInt(j),".csv"),
                        paste0(sMarxanDir,"/output/output_r",PadInt(iSol),".csv"))
        }
    }

    # copy _mvbest and _best files
    file.copy(paste0(sMarxanDir,"/output/output_mv",PadInt(iBest),".csv"),
              paste0(sMarxanDir,"/output/output_mvbest.csv"),
              overwrite=TRUE)
    file.copy(paste0(sMarxanDir,"/output/output_r",PadInt(iBest),".csv"),
              paste0(sMarxanDir,"/output/output_best.csv"),
              overwrite=TRUE)

    # join ssoln files
    ssolntable <- read.csv(paste0(sMarxanDir,"/output/output",i,"_ssoln.csv"))
    colnames(ssolntable)[2] <- "numberX"
    for (i in 2:iCores)
    {
        ssolntable_ <- read.csv(paste0(sMarxanDir,"/output/output",i,"_ssoln.csv"))
        ssolntable <- sqldf("SELECT * from ssolntable LEFT JOIN ssolntable_ USING(planning_unit)")
        ssolntable$numberX <- ssolntable$numberX + ssolntable$number
        ssolntable <- sqldf("SELECT planning_unit, numberX from ssolntable")
    }
    colnames(ssolntable)[2] <- "number"
    write.csv(ssolntable,
              paste0(sMarxanDir,"/output/output_ssoln.csv"),
              quote=FALSE,row.names=FALSE)

    # join cluster files: text parse
    outfile <- file(paste0(sMarxanDir,"/output/output_solutionsmatrix.csv"),"w")
    iRow <- 0
    for (i in 1:iCores)
    {
        infile <- file(paste0(sMarxanDir,"/output/output",i,"_solutionsmatrix.csv"),"r")
        # read header row
        sLine <- readLines(con=infile,n=1)

        # write header row if i == 1
        if (i == 1)
        {
            write(sLine,file=outfile)
        }

        for (j in 1:iRepsPerCore)
        {
            sLine <- readLines(con=infile,n=1)
            iLen <- nchar(sLine)
            if (j < iRepsPerCore)
            {
                # S1..S9 : remove 3 chars
                sLine <- substr(sLine, 4, iLen)
            } else {
                # S10 : remove 4 chars
                sLine <- substr(sLine, 5, iLen)
            }
            iRow <- iRow + 1
            write(paste0("S",iRow,",",sLine),file=outfile,append=TRUE)
        }
        close(infile)
    }
    close(outfile)
}

#' @export
ImportOutputsCsvToShpDbf <- function(sPuShapeFileDbf, sMarxanDir, iNumberOfRuns, sPUID)
{
    # Imports the relevant contents of output files to the planning unit shape file dbf.
    # load and prepare pu_table
    pu_table <- read.dbf(sPuShapeFileDbf)
    pu_table <- sqldf(paste("SELECT ", sPUID, " from pu_table",sep=""))
    colnames(pu_table)[1] <- "PUID"

    pu_table$PUID <- as.integer(pu_table$PUID)

    # load and prepare ssoln_table
    ssoln_table <- read.csv(paste(sMarxanDir,"/output/output_ssoln",GetOutputFileext(sMarxanDir,"SAVESUMSOLN"),sep=""))
    colnames(ssoln_table)[1] <- "PUID"
    colnames(ssoln_table)[2] <- "SSOLN2"
    ssoln_table$SSOLN1 <- as.integer(iNumberOfRuns - ssoln_table$SSOLN2)
    ssoln_table$SSOLN2 <- as.integer(ssoln_table$SSOLN2)

    # join pu_table and ssoln_table
    pu_table <- sqldf("SELECT * from pu_table LEFT JOIN ssoln_table USING(PUID)")

    # load and prepare best_table
    best_table <- read.csv(paste(sMarxanDir,"/output/output_best",GetOutputFileext(sMarxanDir,"SAVEBEST"),sep=""))
    best_table$BESTSOLN <- as.integer(best_table$SOLUTION + 1)
    best_table <- sqldf("SELECT PUID, BESTSOLN from best_table")

    # join pu_table and best_table
    pu_table <- sqldf("SELECT * from pu_table LEFT JOIN best_table USING(PUID)")

    # save the new pu_table
    colnames(pu_table)[1] <- sPUID
    write.dbf(pu_table,sPuShapeFileDbf)
}

#' @export
labelCol <- function(x)
{
    # we set iBest as a global in PrepareCluster_compute before calling labelCol
    if (is.leaf(x))
    {
        a <- attributes(x)
        label <- attr(x, "label")
        colour <- "black"
        if (label == paste0("S",iBest," (Best)")) { colour <- "blue" }
        attr(x, "nodePar") <- c(a$nodePar, lab.col = colour)
    }
    return(x)
}

#' @export
labelCol_marzone <- function(x)
{
    if (is.leaf(x))
    {
        a <- attributes(x)
        label <- attr(x, "label") 
        colour <- "black"
        for (i in 1:iZones)
        {
            sZone <- paste0("Z",i)
            if (substring(label,1,2) == sZone) { colour <- rainbow(iZones)[i] }
        }
        attr(x, "nodePar") <- c(a$nodePar, lab.col = colour)
    }
    return(x)
}

#' @export
PrepareCluster_compute_MarZone <- function(sMarxanDir)
{
    # NOTE: we fail gracefully if there are not enough unique solutions
    # prepare the cluster analysis objects
    withProgress(message="Load cluster matrix",value=0,
    {
        solutions_raw<-read.table(paste0(sMarxanDir,"/output/output_solutionsmatrix.csv"),header=TRUE, row.name=1, sep=",")
    })
    
    withProgress(message="Find unique solutions",value=0,
    {
        solutions <- unique(solutions_raw)
        iUniqueSolutions <- dim(solutions)[1]
    })
    
    if (iUniqueSolutions > 2)
    {
        # render the 2d
        withProgress(message="Compute distance",value=0,
        {
            rainbowpalette <- rainbow(iZones)
            nmdscolours <- rep("white",each = iUniqueSolutions)
            soldist<-vegdist(solutions,distance="jaccard")
        })
        withProgress(message="Compute 2ds",value=0,
        {
            sol.mds<-nmds(soldist,2)
        })
        withProgress(message="Compute clusters",value=0,
        {
            h<-hclust(soldist, method="complete")
        })
        withProgress(message="Compute palette",value=0,
        {
            plotlabels <- row.names(solutions)
            iCount <- 0
            for (j in 1:length(nmdscolours))
            {
                # plotlabel like "Z1S1"
                x <- strsplit(plotlabels[j],split="S")[1]
                y <- strsplit(x[[1]][1],split="Z")
                iZone <- as.numeric(y[[1]][2])

                # the plotlabel displayed is just the solution number
                plotlabels[j] <- paste0(x[[1]][2])

                iCount <- iCount + 1
                nmdscolours[iCount] <- rainbowpalette[iZone]
            }
        })

        withProgress(message="Compute dendogram",value=0,
        {
            # render the dendogram
            d <- dendrapply(as.dendrogram(h), labelCol_marzone)
        })

        withProgress(message="Compute 3ds",value=0,
        {
            # render the 3d
            #sol3d.mds <- nmds(soldist,3)
            sol3d.mds <- NA
        })
    } else {
        sol.mds <- NA
        plotlabels <- NA
        nmdscolours <- NA
        d <- NA
        sol3d.mds <- NA
    }
    sRdata <- paste0(sMarxanDir,"/output/cluster.Rdata")
    save(sol.mds,plotlabels,nmdscolours,d,sol3d.mds,file=sRdata)
}

#' @export
PrepareCluster_compute <- function(sMarxanDir)
{
    # NOTE: we fail gracefully if there are not enough unique solutions
    # prepare the cluster analysis objects
    solutions_raw<-read.table(paste0(sMarxanDir,"/output/output_solutionsmatrix.csv"),header=TRUE, row.name=1, sep=",")
    thetable <- read.csv(paste0(sMarxanDir,"/output/output_sum.csv"))
    iBest <<- which.min(thetable$Score)
    Best <- solutions_raw[iBest,]
    solutions_raw <- solutions_raw[-iBest,]
    solutions_join <- rbind(Best,solutions_raw)

    rownames(solutions_join) <- c(paste0("S",iBest," (Best)"),row.names(solutions_raw))
    plotlabels <- c(paste0("S",iBest," (Best)"),row.names(solutions_raw))

    solutions <- unique(solutions_join)
    iUniqueSolutions <- dim(solutions)[1]
    if (iUniqueSolutions > 2)
    {
        # render the 2d
        nmdscolours <- rep("black",each = iUniqueSolutions)
        nmdscolours[1] <- "blue"
        soldist<-vegdist(solutions,distance="jaccard")
        sol.mds<-nmds(soldist,2)
        h<-hclust(soldist, method="complete")

        # render the dendogram
        d <- dendrapply(as.dendrogram(h), labelCol)

        # render the 3d
        #sol3d.mds <- nmds(soldist,3)
        sol3d.mds <- NA
    } else {
        sol.mds <- NA
        plotlabels <- NA
        nmdscolours <- NA
        d <- NA
        sol3d.mds <- NA
    }
    sRdata <- paste0(sMarxanDir,"/output/cluster.Rdata")
    save(sol.mds,plotlabels,nmdscolours,d,sol3d.mds,file=sRdata)
}

#' @export
list.dirs <- function(path=".", pattern=NULL, all.dirs=FALSE,
                      full.names=FALSE, ignore.case=FALSE)
{
    # use full.names=TRUE to pass to file.info
    all <- list.files(path, pattern, all.dirs,
                      full.names=TRUE, recursive=FALSE, ignore.case)
    dirs <- all[file.info(all)$isdir]
    # determine whether to return full names or just dir names
    if(isTRUE(full.names))
      return(dirs)
    else
      return(basename(dirs))
}

#' @export
SelectDatabase <- function(sCallingApp,session)
{
    load(file=paste0(sMarxanDir,"/pulayer/pulayer.Rdata"))
    puoutline <<- puoutline
    pulayer_ <<- pulayer_
    specdat <- read.csv(paste(sMarxanDir,"/input/spec.dat",sep=""),stringsAsFactors=FALSE)
    # if name not present in spec.dat, use id as name
    if("name" %in% colnames(specdat))
    {
        specnames <<- unlist(as.character(specdat$name))
    } else {
        specnames <<- unlist(as.character(specdat$id))
    }
    sfeature <<- "All features"
    # to display the polygon set in the correct aspect ratio
    iAspectX <<- max(pulayer_$X)-min(pulayer_$X)
    iAspectY <<- max(pulayer_$Y)-min(pulayer_$Y)
    if ((sCallingApp == "marxan") | (sCallingApp == "mxptest"))
    {
        pustatus_ <<- pustatus_
        # read the BLM, prop, SPF ui control values
        inputdat <- readLines(paste(sMarxanDir,"/input.dat",sep=""))
        sParamBLM <- GetParamValue(inputdat,"BLM")
        if (sParamBLM != "")
        {
            sBLM <<- sParamBLM
            cat(paste0("sBLM ",sBLM,"\n"))
        }
    }
    if (sCallingApp == "marzone")
    {
        # iZones, ZoneNames
        zonesdat <- read.csv(paste0(sMarxanDir,"/input/zones.dat"))
        iZones <<- nrow(zonesdat)
        ZoneNames <<- as.character(unlist(zonesdat$zonename))
        
        if (fLocalUser)
        {
            sImagePath <- paste0(sShinyPath,"/marxanui/images/")
            dir.create(sImagePath)
        } else {
            sImagePath <- paste0(sShinyPath,"/images/")
        }
        GenerateMarZoneLegendPNG(iZones,sImagePath)
        
        updateSliderInput(session, "n", , value = iZones, max = iZones)
    }

    fLeafletRdata <<- FALSE
    sLeafletRdata <- paste0(sMarxanDir,"/pulayer/leaflet.Rdata")
    if (file.exists(sLeafletRdata))
    {
        load(file=sLeafletRdata)
        proj10 <<- proj10
        fLeafletRdata <<- TRUE
    }
}

#' @export
PrepareDisplay <- function(sCallingApp)
{
    cat("PrepareDisplay start\n")

    # prepare the map: pulayer object
    pulayer <<- pulayer_
    pu_table <<- read.dbf(paste(sMarxanDir,"/pulayer/pulayer.dbf",sep=""))

    if ((sCallingApp == "marxan") | (sCallingApp == "mxptest"))
    {
        # prepare the planning unit status object
        pustatus <<- pustatus_
        # make status work ok
        # join pu.dat and pulayer with PUID field (to order them and handle missing rows)
        prepare_pu_status()
        fExistingReserves <<- (2 %in% pustatus_)
        fExcluded <<- (3 %in% pustatus_)

        if (sCallingApp == "marxan")
        {
            # load the cluster analysis objects from file
            load(file=paste0(sMarxanDir,"/output/cluster.Rdata"))
            sol.mds <<- sol.mds
            sol3d.mds <<- sol3d.mds
            nmdscolours <<- nmdscolours
            plotlabels <<- plotlabels
            d <<- d
        }
    }

    if (sCallingApp == "marzone")
    {
        if (file.exists(paste0(sMarxanDir,"/output/cluster.Rdata")))
        {
            # load the cluster analysis objects from file
            load(file=paste0(sMarxanDir,"/output/cluster.Rdata"))
            sol.mds <<- sol.mds
            sol3d.mds <<- sol3d.mds
            nmdscolours <<- nmdscolours
            plotlabels <<- plotlabels
            d <<- d
        }
    }

    cat("PrepareDisplay end\n")
}

#' @export
GenerateSolnFilename <- function(iRunNumber,sMarxanDir)
{
    sFilename <- paste0(sMarxanDir,"/output/output_r")
    iPad <- 5 - nchar(as.character(iRunNumber))
    sFilename <- paste0(sFilename,paste0(rep("0",iPad),collapse=""))
    sFilename <- paste0(sFilename,iRunNumber,GetOutputFileext(sMarxanDir,"SAVERUN"))
}

#' @export
map_png <- function(colours,sPngFile,width=600)
{
    cat(paste0(sPngFile,"\n"))

    # plot to png file
    iWidth <- width + 57 + 28
    iHeight <- round(iWidth/iAspectX*iAspectY)
    # plot to png file
    png(filename = sPngFile,width = iWidth, height = iHeight)
    plotPolys(pulayer,col=colours,axes=FALSE,border=NA,cex.lab=0.1,cex.axis=0.1)
    if (!is.na(puoutline))
    {
        addLines(puoutline,col="black")
    }
    dev.off()
    # clip whitespace
    apng <- readPNG(sPngFile)
    apng <- apng[58:(iHeight - 71),58:(iWidth - 28),]
    writePNG(apng,target=sPngFile)
}

#' @export
map_pt_ssolnNmap <- function(tempputable)
{
    sFilename <- paste0(sMarxanDir,"/output/output",swhichparam,iwhichmap,"_ssoln.csv")
    solution_table <- read.csv(sFilename)
    colnames(solution_table)[1] <- "PUID"
    colnames(solution_table)[2] <- "SSOLN2"
    solution_table$SSOLN2 <- as.integer(solution_table$SSOLN2)
    values_ <- sqldf("SELECT * from tempputable LEFT JOIN solution_table USING(PUID)")
    values_ <- sqldf("SELECT SSOLN2 from values_") # + 1
    
    # make NA values 0
    for (i in 1:nrow(values_))
    {
        if (is.na(values_[i,]))
        {
            values_[i,] <- 0
        }
    }
    
    blueramp <- colorRampPalette(c("white","blue"))(5)
    colours <- rep(blueramp[1],nrow(values_))
    for (j in 1:nrow(values_))
    {
        if (pustatus[j] == 2)
        {
            colours[j] <- "#40E0D0" # Turquoise
        } else {
            if (pustatus[j] == 3)
            {
                colours[j] <- "grey"
            } else {
                if (values_[j,] < 1)
                {
                    colours[j] <- "white"
                } else {
                    if (values_[j,] < 3)
                    {
                        colours[j] <- blueramp[2]
                    } else {
                        if (values_[j,] < 7)
                        {
                            colours[j] <- blueramp[3] 
                        } else {
                            if (values_[j,] < 10)
                            {
                                colours[j] <- blueramp[4]
                            } else {
                                colours[j] <- blueramp[5]
                            }
                        }
                    }
                }
            }
        }
    }
    #map_png(colours,paste0(sMarxanDir,"/output/output_pt_map.png"),600)
    plotPolys(pulayer,col=colours,axes=FALSE,border=NA,cex.lab=0.1,cex.axis=0.1)
}

#' @export
map_pt <- function()
{
    tempputable <- sqldf("SELECT PUID from pu_table")
    colnames(tempputable)[1] <- "PUID"
    
    if (swhichrun == "ssoln")
    {
        cat("ssoln\n")
        
        map_pt_ssolnNmap(tempputable)
    } else {
        if (swhichrun == "best")
        {
            cat("best\n")
            sFilename <- paste0(sMarxanDir,"/output/output",swhichparam,iwhichmap,"_best.csv")
        } else {
            cat("solution M\n")
            sFilename <- paste0(sMarxanDir,"/output/output",swhichparam,iwhichmap,"_r",PadInt(as.integer(swhichrun)),".csv")
        }
        solution_table <- read.csv(sFilename)
        
        values_ <- sqldf("SELECT * from tempputable LEFT JOIN solution_table USING(PUID)")
        # plot the map
        values_ <- as.integer(unlist(sqldf("SELECT SOLUTION from values_") + 1))
        colourpalette <- c("white","blue")
        colours <- rep("white",each=length(values_))
        for (j in 1:length(values_))
        {
            if (pustatus[j] == 2)
            {
                colours[j] <- "#40E0D0" # Turquoise
            } else {
                if (pustatus[j] == 3)
                {
                    colours[j] <- "grey"
                } else {
                    colours[j] <- colourpalette[values_[j]]
                }
            }
        }
        
        #map_png(colours,paste0(sMarxanDir,"/output/output_pt_map.png"),600)
        plotPolys(pulayer,col=colours,axes=FALSE,border=NA,cex.lab=0.1,cex.axis=0.1)
    }
}

#' @export
map_mz_ssolnNmap_leaflet <- function(iN)
{
    # output_ssoln.csv
    # "planning unit","number","available","reserved"
    ssolntable <- read.csv(paste0(sMarxanDir,"/output/output_ssoln.csv"))
    colnames(ssolntable)[1] <- "PUID"
    
    cat(paste0("map_mz_ssolnNmap_leaflet iN ",iN," ZoneNames[iN] ",ZoneNames[iN],"\n"))

    # rectify values with order of planning units in the pulayer
    pu_id <- sqldf("SELECT PUID from pu_table")
    ssoln_sorted <- sqldf("SELECT * from pu_id LEFT JOIN ssolntable USING(PUID)")
    values <- sqldf(paste0("SELECT ",ZoneNames[iN]," from ssoln_sorted"))
    # mark pu's in pulayer that are missing from ssolntable as zone 0 (white)
    for (i in 1:nrow(values)) { if (is.na(values[i,])) { values[i,] <- 0 } }
    
    blueramp <- colorRampPalette(c("white","blue"))(5)
    colours <- rep(blueramp[1],nrow(values))
    display_polygon <- rep(TRUE,nrow(values))
    for (j in 1:nrow(values))
    {
        if (values[j,] == 0)
        {
            colours[j] <- "white"
            display_polygon[j] <- FALSE
        } else {
            if (values[j,] < 30)
            {
                colours[j] <- blueramp[2]
            } else {
                if (values[j,] < 70)
                {
                    colours[j] <- blueramp[3] 
                } else {
                    if (values[j,] < 100)
                    {
                        colours[j] <- blueramp[4]
                    } else {
                        colours[j] <- blueramp[5]
                    }
                }
            }
        }
    }

    row.names(proj10) <- as.character(rep(1:length(row.names(proj10))))
    PUID <- as.integer(sapply(slot(proj10, "polygons"), function(x) slot(x, "ID")))
    PUID <- as.data.frame(cbind(PUID,display_polygon))
    colnames(PUID)[2] <- "dpoly"
    leaflet_proj10 <- SpatialPolygonsDataFrame(proj10,data=PUID)
    leaflet_proj10 <<- leaflet_proj10[leaflet_proj10@data$dpoly == TRUE,]
    leaflet_colours <<- subset(colours,display_polygon)
}

#' @export
map_mz_ssolnNmap <- function(iN)
{
    # output_ssoln.csv
    # "planning unit","number","available","reserved"
    ssolntable <- read.csv(paste0(sMarxanDir,"/output/output_ssoln.csv"))
    colnames(ssolntable)[1] <- "PUID"
    # change the zone names in case they are using SQL reserved word as zone name
    #for (i in 1:iZones)
    #{
    #    colnames(ssolntable)[i+2] <- paste0("zone",i)
    #}
    
    #values <- prepare_M_values_mz(soln)

    # rectify values with order of planning units in the pulayer
    pu_id <- sqldf("SELECT PUID from pu_table")
    ssoln_sorted <- sqldf("SELECT * from pu_id LEFT JOIN ssolntable USING(PUID)")
    values <- sqldf(paste0("SELECT ",ZoneNames[iN]," from ssoln_sorted"))
    # mark pu's in pulayer that are missing from ssolntable as zone 0 (white)
    for (i in 1:nrow(values))
    {
        if (is.na(values[i,]))
        {
            values[i,] <- 0
        }
    }
    
    blueramp <- colorRampPalette(c("white","blue"))(5)
    colours <- rep(blueramp[1],nrow(values))
    for (j in 1:nrow(values))
    {
        if (values[j,] == 0)
        {
            colours[j] <- "white"
        } else {
            if (values[j,] < 30)
            {
                colours[j] <- blueramp[2]
            } else {
                if (values[j,] < 70)
                {
                    colours[j] <- blueramp[3] 
                } else {
                    if (values[j,] < 100)
                    {
                        colours[j] <- blueramp[4]
                    } else {
                        colours[j] <- blueramp[5]
                    }
                }
            }
        }
    }
    #map_png(colours,paste0(sMarxanDir,"/output/output_map.png"),600)
    plotPolys(pulayer,col=colours,axes=FALSE,border=NA,cex.lab=0.1,cex.axis=0.1)
}

#' @export
map_mz_bestmap_leaflet <- function()
{
    # output_sum.csv
    # "Run Number","Score","Cost","Planning Units",available PuCount,reserved PuCount,available Cost,reserved Cost,"Connection Strength","Penalty","Shortfall","Missing_Values","MPM"
    sumtable <- read.csv(paste0(sMarxanDir,"/output/output_sum.csv"))
    iBest <- which(sumtable$Score==min(sumtable$Score))
    if (length(iBest) > 1)
    {
        iBest <- iBest[1]
    }
    # output_r00001.csv
    # planning_unit,zone
    map_mz_runMmap_leaflet(iBest)
}

#' @export
map_mz_bestmap <- function()
{
    # output_sum.csv
    # "Run Number","Score","Cost","Planning Units",available PuCount,reserved PuCount,available Cost,reserved Cost,"Connection Strength","Penalty","Shortfall","Missing_Values","MPM"
    sumtable <- read.csv(paste0(sMarxanDir,"/output/output_sum.csv"))
    iBest <- which(sumtable$Score==min(sumtable$Score))
    if (length(iBest) > 1)
    {
        iBest <- iBest[1]
    }
    # output_r00001.csv
    # planning_unit,zone
    map_mz_runMmap(iBest)
}

#' @export
map_mz_runMmap_leaflet <- function(iM)
{
    # output_r00001.csv
    # planning_unit,zone
    soln <- read.csv(paste0(sMarxanDir,"/output/output_r",PadInt(iM),".csv"))
    colnames(soln)[1] <- "PUID"

    cat(paste0("map_mz_runMmap_leaflet iZones ",iZones,"\n"))

    # rectify values with order of planning units in the pulayer
    pu_id <- sqldf("SELECT PUID from pu_table")
    soln_sorted <- sqldf("SELECT * from pu_id LEFT JOIN soln USING(PUID)")
    values <- sqldf(paste0("SELECT zone from soln_sorted"))
    # mark pu's in pulayer that are missing from ssolntable as zone 0 (white)
    for (i in 1:nrow(values)) { if (is.na(values[i,])) { values[i,] <- 0 } }

    # use a rainbow colour palette
    rainbowramp <- col2hex(rainbow(iZones))

    colours <- rep("white",nrow(values))
    display_polygon <- rep(TRUE,nrow(values))
    for (j in 1:nrow(values))
    {
        if (values[j,] == 0)
        {
            colours[j] <- "white" # mark pu's in pulayer that are missing from soln as zone 0 (white)
            display_polygon[j] <- FALSE
        } else {
            colours[j] <- rainbowramp[values[j,]]
        }
    }

    row.names(proj10) <- as.character(rep(1:length(row.names(proj10))))
    PUID <- as.integer(sapply(slot(proj10, "polygons"), function(x) slot(x, "ID")))
    PUID <- as.data.frame(cbind(PUID,display_polygon))
    colnames(PUID)[2] <- "dpoly"
    leaflet_proj10 <- SpatialPolygonsDataFrame(proj10,data=PUID)
    leaflet_proj10 <<- leaflet_proj10[leaflet_proj10@data$dpoly == TRUE,]
    leaflet_colours <<- subset(colours,display_polygon)
}

#' @export
map_mz_runMmap <- function(iM)
{
    # output_r00001.csv
    # planning_unit,zone
    soln <- read.csv(paste0(sMarxanDir,"/output/output_r",PadInt(iM),".csv"))
    colnames(soln)[1] <- "PUID"

    values <- prepare_M_values_mz(soln)

    # use a rainbow colour palette
    rainbowramp <- rainbow(iZones)
    colours <- rep("white",nrow(values))
    for (j in 1:nrow(values))
    {
        if (values[j,] == 0)
        {
            colours[j] <- "white" # mark pu's in pulayer that are missing from soln as zone 0 (white)
        } else {
            colours[j] <- rainbowramp[values[j,]]
        }
    }
    #map_png(colours,paste0(sMarxanDir,"/output/output_map.png"),600)
    plotPolys(pulayer,col=colours,axes=FALSE,border=NA,cex.lab=0.1,cex.axis=0.1)
}

#' @export
map_ssolnNmap_leaflet <- function()
{
    values <- sqldf(paste("SELECT SSOLN2 from pu_table",sep=""))
    
    # make NA values 0
    for (i in 1:nrow(values))
    {
        if (is.na(values[i,]))
        {
            values[i,] <- 0
        }
    }

    blueramp <- colorRampPalette(c("white","blue"))(5)
    colours <- rep(blueramp[1],nrow(values))
    display_polygon <- rep(TRUE,nrow(values))
    for (j in 1:nrow(values))
    {
        if (pustatus[j] == 2)
        {
            colours[j] <- "#40E0D0" # Turquoise
        } else {
            if (pustatus[j] == 3)
            {
                colours[j] <- "grey"
            } else {
                if (values[j,] == 0)
                {
                    colours[j] <- "white"
                    display_polygon[j] <- FALSE
                } else {
                    if (values[j,] < 30)
                    {
                        colours[j] <- blueramp[2]
                    } else {
                        if (values[j,] < 70)
                        {
                            colours[j] <- blueramp[3] 
                        } else {
                            if (values[j,] < 100)
                            {
                                colours[j] <- blueramp[4]
                            } else {
                                colours[j] <- blueramp[5]
                            }
                        }
                    }
                }
            }
        }
    }

    row.names(proj10) <- as.character(rep(1:length(row.names(proj10))))
    PUID <- as.integer(sapply(slot(proj10, "polygons"), function(x) slot(x, "ID")))
    PUID <- as.data.frame(cbind(PUID,display_polygon))
    colnames(PUID)[2] <- "dpoly"
    leaflet_proj10 <- SpatialPolygonsDataFrame(proj10,data=PUID)
    leaflet_proj10 <<- leaflet_proj10[leaflet_proj10@data$dpoly == TRUE,]
    leaflet_colours <<- subset(colours,display_polygon)
}

#' @export
map_ssolnNmap <- function()
{
    values <- sqldf(paste("SELECT SSOLN2 from pu_table",sep=""))
    
    # make NA values 0
    for (i in 1:nrow(values))
    {
        if (is.na(values[i,]))
        {
            values[i,] <- 0
        }
    }

    blueramp <- colorRampPalette(c("white","blue"))(5)
    colours <- rep(blueramp[1],nrow(values))
    for (j in 1:nrow(values))
    {
        if (pustatus[j] == 2)
        {
            colours[j] <- "#40E0D0" # Turquoise
        } else {
            if (pustatus[j] == 3)
            {
                colours[j] <- "grey"
            } else {
                if (values[j,] == 0)
                {
                    colours[j] <- "white"
                } else {
                    if (values[j,] < 30)
                    {
                        colours[j] <- blueramp[2]
                    } else {
                        if (values[j,] < 70)
                        {
                            colours[j] <- blueramp[3] 
                        } else {
                            if (values[j,] < 100)
                            {
                                colours[j] <- blueramp[4]
                            } else {
                                colours[j] <- blueramp[5]
                            }
                        }
                    }
                }
            }
        }
    }
    #map_png(colours,paste0(sMarxanDir,"/output/output_map.png"),600)
    plotPolys(pulayer,col=colours,axes=FALSE,border=NA,cex.lab=0.1,cex.axis=0.1)
}

#' @export
prepare_pu_status <- function()
{
    # prepare pustatus
    # join pu.dat and pulayer with PUID field (to order them and handle missing rows)
    pudat <- read.csv(paste0(sMarxanDir,"/input/pu.dat"),stringsAsFactors=FALSE)
    colnames(pudat)[1] <- "PUID"
    pu_id <- sqldf("SELECT PUID from pu_table")
    pustatus_sorted <- sqldf("SELECT * from pu_id LEFT JOIN pudat USING(PUID)")
    pustatus <- unlist(pustatus_sorted$status)
    # mark pu's in pulayer that are missing from pu.dat as status 0
    for (i in 1:length(pustatus))
    {
        if (is.na(pustatus[i]))
        {
            pustatus[i] <- 0
        }
    }
    pustatus <<- pustatus
}

#' @export
binary_map_leaflet <- function(values)
{
    greenramp <- colorRampPalette(c("white","blue"))(2)
    colours <- rep(greenramp[1],nrow(values))
    display_polygon <- rep(TRUE,nrow(values))
    for (j in 1:nrow(values))
    {
        if (pustatus[j] == 2)
        {
            colours[j] <- "#40E0D0" # Turquoise
            #display_polygon[j] <- TRUE
        } else {
            if (pustatus[j] == 3)
            {
                colours[j] <- "grey"
                #display_polygon[j] <- TRUE
            } else {
                colours[j] <- greenramp[values[j,]]
                if (values[j,] == 1)
                {
                    display_polygon[j] <- FALSE
                }
            }
        }
    }

    row.names(proj10) <- as.character(rep(1:length(row.names(proj10))))
    PUID <- as.integer(sapply(slot(proj10, "polygons"), function(x) slot(x, "ID")))
    PUID <- as.data.frame(cbind(PUID,display_polygon))
    colnames(PUID)[2] <- "dpoly"
    leaflet_proj10 <- SpatialPolygonsDataFrame(proj10,data=PUID)
    leaflet_proj10 <<- leaflet_proj10[leaflet_proj10@data$dpoly == TRUE,]
    leaflet_colours <<- subset(colours,display_polygon)
}

#' @export
binary_map <- function(values)
{
    greenramp <- colorRampPalette(c("white","blue"))(2)
    colours <- rep(greenramp[1],nrow(values))
    for (j in 1:nrow(values))
    {
        if (pustatus[j] == 2)
        {
            colours[j] <- "#40E0D0" # Turquoise
        } else {
            if (pustatus[j] == 3)
            {
                colours[j] <- "grey"
            } else {
                colours[j] <- greenramp[values[j,]]
            }
        }
    }
    #map_png(colours,paste0(sMarxanDir,"/output/output_map.png"),600)
    plotPolys(pulayer,col=colours,axes=FALSE,border=NA,cex.lab=0.1,cex.axis=0.1)
}

#' @export
map_bestmap_leaflet <- function()
{
    values <- sqldf("SELECT BESTSOLN from pu_table")
    binary_map_leaflet(values)
}

#' @export
map_bestmap <- function()
{
    values <- sqldf("SELECT BESTSOLN from pu_table")
    binary_map(values)
}

#' @export
prepare_M_values_mz <- function(solnX_table)
{
    # prepare pustatus
    # join solnX_table and pulayer with PUID field (to order them and handle missing rows)
    pu_id <- sqldf("SELECT PUID from pu_table")
    colnames(solnX_table)[1] <- "PUID"
    values_sorted <- sqldf("SELECT * from pu_id LEFT JOIN solnX_table USING(PUID)")
    values <- sqldf("SELECT zone from values_sorted")
    # mark pu's in pulayer that are missing from values as 0
    for (i in 1:nrow(values))
    {
        if (is.na(values[i,]))
        {
            values[i,] <- 0
        }
    }
    return(values)
}

#' @export
prepare_M_values <- function(solnX_table)
{
    # prepare pustatus
    # join solnX_table and pulayer with PUID field (to order them and handle missing rows)
    pu_id <- sqldf("SELECT PUID from pu_table")
    values_sorted <- sqldf("SELECT * from pu_id LEFT JOIN solnX_table USING(PUID)")
    values <- sqldf("SELECT SOLUTION from values_sorted")
    # mark pu's in pulayer that are missing from values as "Available"
    for (i in 1:nrow(values))
    {
        if (is.na(values[i,]))
        {
            values[i,] <- 0
        }
    }
    return(values + 1)
}

#' @export
map_runMmap <- function()
{
    solnX_table <- read.csv(GenerateSolnFilename(iM,sMarxanDir))
    #values <- sqldf(paste("SELECT SOLUTION from solnX_table",sep="")) + 1
    
    # "fix" the values to match order of putable
    prepare_pu_status()
    values <- prepare_M_values(solnX_table)
    
    binary_map(values)
}

#' @export
map_runMmap_leaflet <- function()
{
    solnX_table <- read.csv(GenerateSolnFilename(iM,sMarxanDir))
    #values <- sqldf(paste("SELECT SOLUTION from solnX_table",sep="")) + 1
    
    # "fix" the values to match order of putable
    prepare_pu_status()
    values <- prepare_M_values(solnX_table)
    
    binary_map_leaflet(values)
}

#' @export
auto_compute_marzone_cluster <- function()
{
    if (!file.exists(paste0(sMarxanDir,"/output/cluster.Rdata")))
    {
        PrepareCluster_compute_MarZone(sMarxanDir)

        withProgress(message="Load cluster.Rdata",value=0,
        {
            # load the cluster analysis objects from file

            load(file=paste0(sMarxanDir,"/output/cluster.Rdata"))
            sol.mds <<- sol.mds
            sol3d.mds <<- sol3d.mds
            nmdscolours <<- nmdscolours
            plotlabels <<- plotlabels
            d <<- d
        })
    }
}

#' @export
cluster_2ds <- function(sCallingApp,fDisplayText)
{
    if (sCallingApp == "marzone")
    {
        auto_compute_marzone_cluster()
    }
    if (is.na(sol.mds))
    {
        plot(1,1)
    }
    else
    {
        withProgress(message="Plot cluster",value=0,
        {
            plot(sol.mds$points, xlab='', ylab='', main='NMDS of solutions', col=nmdscolours)
            if (fDisplayText)
            {
                text(sol.mds$points,labels=plotlabels,pos=4, col=nmdscolours)
            }
        })
    }
}
 
#' @export
cluster_3ds <- function(sCallingApp)
{
    if (sCallingApp == "marzone")
    {
        auto_compute_marzone_cluster()
    }
    if (is.na(sol3d.mds))
    {
        plot(1,1)
    }
    else
    {
        withProgress(message="Plot cluster",value=0,
        {
            plot3d(sol3d.mds$points, xlab="",ylab="",zlab="", col=nmdscolours)
        })
    }
}

#' @export
cluster_dendogram <- function(sCallingApp)
{
    if (sCallingApp == "marzone")
    {
        auto_compute_marzone_cluster()
    }
    if (is.na(d))
    {
        plot(1,1)
    }
    else
    {
        withProgress(message="Plot cluster",value=0,
        {
            plot(d, xlab="Solutions", ylab="Disimilarity", main="Bray-Curtis dissimilarity of solutions")
        })
    }
}

#' @export
ExecuteMarxan <- function(sMarxanDir,sExecutable,iCores,iRepsPerCore)
{
    # read input.dat
    #inputdat <- readLines(paste0(sMarxanDir,"/input.dat"))
    randomseeds <- round(runif(10)*100000)

    #if (fWindows) { registerDoParallel(makeCluster(iCores,type="PSOCK")) }
    registerDoParallel(makeCluster(iCores,type="PSOCK"))

    # need to export objects not in local environment
    export_list <- c('fWindows','sShinyDataPath')

    # run Marxan
    foreach(i=1:iCores,.export=export_list) %dopar%
    {
        #system2("touch",paste0(sMarxanDir,"/core",i,"/a"),wait=T)

        dir.create(paste0(sMarxanDir,"/core",i))
        file.copy(paste0(sShinyDataPath,"/",sExecutable),paste0(sMarxanDir,"/core",i,"/",sExecutable))
        if (!fWindows) { system(paste0("chmod +x ",sMarxanDir,"/core",i,"/",sExecutable)) }

        #system2("touch",paste0(sMarxanDir,"/core",i,"/b"),wait=T)

        # set parameters for multi core
        inputdat <- readLines(paste0(sMarxanDir,"/input.dat"))
        iINPUTDIRparam <- which(regexpr("INPUTDIR",inputdat)==1)
        iOUTPUTDIRparam <- which(regexpr("OUTPUTDIR",inputdat)==1)
        iSCENNAMEparam <- which(regexpr("SCENNAME",inputdat)==1)
        iNUMREPSparam <- which(regexpr("NUMREPS",inputdat)==1)
        iRANDSEEDparam <- which(regexpr("RANDSEED",inputdat)==1)
        inputdat[iINPUTDIRparam] <- paste0("INPUTDIR ",sMarxanDir,"/input")
        inputdat[iOUTPUTDIRparam] <- paste0("OUTPUTDIR ",sMarxanDir,"/output")
        inputdat[iSCENNAMEparam] <- paste0("SCENNAME output",i)
        inputdat[iNUMREPSparam] <- paste0("NUMREPS ",iRepsPerCore)
        inputdat[iRANDSEEDparam] <- paste0("RANDSEED ",randomseeds[i])

        #system2("touch",paste0(sMarxanDir,"/core",i,"/c"),wait=T)

        writeLines(inputdat,paste0(sMarxanDir,"/core",i,"/input.dat"))

        #system2("touch",paste0(sMarxanDir,"/core",i,"/d"),wait=T)

        setwd(paste0(sMarxanDir,"/core",i))

        #system2("touch",paste0(sMarxanDir,"/core",i,"/e"),wait=T)

        if (fWindows)
        {
            system2(sExecutable,"-s",wait=T)
        } else {
            system(paste0("./",sExecutable," -s"))
        }
        #system2(sExecutable,"-s",wait=T)

        #system2("touch",paste0(sMarxanDir,"/core",i,"/f"),wait=T)

    }

    #if (fWindows) { registerDoSEQ() }
    registerDoSEQ()

    for (i in 1:iCores)
    {
      file.remove(paste0(sMarxanDir,"/core",i,"/input.dat"))
    }
}

#' @export
RunMarZone <- function(sMarxanDir,sShinyDataPath,iCores,iRepsPerCore)
{
    withProgress(message="Run MarZone",value=0,
    {
        withProgress(message="MarZone",value=0,
        {
            ExecuteMarxan(sMarxanDir,sExecutable,iCores,iRepsPerCore)
        })

        withProgress(message="Merge results",value=0,
        {
            JoinParallelResults_MarZone(sMarxanDir,iCores,iRepsPerCore,iZones)
        })

        # invalidate cluster object by removing it
        file.remove(paste0(sMarxanDir,"/output/cluster.Rdata"))
    })
}

#' @export
RunMarZone_app <- function()
{
    RunMarZone(sMarxanDir,sShinyDataPath,iCores,iRepsPerCore)
    
    withProgress(message="Run MarZone",value=0,
    {
        withProgress(message="Prepare display",value=0,
        {
            PrepareDisplay("marzone")
        })
    })
}

#' @export
RunMarxan <- function(sMarxanDir,sShinyDataPath,iCores,iRepsPerCore)
{
    withProgress(message="Run Marxan",value=0,
    {
        withProgress(message="Marxan",value=0,
        {
            ExecuteMarxan(sMarxanDir,sExecutable,iCores,iRepsPerCore)
        })
        
        withProgress(message="Merge results",value=0,
        {
            JoinParallelResults(sMarxanDir,iCores,iRepsPerCore)
        })
        
        withProgress(message="Populate dbf",value=0,
        {
            ImportOutputsCsvToShpDbf(paste0(sMarxanDir,"/pulayer/pulayer.dbf"),sMarxanDir,round(iCores*iRepsPerCore),"PUID")
        })
        
        withProgress(message="Prepare cluster",value=0,
        {
            PrepareCluster_compute(sMarxanDir)
        })
    })
}

#' @export
RunMarxan_app <- function()
{
    # save BLM parameter
    inputdat <- readLines(paste0(sMarxanDir,"/input.dat"))
    iBLMparam <- which(regexpr("BLM",inputdat)==1)
    inputdat[iBLMparam] <- paste0("BLM ",rblm)
    writeLines(inputdat,paste0(sMarxanDir,"/input.dat"))
    
    RunMarxan(sMarxanDir,sShinyDataPath,iCores,iRepsPerCore)

    withProgress(message="Run Marxan",value=0,
    {
        withProgress(message="Prepare display",value=0,
        {
            PrepareDisplay("marxan")
        })
    })
}

#' @export
CreateLogFile <- function(sPath,sID,sCallingApp)
{
    Gfold <- sprintf("%s",round(runif(1)*1000000))
    for (ii in 1:100000)
    {
        sFile <- sprintf("%s/%s_%s_%s.log",sPath,sCallingApp,sID,Gfold)
        if(!file.exists(sFile))
        {
            write(paste0(date()," start log ",sID),file=sFile)
            break()
        }
    }
    return(sFile)
}

#' @export
AppendLogFile <- function(sLogFileName,sMessage)
{
    write(paste0(date()," ",sMessage),file=sLogFileName,append=TRUE)
}

#' @export
AuthenticateUserSession <- function(sessionkey)
{
    sUserSessionKey <<- sessionkey
    
    # get the user session file
    sSessionKeyFile <- paste0(sShinyPath,"sessions/",sessionkey,".Rdata")
    if (file.exists(sSessionKeyFile))
    {
        load(file=sSessionKeyFile)
        sText <- paste0("sessionkey: ", sUserSessionKey,"\n",
                        "sessionkeyfile: ", sSessionKeyFile,"\n",
                        "username: ",sSessionUserName,"\n",
                        "logindate: ", SessionLoginDate, "\n",
                        "userip: ", sSessionUserIP)
        cat(paste0(sText,"\n"))
        # verify session details
        # does ip match ip in session file ?
        #sUserIP <- as.character(input$ipid)
        if (fSessionValid) #(sUserIP == sSessionUserIP)
        {
            # is session < 12 hours old ?
            rDiffTime <- as(difftime(Sys.time(),SessionLoginDate,units="hours"),"numeric")
            if (rDiffTime < 12)
            {
                cat(paste0("session is ",rDiffTime," hours old\n"))
                cat("user is authenticated\n")
                sUserName <<- sSessionUserName
                return(TRUE)
            } else {
                cat("session has expired\n")
                return(FALSE)
            }
        } else {
            #cat(paste0("IP address mismatch user:>",sUserIP,"< session:>",sSessionUserIP,"<\n"))
            cat(paste0("User has logged out user:>",sUserIP,"< session:>",sSessionUserIP,"<\n"))
            return(FALSE)
        }
    } else {
        cat(paste0("session file >",sSessionKeyFile,"< does not exist\n"))
        return(FALSE)
    }
}

#' @export
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

#' @export
marxanui_init_user <- function()
{
    # does the users home directory exist?
    sHOME <- Sys.getenv("HOME")

    sUserHome <- paste0(sHOME,"/marxanui")
    if (!file.exists(sUserHome))
    {
        # create user home and populate it
        cat("create user home\n")
        dir.create(sUserHome)
        sMxHome <- paste0(sUserHome,"/marxan")
        sMzHome <- paste0(sUserHome,"/marzone")
        sDataDir <- paste0(sUserHome,"/data")
        dir.create(sMxHome)
        dir.create(sMzHome)
        dir.create(sDataDir)
        dir.create(paste0(sUserHome,"/temp"))
        dir.create(paste0(sUserHome,"/log"))

        # fetch data zip from the web
        # the zip file contains executable files and sample datasets for the appropriate platform
        if (fWindows)
        {
            sDownloadFile <- "windows_data.zip"
        } else {
            sDownloadFile <- "unix_data.zip"
        }
        sURL <- paste0("http://marxan.net/downloads/",sDownloadFile)
        sDestFile <- paste0(sUserHome,"/data.zip")
        download.file(sURL,sDestFile,mode="wb",cacheOK=F)

        # unzip the data zip file
        unzip(sDestFile,overwrite=T,junkpaths=T,exdir=sDataDir)

        # unzip the sample datasets
        sMxSample <- paste0(sDataDir,"/Tasmania.zip")
        sMzSample <- paste0(sDataDir,"/RottnestIsland_Scenario4.zip")
        unzip(sMxSample,overwrite=T,exdir=sMxHome)
        unzip(sMzSample,overwrite=T,exdir=sMzHome)

        # remove the zip files we no longer need
        file.remove(sDestFile)
        file.remove(sMxSample)
        file.remove(sMzSample)

        # remove the executable files we don't need
        if (fWindows)
        {
            if (f64bit)
            {
                # system is 64 bit: remove 32 bit executables
                file.remove(paste0(sDataDir,"/Marxan.exe"))
                file.remove(paste0(sDataDir,"/MarZone.exe"))
            } else {
                # system is 32 bit: remove 64 bit executables
                file.remove(paste0(sDataDir,"/Marxan_x64.exe"))
                file.remove(paste0(sDataDir,"/MarZone_x64.exe"))
            }
        } else {
            if (fLinux)
            {
                file.remove(paste0(sDataDir,"/MarOpt_v243_Mac64"))
                file.remove(paste0(sDataDir,"/MarZone_v201_Mac64"))
                if (f64bit)
                {
                    file.remove(paste0(sDataDir,"/MarOpt_v243_Linux32"))
                    file.remove(paste0(sDataDir,"/MarZone_v201_Linux32"))
                } else {
                    file.remove(paste0(sDataDir,"/MarOpt_v243_Linux64"))
                    file.remove(paste0(sDataDir,"/MarZone_v201_Linux64"))
                }
            } else {
                file.remove(paste0(sDataDir,"/MarOpt_v243_Linux32"))
                file.remove(paste0(sDataDir,"/MarZone_v201_Linux32"))
                file.remove(paste0(sDataDir,"/MarOpt_v243_Linux64"))
                file.remove(paste0(sDataDir,"/MarZone_v201_Linux64"))
            }
        }
    }
}

#' @export
marxanui_start <- function(sCallingApp)
{
    sUserName <<- "localuser"

    cat(paste0("marxanui_start start ",sUserName,"\n"))

    detect_platform()
    # populate the users home directory if it doesn't exist
    marxanui_init_user()

    # if the users home directory doesn't exist, create it and populate it with a sample dataset
    sUserHome <<- paste0(sShinyUserPath)
    sUserTemp <- paste0(sShinyTempPath)
    
    if (sCallingApp == "import")
    {
        
    } else {
            if ((sCallingApp == "marxan") | (sCallingApp == "mxptest"))
            {
                if (fLinux)
                {
                    if (f64bit)
                    {
                        sExecutable <<- "MarOpt_v243_Linux64"
                    } else {
                        sExecutable <<- "MarOpt_v243_Linux32"
                    }
                }
                if (fMac) { sExecutable <<- "MarOpt_v243_Mac64" }
                if (fWindows)
                {
                    if (f64bit)
                    {
                        sExecutable <<- "Marxan_x64.exe"
                    } else {
                        sExecutable <<- "Marxan.exe"
                    }
                }

                sAppHome <<- paste0(sShinyUserPath,"/marxan/")
            }
            if (sCallingApp == "marzone")
            {
                if (fLinux)
                {
                    if (f64bit)
                    {
                        sExecutable <<- "MarZone_v201_Linux64"
                    } else {
                        sExecutable <<- "MarZone_v201_Linux32"
                    }
                }
                if (fMac) { sExecutable <<- "MarZone_v201_Mac64" }
                if (fWindows)
                {
                    if (f64bit)
                    {
                        sExecutable <<- "MarZone_x64.exe"
                    } else {
                        sExecutable <<- "MarZone.exe"
                    }
                }

                sAppHome <<- paste0(sShinyUserPath,"/marzone/")
            }
            if (sCallingApp == "manage")
            {
                sMarxanHome <<- paste0(sShinyUserPath,"/marxan/")
                sMarZoneHome <<- paste0(sShinyUserPath,"/marzone/")
                sAppHome <<- paste0(sShinyUserPath)
            }
    
            # restore the database name
            sRestoreFile <- paste0(sAppHome,"/database_",sCallingApp,".txt")
            if (file.exists(sRestoreFile))
            {
                sRestoreDb <- readLines(sRestoreFile)
                # is the restored database name in our list of databases?
                if (length(grep(sRestoreDb,c(list.dirs(sAppHome)))) > 0)
                {
                    sSelectDb <<- sRestoreDb
                }
            }

            # remember creation data for most recent database import
            ImportTime <<- max(file.info(c(list.dirs(sAppHome,full.names = TRUE)))$ctime)
    }

    # initialise the user session log file
    sLogFile <<- CreateLogFile(paste0(sShinyLogPath),sUserName,sCallingApp)

    cat("marxanui_start end\n")
}

#' @export
AuthenticateUser <- function(sCallingApp)
{
    cat(paste0("AuthenticateUser start ",sUserName,"\n"))

    # if the users home directory doesn't exist, create it and populate it with a sample dataset
    sUserHome <<- paste0(sShinyUserPath,sUserName)
    sUserTemp <- paste0(sShinyTempPath,sUserName)
    
    if (sCallingApp == "upload")
    {
        
    } else {
        if (sCallingApp == "reset")
        {
            # erase the session key so it can't be used for Marxan sessions, only password reset
            sSessionKeyFile <- paste0(sShinyPath,"sessions/",sUserSessionKey,".Rdata")
            dir.create(paste0(sShinyUserPath,sUserName))
            #file.remove(sSessionKeyFile)

        } else {    
            if ((sCallingApp == "marxan") | (sCallingApp == "mxptest"))
            {
                if (fLinux)
                {
                    sExecutable <<- "MarOpt_v243_Linux64"
                } else {
                    sExecutable <<- "MarOpt_v243_Mac64"
                }

                sAppHome <<- paste0(sShinyUserPath,sUserName,"/marxan/")
            }
            if (sCallingApp == "marzone")
            {
                if (fLinux)
                {
                    sExecutable <<- "MarZone_v201_Linux64"
                } else {
                    sExecutable <<- "MarZone_v201_Mac64"
                }

                sAppHome <<- paste0(sShinyUserPath,sUserName,"/marzone/")
            }
            if (sCallingApp == "download")
            {
                sMarxanHome <<- paste0(sShinyUserPath,sUserName,"/marxan/")
                sMarZoneHome <<- paste0(sShinyUserPath,sUserName,"/marzone/")
                sAppHome <<- paste0(sShinyUserPath,sUserName,"/")
            }

            # restore the database name
            sRestoreFile <- paste0(sAppHome,"/database_",sCallingApp,".txt")
            if (file.exists(sRestoreFile))
            {
                sRestoreDb <- readLines(sRestoreFile)
                # is the restored database name in our list of databases?
                if (length(grep(sRestoreDb,c(list.dirs(sAppHome)))) > 0)
                {
                    sSelectDb <<- sRestoreDb
                }
            }

            # remember creation data for most recent database import
            ImportTime <<- max(file.info(c(list.dirs(sAppHome,full.names = TRUE)))$ctime)
        }
    }

    # initialise the user session log file
    sLogFile <<- CreateLogFile(paste0(sShinyUserPath,sUserName),sUserName,sCallingApp)

    cat("AuthenticateUser end\n")
}

#' @export
LogoutSession <- function()
{
    # create the session key file
    load(file=sUserSessionKeyFile)
    fSessionValid <- FALSE
    save(sSessionUserName,SessionLoginDate,sUserSessionKey,sSessionUserIP,fSessionValid,file=sUserSessionKeyFile)
    AppendLogFile(sLogFile,"LogoutSession")
    cat("LogoutSession\n")
    USER$Logged <<- FALSE
    iLogin <<- iLoginClick
}

#' @export
InitialiseUserSession <- function()
{
    # initialise the user session key and file
    sSessionsDir <- paste0(sShinyPath,"sessions/")
    dir.create(sSessionsDir)
    SessionLoginDate <- Sys.time()
    sSessionUserName <- sUserName
    sSessionUserIP <- sUserIP
    fSessionValid <- TRUE
    
    repeat
    ({
        sUserSessionKey <<- gen_pwd()
        sUserSessionKeyFile <<- paste0(sSessionsDir,sUserSessionKey,".Rdata")

        if (!file.exists(sUserSessionKeyFile))
        {
            # create the session key file
            save(sSessionUserName,SessionLoginDate,sUserSessionKey,sSessionUserIP,fSessionValid,file=sUserSessionKeyFile)
            sText <- paste0("sessionkey: ", sUserSessionKey,"\n",
                            "sessionkeyfile: ", sUserSessionKeyFile,"\n",
                            "username: ",sSessionUserName,"\n",
                            "logindate: ", SessionLoginDate, "\n",
                            "userip: ", sSessionUserIP)
            AppendLogFile(sLogFile,sText)
            cat(paste0(sText,"\n"))
            
            break
        }
    })
}

#' @export
InitialiseUser <- function()
{
    cat(paste0("InitialiseUser start ",sUserName,"\n"))

    # if the users home directory doesn't exist, create it and populate it with a sample dataset
    sUserHome <<- paste0(sShinyUserPath,sUserName)
    sMarxanHome <- paste0(sShinyUserPath,sUserName,"/marxan/")
    sMarZoneHome <- paste0(sShinyUserPath,sUserName,"/marzone/")
    dir.create(sUserHome)
    if (!file.exists(sMarxanHome))
    {
        dir.create(sMarxanHome)
        system(paste0("unzip ",sShinyDataPath,"/",sSampleMarxanDataset,".zip -d ",sMarxanHome))
    }
    if (!file.exists(sMarZoneHome))
    {
        dir.create(sMarZoneHome)
        system(paste0("unzip ",sShinyDataPath,"/",sSampleMarZoneDataset,".zip -d ",sMarZoneHome))
    }

    # create the users apps
    sUserApps <- paste0(sShinyPath,"apps/",sUserName,"/")
    dir.create(sUserApps)
    sUserAllApps <- paste0(sUserApps,sAllApps,"/")
    dir.create(sUserAllApps)
    sCpCmd <- paste0("cp -rf ",sShinyPath,"apps/",sAllApps,"/* ",sUserAllApps)
    cat(paste0(sCpCmd,"\n"))
    system(sCpCmd)

    # if the users temp directory doesn't exist, create it
    sUserTemp <- paste0(sShinyTempPath,sUserName)
    if (!file.exists(sUserTemp))
    {
        dir.create(sUserTemp)
    }

    # initialise the user session log file
    sLogFile <<- CreateLogFile(paste0(sShinyUserPath,sUserName),sUserName,"login")

    cat("InitialiseUser end\n")
}

#' @export
ChangeDatabase <- function(sCallingApp,session)
{
    cat("ChangeDatabase start\n")

    if (sCallingApp == "marxan")
    {
        # do we need to do RunMarxan 1st ?
        if (!file.exists(paste0(sMarxanDir,"/output/output_sum.csv")))
        {
            RunMarxan(sMarxanDir,sShinyDataPath,iCores,iRepsPerCore)
        }
    }
    
    if ((sCallingApp == "marzone") | (sCallingApp == "marzone_leaflet"))
    {
        # do we need to do RunMarZone 1st ?
        if (!file.exists(paste0(sMarxanDir,"/output/output_sum.csv")))
        {
            # set iZones in ChangeDatabase before calling JoinParallelResults_MarZone
            # fixes subtle MarZone import bug
            zonesdat <- read.csv(paste0(sMarxanDir,"/input/zones.dat"))
            iZones <<- nrow(zonesdat)
            ZoneNames <<- as.character(unlist(zonesdat$zonename))

            RunMarZone(sMarxanDir,sShinyDataPath,iCores,iRepsPerCore)
        }
        
        # generate a list of input files for this dataset
        cat(paste0(paste0(sMarxanDir,"/input.dat"),"\n"))
        inputdat <- readLines(paste0(sMarxanDir,"/input.dat"))

        fZoneBoundCost <<- (GetParamValue(inputdat,"ZONEBOUNDCOSTNAME") != "")
        fZoneContrib <<- (GetParamValue(inputdat,"ZONECONTRIBNAME") != "")
        fZoneContrib2 <<- (GetParamValue(inputdat,"ZONECONTRIB2NAME") != "")
        fZoneTarget <<- (GetParamValue(inputdat,"ZONETARGETNAME") != "")
        fZoneTarget2 <<- (GetParamValue(inputdat,"ZONETARGET2NAME") != "")

        input_list <- c("feat","zones","costs","zonecost")
        if (fZoneBoundCost)
        {
            input_list <- c(input_list,"zonebound")
        }
        if (fZoneContrib | fZoneContrib2)
        {
            input_list <- c(input_list,"zonecontrib")
        }
        if (fZoneTarget | fZoneTarget2)
        {
            input_list <- c(input_list,"zonetarget")
        }
        input_list <<- input_list
        cat(paste0(input_list,"\n"))
    }
    
    if (sCallingApp == "mxptest")
    {
        # do we need to do RunMarxan_paramtest 1st for this parameter?
        if (!file.exists(paste0(sMarxanDir,"/output/output_",swhichparam,"summary.csv")))
        {
            RunMarxan_paramtest(swhichparam)
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
        }
    }
    
    SelectDatabase(sCallingApp,session)
    PrepareDisplay(sCallingApp)

    # save the database name
    writeLines(sSelectDb,paste0(sAppHome,"/database_",sCallingApp,".txt"))

    cat("ChangeDatabase end\n")
}

#' @export
AddDatabase <- function(sDatabase)
{
    dir.create(sDatabase)
    dir.create(paste0(sDatabase,"/input"))
    dir.create(paste0(sDatabase,"/output"))
    dir.create(paste0(sDatabase,"/pulayer"))
    # copy the marxan files to new directory
    if (fMarZone)
    {
        file.copy(paste0(sUserSession,"/marzone/input.dat"),paste0(sDatabase,"/input.dat"))
        system(paste0("cp ",sUserSession,"/marzone/input/* ",sDatabase,"/input/"))
        system(paste0("cp ",sUserSession,"/marzone/pulayer/* ",sDatabase,"/pulayer/"))
    } else {
        file.copy(paste0(sUserSession,"/marxan/input.dat"),paste0(sDatabase,"/input.dat"))
        system(paste0("cp ",sUserSession,"/marxan/input/* ",sDatabase,"/input/"))
        system(paste0("cp ",sUserSession,"/marxan/pulayer/* ",sDatabase,"/pulayer/"))
    }
}

#' @export
freegeoip <- function(ip, format = ifelse(length(ip)==1,'list','dataframe'))
{
    return("unknown")
}   

#' @export
substrRight <- function(x, n)
{
    substr(x, nchar(x)-n+1, nchar(x))
}

#' @export
gen_pwd <- function(iLength=16)
# password generator. minimum length is 4
# at least 1 upper case character, 1 lower case character, 1 number
# omit IiLlOo01 so no character confusion when reading/typing
{
    library(stringi)
    rand_all <- stri_rand_strings(n=1, length=iLength-3, pattern="[ABCDEFGHJKMNPQRSTUVWXYZabcdefghjkmnpqrstuvwxyz23456789]")
    rand_upper <- stri_rand_strings(n=1, length=1, pattern="[ABCDEFGHJKMNPQRSTUVWXYZ]")
    rand_lower <- stri_rand_strings(n=1, length=1, pattern="[abcdefghjkmnpqrstuvwxyz]")
    rand_numeric <- stri_rand_strings(n=1, length=1, pattern="[23456789]")
    x <- paste0(rand_all,rand_upper,rand_lower,rand_numeric)
    y <- as.data.frame(strsplit(x,""))
    return(paste(as.character(y[sample(nchar(x)),]),collapse=""))
}

#' @export
GenerateLegendPNG <- function(AColour,sOutputDir,sPngName)
{
    sPng <- paste0(sOutputDir,"/",sPngName)
    png(file=sPng,bg=AColour)
    plot(1:10)
    rect(1,10,1,10,col=AColour)
    dev.off()
    # crop a PNG file
    apng <- readPNG(sPng)
    png2 <- apng[1:19,1:19,]
    writePNG(png2,target=sPng)
}

#' @export
GenerateMarZoneLegendPNG <- function(iZones,sOutputDir)
{
    ARainbox <- rainbow(iZones)
    for (i in 1:iZones)
    {
        GenerateLegendPNG(ARainbox[i],sOutputDir,paste0("rainbow_",iZones,"_",i,".png"))
    }
}

#' @export
gen_input_table_zonetarget <- function()
{
    # load requried data
    zonesdat <- read.csv(paste0(sMarxanDir,"/input/zones.dat"),stringsAsFactors=FALSE)
    zt_zones <- zonesdat$zonename

    # create table
    arow <- rep(0,length(zt_zones))
    atable <- c()
    if (fZoneTarget)
    {
        # load required data
        # zoneid,featureid,target,targettype
        zonetarget <- read.csv(paste0(sMarxanDir,"/input/zonetarget.dat"),stringsAsFactors=FALSE)
        colnames(zonetarget) <- c("zoneid","featureid","target","targettype")
        zonetarget <- sqldf("SELECT * from zonetarget where target > 0")
        specdat <- read.csv(paste0(sMarxanDir,"/input/spec.dat"),stringsAsFactors=FALSE)
        if ("name" %in% colnames(specdat))
        {
            specname <- unlist(specdat$name)
        } else {
            specname <- unlist(specdat$id)
        }
        for (i in 1:length(specname))
        {
            atable <- rbind(atable,arow)
        }
        #rownames(atable) <- make.names(specname,unique=TRUE)
        colnames(atable) <- zt_zones
        atable <- cbind(unlist(specname),atable)
        colnames(atable)[1] <- c("features")
        rownames(atable) <- rep(1:length(specname))
 
        # populate table
        for (i in 1:nrow(zonetarget))
        {
            iZoneId <- zonetarget$zoneid[i]
            sSpecId <- zonetarget$featureid[i]
            rTarget <- zonetarget$target[i]
            iSpecIndex <- which(sSpecId==specdat$id)
            atable[iSpecIndex,iZoneId+1] <- rTarget
        }
    }
    if (fZoneTarget2)
    {
        # load required data
        # zoneid,target,targettype
        zonetarget <- read.csv(paste0(sMarxanDir,"/input/zonetarget2.dat"),stringsAsFactors=FALSE)
        colnames(zonetarget) <- c("zoneid","target","targettype")
        zonetarget <- sqldf("SELECT * from zonetarget where target > 0")
        atable <- rbind(atable,arow)
        colnames(atable) <- zt_zones
        atable <- cbind(c("features"),atable)
        colnames(atable)[1] <- c("features")
        rownames(atable) <- c(1)

        #rownames(atable) <- c("features")
        # populate table
        for (i in 1:nrow(zonetarget))
        {
            iZoneId <- zonetarget$zoneid[i]
            rTarget <- zonetarget$target[i]
            atable[1,iZoneId+1] <- rTarget
        }
    }
    #colnames(atable) <- zt_zones

    return(atable)
}

#' @export
gen_input_table_zonecontrib <- function()
{
    # load requried data
    zonesdat <- read.csv(paste0(sMarxanDir,"/input/zones.dat"),stringsAsFactors=FALSE)
    zc_zones <- zonesdat$zonename

    # create table
    arow <- rep(0,length(zc_zones))
    atable <- c()
    if (fZoneContrib)
    {
        # load required data
        # zoneid,featureid,fraction
        zonecontrib <- read.csv(paste0(sMarxanDir,"/input/zonecontrib.dat"),stringsAsFactors=FALSE)
        colnames(zonecontrib) <- c("zoneid","featureid","fraction")
        zonecontrib <- sqldf("SELECT * from zonecontrib where fraction > 0")
        specdat <- read.csv(paste0(sMarxanDir,"/input/spec.dat"),stringsAsFactors=FALSE)
        if ("name" %in% colnames(specdat))
        {
            specname <- unlist(specdat$name)
        } else {
            specname <- unlist(specdat$id)
        }
        for (i in 1:length(specname))
        {
            atable <- rbind(atable,arow)
        }
        #rownames(atable) <- make.names(specname,unique=TRUE)
        colnames(atable) <- zc_zones
        atable <- cbind(unlist(specname),atable)
        colnames(atable)[1] <- "features"
        rownames(atable) <- rep(1:length(specname))

        # populate table
        for (i in 1:nrow(zonecontrib))
        {
            iZoneId <- zonecontrib$zoneid[i]
            sSpecId <- zonecontrib$featureid[i]
            rFraction <- zonecontrib$fraction[i]
            iSpecIndex <- which(sSpecId==specdat$id)
            atable[iSpecIndex,iZoneId+1] <- rFraction
        }
    }
    if (fZoneContrib2)
    {
        # load required data
        # zoneid,fraction
        zonecontrib <- read.csv(paste0(sMarxanDir,"/input/zonecontrib2.dat"),stringsAsFactors=FALSE)
        colnames(zonecontrib) <- c("zoneid","fraction")
        zonecontrib <- sqldf("SELECT * from zonecontrib where fraction > 0")
        atable <- rbind(atable,arow)
        #rownames(atable) <- c("features")
        colnames(atable) <- zc_zones
        atable <- cbind("features",atable)
        colnames(atable)[1] <- "features"
        rownames(atable) <- c(1)

        # populate table
        for (i in 1:nrow(zonecontrib))
        {
            iZoneId <- zonecontrib$zoneid[i]
            rFraction <- zonecontrib$fraction[i]
            atable[1,iZoneId+1] <- rFraction
        }
    }
    #colnames(atable) <- zc_zones

    return(atable)
}

#' @export
gen_input_table_zonecost <- function()
{
    # load required data
    zonesdat <- read.csv(paste0(sMarxanDir,"/input/zones.dat"),stringsAsFactors=FALSE)
    costsdat <- read.csv(paste0(sMarxanDir,"/input/costs.dat"),stringsAsFactors=FALSE)
    zonecost <- read.csv(paste0(sMarxanDir,"/input/zonecost.dat"),stringsAsFactors=FALSE)
    colnames(zonecost) <- c("zoneid","costid","multiplier")
    zonecost <- sqldf("SELECT * from zonecost where multiplier > 0")
    zc_zones <- zonesdat$zonename
    zc_costs <- costsdat$costname

    # create table
    arow <- rep(0,length(zc_zones))
    atable <- c()
    for (i in 1:length(zc_costs))
    {
        atable <- rbind(atable,arow)
    }
    colnames(atable) <- zc_zones
    atable <- cbind(unlist(zc_costs),atable)
    colnames(atable)[1] <- "costs"
    rownames(atable) <- rep(1:length(zc_costs))

    # populate table
    for (i in 1:nrow(zonecost))
    {
        iZoneId <- zonecost$zoneid[i]
        iCostId <- zonecost$costid[i]
        rMultiplier <- zonecost$multiplier[i]
        atable[iCostId,iZoneId+1] <- rMultiplier
    }

    return(atable)
}

#' @export
gen_input_table_zoneboundcost <- function()
{
    if (fZoneBoundCost)
    {
        zoneboundcost <- read.csv(paste0(sMarxanDir,"/input/zoneboundcost.dat"))
        colnames(zoneboundcost) <- c("zoneid1","zoneid2","cost")
        zoneboundcost <- sqldf("SELECT * from zoneboundcost where cost > 0")
        # make blank ZBC table
        ZBC <- rep(0,iZones)
        for (i in 2:iZones)
        {
            ZBC <- rbind(ZBC,rep(0,iZones))
        }
        colnames(ZBC) <- ZoneNames
        rownames(ZBC) <- ZoneNames
        # populate ZBC table from zoneboundcost file
        for (i in (1:nrow(zoneboundcost)))
        {
            ZBC[zoneboundcost$zoneid1[i],zoneboundcost$zoneid2[i]] <- as.character(zoneboundcost$cost[i])
            ZBC[zoneboundcost$zoneid2[i],zoneboundcost$zoneid1[i]] <- as.character(zoneboundcost$cost[i])
        }
        thetable <- ZBC
    } else {
        thetable <- as.data.frame(cbind(c("input","1","2"),c(sTable,"a","b")))
    }
    return(thetable)
}

#' @export
which_zones_targeted <- function()
{
    zones_targeted <- rep(0,iZones)
    if (fZoneTarget)
    {
        zonetarget <- read.csv(paste0(sMarxanDir,"/input/zonetarget.dat"),stringsAsFactors=FALSE)
        colnames(zonetarget) <- c("zoneid","featureid","target","targettype")
    }
    if (fZoneTarget2)
    {
        zonetarget <- read.csv(paste0(sMarxanDir,"/input/zonetarget2.dat"),stringsAsFactors=FALSE)
        colnames(zonetarget) <- c("zoneid","target","targettype")
    }
    zonetarget <- sqldf("SELECT * from zonetarget where target > 0")
    the_zones <- unique(zonetarget$zoneid)
    for (i in 1:length(the_zones))
    {
        iZone <- as.numeric(the_zones[i])
        zones_targeted[iZone] <- 1
    }
    return(zones_targeted)
}

#' @export
return_zone_mv_fields <- function(iTable,WhichZonesTargeted)
{
    # load the missing values table
    sFile <- paste0(sMarxanDir,"/output/output_mv",PadInt(iTable),".csv")
    thetable <- read.csv(sFile,stringsAsFactors=FALSE)

    # get the necessary fields
    mv_fields <- rep(0,nrow(thetable))
    iZonesAdded <- 0
    for (i in 1:iZones)
    {
        if (WhichZonesTargeted[i] > 0)
        {
            iFieldIndex <- round(9 + ((i-1) * 6))
            mv_fields <- cbind(mv_fields,
                               thetable[,iFieldIndex],   # targ field
                               thetable[,iFieldIndex+2], # contrib field
                               thetable[,iFieldIndex+5])  # met field

            if (iZonesAdded == 0)
            {
                mv_fields <- mv_fields[,-1] # drop the first field
            }
            iZonesAdded <- 1

            colnames(mv_fields)[ncol(mv_fields)-2] <- paste0(ZoneNames[i],"_Target")
            colnames(mv_fields)[ncol(mv_fields)-1] <- paste0(ZoneNames[i],"_AmountHeld")
            colnames(mv_fields)[ncol(mv_fields)] <- paste0(ZoneNames[i],"_TargetMet")
        }
    }
    return(mv_fields)
}

#' @export
return_mv_table <- function(iTable)
{
    sFile <- paste0(sMarxanDir,"/output/output_mv",PadInt(iTable),".csv")
    thetable <- read.csv(sFile,stringsAsFactors=FALSE)
            
    # sort the table the way spec.dat is ordered
    tableorder <- seq.int(from=nrow(thetable),to=1)
    thetable <- thetable[tableorder,]

    # rename & extract relevant fields
    colnames(thetable)[2] <- "Name"
    colnames(thetable)[4] <- "Total"
    colnames(thetable)[5] <- "AmountHeld"
    colnames(thetable)[8] <- "TargetMet"
    # are overall targets in use?
    if (sum(thetable$Target) > 0)
    {
        thetable <- sqldf("SELECT Name, Total, Target, AmountHeld, TargetMet from thetable")
    } else {
        thetable <- sqldf("SELECT Name, Total from thetable")
    }

    # are zone targets in use?
    # which zones have targets? - display 3 fields for each: targ, contrib, met
    if (fZoneTarget | fZoneTarget2)
    {
        WhichZonesTargeted <- which_zones_targeted()
        Zone_MV_Fields <- return_zone_mv_fields(iTable,WhichZonesTargeted)
        Zone_MV_Fields <- Zone_MV_Fields[tableorder,]

        thetable <- cbind(thetable,Zone_MV_Fields)
    }
}

#' @export
elapsed_to_string <- function(iElapsed)
{
    if (iElapsed < 1)
    {
        sElapsed <- "< 1s"
    } else {
        sElapsed <- seconds_to_period()
    }
    return(sElapsed)
}
