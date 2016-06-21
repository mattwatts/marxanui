#' @export
safe_log <- function(rValue)
{
    if (rValue > 0)
    {
        return(log(rValue))
    } else {
        return(-20)
    }
}

#' @export
ExecuteMarxan_paramtest <- function(sParam,rMin,rMax,rUserBLM,rUserSPF,rUserTarg)
{
    cat(paste0("ExecuteMarxan_paramtest start\n"))

    withProgress(message="Run parameter test",value=0,
    {
        withProgress(message=sParam,value=0,
        {
            if (sParam == "BLM")
            {
                rMinimum <- safe_log(rMin)
                rMaximum <- safe_log(rMax)
                rInterval <- (rMaximum - rMinimum) / (iCores-1)
                rValue <- rMin
            }
            if (sParam == "SPF")
            {
                rMinimum <- safe_log(rMin)
                rMaximum <- safe_log(rMax)
                rInterval <- (rMaximum - rMinimum) / (iCores-1)
                rValue <- rMin
            }
            if (sParam == "Targ")
            {
                rMinimum <- rMin
                rMaximum <- rMax
                rInterval <- (rMaximum - rMinimum) / (iCores-1)
                rValue <- rMin
            }

            # create the ramped value file
            write(paste0('i,',sParam),file=paste0(sMarxanDir,"/",sParam,".csv"))
            write(paste0(1,",",rValue),file=paste0(sMarxanDir,"/",sParam,".csv"),append=TRUE)
            for (i in 2:iCores)
            {
                if (sParam == "Targ")
                {
                    rValue <- rMinimum+((i-1)*rInterval)       # linear ramping for Target
                } else {
                    rValue <- exp(rMinimum+((i-1)*rInterval))  # exponential ramping for BLM, SPF and Cost
                }
                write(paste0(i,",",rValue),file=paste0(sMarxanDir,"/",sParam,".csv"),append=TRUE)
            }

            # initialise the parameter summary file
            sSummary <- paste0(sMarxanDir,"/output/output_",sParam,"summary.csv")
            if (sParam == "BLM")  { write("i,BLM,cost,boundary length",file=sSummary) }
            if (sParam == "SPF")  { write("i,SPF,cost,shortfall",file=sSummary) }
            if (sParam == "Targ") { write('i,Targ,cost',file=sSummary) }

            # load the ramped value file
            VALUEcsv <- read.csv(paste0(sMarxanDir,"/",sParam,".csv"))
            
            randomseeds <- round(runif(10)*100000)

            #if (fWindows) { registerDoParallel(makeCluster(iCores,type="PSOCK")) }
            registerDoParallel(makeCluster(iCores,type="PSOCK"))

            # need to export objects not in local environment
            export_list <- c('fWindows','sMarxanDir','sShinyDataPath','sExecutable','iRepsPerCore')

            # prepare the Marxan input files
            foreach(i=1:iCores,.export=export_list) %dopar%
            {
                dir.create(paste0(sMarxanDir,"/core",i))
                file.copy(paste0(sShinyDataPath,"/",sExecutable),paste0(sMarxanDir,"/core",i,"/",sExecutable))
                if (!fWindows) { system(paste0("chmod +x ",sMarxanDir,"/core",i,"/",sExecutable)) }

                # read input.dat and edit parameters
                inputdat <- readLines(paste0(sMarxanDir,"/input.dat"))
                iINPUTDIRparam <- which(regexpr("INPUTDIR",inputdat)==1)
                iOUTPUTDIRparam <- which(regexpr("OUTPUTDIR",inputdat)==1)
                iBLMparam <- which(regexpr("BLM",inputdat)==1)
                iSCENNAMEparam <- which(regexpr("SCENNAME",inputdat)==1)
                iNUMREPSparam <- which(regexpr("NUMREPS",inputdat)==1)
                iSPECNAMEparam <- which(regexpr("SPECNAME",inputdat)==1)
                iPUNAMEparam <- which(regexpr("PUNAME",inputdat)==1)
                iRANDSEEDparam <- which(regexpr("RANDSEED",inputdat)==1)
                # read spec.dat
                specdat <- read.csv(paste0(sMarxanDir,"/input/spec.dat"))
                if (sParam == "BLM")
                {
                    inputdat[iBLMparam] <- paste0("BLM ",VALUEcsv[i,2])
                    specdat$spf <- rUserSPF
                    specdat$prop <- rUserTarg
                }
                if (sParam == "SPF")
                {
                    inputdat[iBLMparam] <- paste0("BLM ",rUserBLM)
                    specdat$spf <- VALUEcsv[i,2]
                    specdat$prop <- rUserTarg
                }
                if (sParam == "Targ")
                {
                    inputdat[iBLMparam] <- paste0("BLM ",rUserBLM)
                    specdat$spf <- rUserSPF
                    specdat$prop <- VALUEcsv[i,2]
                }
                # save spec.dat
                write.csv(specdat,paste0(sMarxanDir,"/input/spec",sParam,i,".dat"),quote=FALSE,row.names=FALSE)
                # edit parameters
                inputdat[iINPUTDIRparam] <- paste0("INPUTDIR ",sMarxanDir,"/input")
                inputdat[iOUTPUTDIRparam] <- paste0("OUTPUTDIR ",sMarxanDir,"/output")
                inputdat[iSPECNAMEparam] <- paste0("SPECNAME spec",sParam,i,".dat")
                inputdat[iSCENNAMEparam] <- paste0("SCENNAME output",sParam,i)
                inputdat[iNUMREPSparam] <- paste0("NUMREPS ",iRepsPerCore)
                inputdat[iRANDSEEDparam] <- paste0("RANDSEED ",randomseeds[i])
                # save input.dat
                writeLines(inputdat,paste0(sMarxanDir,"/core",i,"/input",sParam,i,".dat"))
            }
            
            cat("ExecuteMarxan_paramtest before run Marxan\n")
            export_list <- c('fWindows','sMarxanDir','sExecutable')

            # run Marxan
            foreach(i=1:iCores, .export=export_list) %dopar%
            {
                setwd(paste0(sMarxanDir,"/core",i))
                if (fWindows)
                {
                    system2(sExecutable,paste0("-s input",sParam,i,".dat"),wait=T)
                } else {
                    system(paste0("./",sExecutable," -s input",sParam,i,".dat"))
                }
                #system(paste0("./",sExecutable," -s input",sParam,i,".dat"))

                # read the Marxan summary file
                sumfile <- read.csv(paste0(sMarxanDir,"/output/output",sParam,i,"_sum.csv"))

                # write to the parameter summary file
                sSummaryI <- paste0(sMarxanDir,"/output/output_",sParam,"summary",i,".csv")
                if (sParam == "BLM")  { write(paste(i,VALUEcsv[i,2],mean(sumfile$Cost),mean(sumfile$Connectivity),sep=","),file=sSummaryI) }
                if (sParam == "SPF")  { write(paste(i,VALUEcsv[i,2],mean(sumfile$Cost),mean(sumfile$Shortfall),sep=","),file=sSummaryI) }
                if (sParam == "Targ") { write(paste(i,VALUEcsv[i,2],mean(sumfile$Cost),sep=","),file=sSummaryI) }
            }

            #if (fWindows) { registerDoSEQ() }
            registerDoSEQ()

            # compose parameter summary table across all parallel runs
            for (i in 1:iCores)
            {
                sSummaryI <- paste0(sMarxanDir,"/output/output_",sParam,"summary",i,".csv")
                if (sParam == "BLM")  { write(readLines(con=sSummaryI),file=sSummary,append=TRUE) }
                if (sParam == "SPF")  { write(readLines(con=sSummaryI),file=sSummary,append=TRUE) }
                if (sParam == "Targ") { write(readLines(con=sSummaryI),file=sSummary,append=TRUE) }
            }

            # compose parameter summary table where values are cumulatively added during workflow
            if (sParam == "BLM")  { sAppendSummary <<- paste0(sMarxanDir,"/output/output_BLMsummary_SPF",rUserSPF,"_Targ",rUserTarg,".csv") }
            if (sParam == "SPF")  { sAppendSummary <<- paste0(sMarxanDir,"/output/output_SPFsummary_BLM",rUserBLM,"_Targ",rUserTarg,".csv") }
            if (sParam == "Targ") { sAppendSummary <<- paste0(sMarxanDir,"/output/output_Targsummary_BLM",rUserBLM,"_SPF",rUserSPF,".csv") }
            if (file.exists(sAppendSummary))
            {
                # ignore header row in sSummary if sAppendSummary exists
                sBuffer <- readLines(con=sSummary)
                write(sBuffer[-1],file=sAppendSummary,append=TRUE)
            } else {
                write(readLines(con=sSummary),file=sAppendSummary,append=FALSE)
            }
        })
    })

    cat(paste0("ExecuteMarxan_paramtest end\n"))
}

#' @export
RunMarxan_paramtest <- function(sParam)
{
    if (sParam == "BLM")
    {
        rMin <- 0
        rMax <- 10000000000000
    }
    if (sParam == "SPF")
    {
        rMin <- 0.0001
        rMax <- 10000000000000
    }
    if (sParam == "Targ")
    {
        rMin <- 0
        rMax <- 1
    }

    ExecuteMarxan_paramtest(sParam=sParam,rMin=rMin,rMax=rMax,
                            rUserBLM=0,rUserSPF=1,rUserTarg=0.3)
}

#' @export
RunMarxan_paramtest_app <- function(sParam)
{
    # set min, max, interval for value ramping
    if (sParam == "BLM")
    {
        rMin <- rRampBLMmin
        rMax <- rRampBLMmax
    }
    if (sParam == "SPF")
    {
        rMin <- rRampSPFmin
        rMax <- rRampSPFmax
    }
    if (sParam == "Targ")
    {
        rMin <- rtargetmin
        rMax <- rtargetmax
    }

    ExecuteMarxan_paramtest(sParam=sParam,rMin=rMin,rMax=rMax,
                            rUserBLM=ruserblm,rUserSPF=ruserspf,rUserTarg=rusertarg)
}


