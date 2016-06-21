# marxan.io Run Marxan

cat(paste0("hello\n"))
cat(paste0(getwd(),"\n"))

sAppDir <<- getwd()

sShinyPath <<- Sys.getenv("HOME")
sShinyUserPath <<- paste0(sShinyPath,"/marxanui/")
sShinyLogPath <<- paste0(sShinyPath,"/marxanui/log/")
sShinyDataPath <- paste0(sShinyPath,"/marxanui/data/")
sShinyTempPath <<- paste0(sShinyPath,"/marxanui/temp/")

#iRevision <- 48
sUserGuide <<- "Marxan_io_rev39_user_guide.pdf"

# where are the apps stored?
if (.Platform$pkgType == "source")
{
  # Linux
  
} else {
  if (.Platform$pkgType == "win.binary")
  {
    # Windows
    sAppPath <<- "f:/marxanui"
  } else {
    # Mac
    sAppPath <<- "/Users/matt/Documents/R/marxanui"
  }
}

sShinySourcePath <<- paste0(sAppPath,"/source/")

sDatabase <<- ""
sSelectDb <<- "Tasmania"
sMarxanDir <<- ""
irefreshinput <<- 0
irefreshmrun <<- 0
isavetargetspf <<- 0
sSampleMarxanDataset <<- "Tasmania"
sSampleMarZoneDataset <<- "RottnestIsland_Scenario4"
sBLM <<- 0.1
sProp <<- 0.3
sSPF <<- 1
iSpecDatRows <<- 0
iCores <<- 10
iRepsPerCore <<- 10

swhichparam <<- "BLM"
ruserblm <<- 0
ruserspf <<- 1
rusertarg <<- 0.3
irefreshptinput <<- 0

iParamTestReps <<- 10

fLocalUser <<- TRUE
