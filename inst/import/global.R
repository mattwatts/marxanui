# marxan.io Upload a Dataset

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

iupdateusermessages <<- 0
puid_choices <<- c("a","b")
iupdatepuidchoices <<- 0
fUserMessagesFile <<- FALSE
input_messages_df <<- c()#as.data.frame(c("a"))
iCores <<- 10
iRepsPerCore <<- 10

fLocalUser <<- TRUE

#close_jscode <<- '
#  Shiny.addCustomMessageHandler("myCallbackHandler",
#    function(a_msg){
#      function close_window(){
#       var jsWidth = window.innerWidth;
#        var jsHeight = window.innerHeight;
#        var obj = {width: jsWidth, height: jsHeight};
#        Shiny.onInputChange("GetResize", obj);
#      }
#
#      var doit;
#      timer.close = function(){
#        clearTimeout(doit);
#        doit = setTimeout(close_window, 2000);
#      };
#  });
#  '

