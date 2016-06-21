#' @export
runImport <- function() {
  appDir <- system.file(".", "import", package = "marxanui")
  if (appDir == "") {
    stop("Could not find app directory. Try re-installing `marxanui`.", call. = FALSE)
  }

  shiny::runApp(appDir, display.mode = "normal")
}