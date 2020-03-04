#' @export
runSimulation <- function() {
  appDir <- system.file("shiny-examples", "distsim", package = "distsim")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `distsim`.", call. = FALSE)
  }
  shiny::runApp(appDir, display.mode = "normal")
}
