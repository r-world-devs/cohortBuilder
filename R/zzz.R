

.onUnload <- function(...) {
  if ("S7_on_unload" %in% getNamespaceExports("S7")) {
    getExportedValue("S7", "S7_on_unload")()
  }
}

if ("S7_on_build" %in% getNamespaceExports("S7")) {
  getExportedValue("S7", "S7_on_build")()
}
