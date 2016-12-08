#' make_impact_table
#' @description Convenience function to make a table showing difference between
#'   baseline and project scenario
#' @param base Raster
#' @param proj Raster
#' @param def Raster
#' @param pmvar Character Name of pollutant to select. Must match in names(base) and names(proj)
#' @export
#' 

make_impact_table <- function(base, proj, def, pmvar = "pm", impname = "Deficit"){
res <- as.data.frame(rbind(
  Baseline =summary(cellStats(base[[grep(pmvar, names(base))]], mean)), 
  Project = summary(cellStats(proj[[grep(pmvar, names(proj))]], mean)), 
  Deficit = summary(cellStats(def[[grep(pmvar, names(def))]], mean))))
rownames(res)[3] <- impname
res
}



