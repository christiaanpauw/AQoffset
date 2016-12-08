#' Summary_PTI
#' @description Summarise the proportion to implement. Convenience function to
#'   print table of proportion to implement. Possibly masking with a SPDF
#' @param s RasterStack
#' @param msk SPDF to act as mask or NULL
#' @param nm Character Name of row or NULL
#' @export

summary_PTI <- function(s, msk = KWA[which(KWA$SP_NAME=="Kwazamokuhle SP"), ], nm = NULL){
  s[is.infinite(s)] <- NA
  s[s==0] <- NA
  if (!is.null(msk)) s <- mask(s, msk)
  res <- broom::tidy(summary(cellStats(s, mean)))
  if (!is.null(nm)) rownames(res) <- nm
  res 
}