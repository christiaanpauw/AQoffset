#' make_population_brick Make a raster brick that is the equavalent of a population pyramid
#' @details Every layer represents an age sex-group e.g. men-42 represents men aged 
#' older or equal to 42 and less than 43. The use of this is to apply an CRF to each 
#' layer depending on whether that layer qualifies. E.g. If the function is defined for 
#' people above 18 then a function must select only the layers that meet the requirement
#' @param r A Raster containing population numbers for males and females. If you only have the total, proportion_sex() will 
#' be used internally to make a two layer raster brick with male and female numbers. You can provide a prop.men if you have it 
#' (else the default of 0.48 will be used)
#' @param prop A numeric vector giving the proportion of the population per age or sex group
#' @param DF a data frame containing A column of age groups cut-offs, a column of proportion of mae and a column of proportion of females
#' @param groupname The variable name in DF that contains the age group descriptions
#' @param malename The variable name in DF that contains proportions of males per age group
#' @param femalename The variable name in DF that contains proportions of females per age group


proportion_sex <- function(r, prop.men = 0.48){
  # prop.men must be between 0 and 1
  res = r
  res$male <- r * prop.men 
  res$female <- r - res$male
  res <- dropLayer(res, 1)
  res
}

agify <- function(r, breaks, prop = 1/10){
  res = x * prop
  res
}

sexify_age <- function(r, 
                       DF, 
                       groupname = "Age_group", 
                       malename = "Male", 
                       femalename = "Female", prop.male = 0.48){
  if (nlayers(r) == 1) r = proportion_sex(r, prop.men = prop.male)
  bb = brick(x = extent(r), nl = 2 * nrow(pop_dist))
  ncol(bb) <- ncol(r)
  nrow(bb) <- nrow(r)
  names(bb) <- c(paste("female", DF[ ,groupname]),paste("male", DF[ ,groupname]))
  bb <- setValues(bb, 
                  c(rep(DF[ ,femalename], each = ncell(bb)), 
                    rep(DF[ ,malename], each = ncell(bb))))
  r[r == 0] <- NA
  bb[is.na(r)] <- NA
  femalelayers = grep("female", names(bb))
  bbf <- bb[[femalelayers]] * r$female
  names(bbf) <- names(bb)[femalelayers]
  bbm <- bb[[-femalelayers]] * r$male
  names(bbm) <- names(bb)[-femalelayers]
  bb <- stack(bbf, bbm)
  bb
}