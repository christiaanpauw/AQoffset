# plume functions
# vanaf http://www.maths.lancs.ac.uk/~rowlings/Teaching/UseR2012/plume.html
# Barry Rowlingson 2012
# Lancaster University

# plume function
# f=α exp(−(d/β)^2 e^−κcos(θ−ϕ)2)
# Where α is the pollution level at distance zero, 
# β is a distance scale factor, 
# ϕ is the major angle of the pollution plume, and 
# κ is the eccentricity. If κ is 0 then the plume becomes a circularly symmetric exponential decay, and as κ increases the plume becomes more stretched in the direction of the ϕ parameter. 
# You can consider κ and ϕ as the wind strength and direction respectively.

pfunc = function(d2, ang, a, b, k, phi) {
  ## plume value for distance-squared d2
  a * exp(-(d2 * exp(-k * cos(ang - phi))^2/b^2))
}

plume <- function(src, dst, a, b, k, phi) {
  ## plume value for src at dst
  src = coordinates(src)
  dst = coordinates(dst)
  d2 = (dst[, 1] - src[, 1])^2 + (dst[, 2] - src[, 2])^2
  ang = atan2(dst[, 2] - src[, 2], dst[, 1] - src[, 1])
  pfunc(d2, ang, a, b, k, phi)
}

toOSGB = function(s) {
  spTransform(s, CRS("+init=epsg:27700"))
}

invpfunc <- function(ang, a, b, k, phi, f) {
  ### inverse plume function - compute distance given f
  d2 = -log(f/a) * b^2/exp(-k * cos(ang - phi))^2
  sqrt(d2)
}

contourPlume <- function(src, a, b, k, phi, f) {
  ### return a SpatialPolygon of a plume function at value f
  ### by sweeping round 360 degrees and computing the distance
  ang = seq(0, 2 * pi, len = 360)
  d = invpfunc(ang, a, b, k, phi, f = f)
  xy = coordinates(src)
  polys = SpatialPolygons(list(Polygons(list(Polygon(cbind(xy[, 1] + d * sin(ang), xy[, 2] + d * cos(ang)))),
                                        ID = 1)))
  proj4string(polys) = proj4string(src)
  polys
}