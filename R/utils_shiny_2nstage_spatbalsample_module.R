#' utm transformation by long/lat zone
#'
#'
#' @noRd
#' @keywords internal
#'


project_to_utm<-function(shp) {
  ## 1. get zone
  maxLong<-sf::st_bbox(shp)[3]
  long2UTM <- function(long=maxLong) {
    (floor((long + 180)/6) %% 60) + 1
  }
  ## 2. Create CRS string
  utmZone<-long2UTM()
  epsg<-ifelse(sf::st_bbox(shp)[4]<=0, sprintf("327%2d", utmZone) ,sprintf("326%2d", utmZone))
  #crs=(paste0("+proj=utm +south +zone=", utmZone, " +ellps=WGS84 +towgs84=0,0,0, +init=epsg:",epsg))
  ## 3. Transform
  shp<-shp %>% sf::st_transform(as.numeric(epsg))
  return(shp)
}
