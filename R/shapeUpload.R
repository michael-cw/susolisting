#' Function for shape file upload and validity check
#'
#' @noRd
#' @keywords internal

shapeLoad2<-function(path=NULL, lay=NULL, sp.Library="sf"){
  # list for return
  outlist<-vector("list",2)
  SHP<-sf::st_read(path)
  ## Check validity and make valid
  crsOld<-sf::st_crs(SHP)
  # validate shape file
  if (sum(sf::st_is_valid(SHP))!=nrow(SHP)) {
    suppressWarnings(
      SHP<-SHP %>%
        sf::st_transform(3857) %>%
        sf::st_make_valid() %>%
        sf::st_cast("MULTIPOLYGON") %>%
        sf::st_buffer(0.0) %>%
        sf::st_transform(crsOld)
    )
  }
  # transform to lat long
  if(!sf::st_is_longlat(SHP)) {
    SHP<-SHP %>% sf::st_transform(4326)
  }
  # return list with crs and shape
  outlist[[1]]<-sf::st_crs(SHP)[[2]]
  outlist[[2]]<-SHP
  return(outlist)

}
