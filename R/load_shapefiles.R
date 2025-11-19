#' Loads a simplified shapefile for EU coastline.
#' 
#' @return sf file
#'
#'@import sf
#'@import dplyr
#'@export
#' 
load_shape_europe <- function(){
  
  eu_shape <- sf::st_read(dsn = "inst/app/www/Shapefiles/shape_eu", 
                      layer = "world_map_simplified", quiet=TRUE)
  return(eu_shape)
}

#' Loads a simplified shapefile for ICES Ecoregions.
#' 
#' @return sf file
#'
#' @note
#' Can add some helpful information here
#'
#' @examples
#' \dontrun{
#' load_shape_ecoregions()
#' }
#'
#' 
#'
#' @export
#'
load_shape_ecoregions <- function(){
  shape_eco <- sf::st_read(dsn = "inst/app/www/Shapefiles/shape_ecoregion", 
                       layer = "shape_eco_simplified", quiet=TRUE)
  
  
  shape_eco <- dplyr::filter(shape_eco, Ecoregion %in% c("Norwegian Sea","Icelandic Waters"))
  # Add an id to each ecoregion (this potentially can be eliminated because the ecoregions in the shape file have already an id)
  shape_eco$uid <- paste0("P", 1:2)
  return(shape_eco)
}
######## this is the library I used to reduce the shape file size for quicker app loading (we can test a keep = 0.01 and see how it looks)
# library(rmapshaper)
# # object.size(shape_eco)
# # df1 <- ms_simplify(shape_eco, keep = 0.05, keep_shapes = TRUE)
# # object.size(df1)
# # df2 <- ms_simplify(eu_shape, keep = 0.05, keep_shapes = TRUE)
# eu_shape <-df2
# shape_eco <- df1
# st_write(df1, "shape_eco_simplified.shp")
# st_write(df2, "shape_EU_simplified.shp")


# object.size(df2)

############# Load shapefiles ###########
eu_shape <- load_shape_europe()
shape_eco <- load_shape_ecoregions()
