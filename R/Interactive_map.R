#' Function to plot the intercative map of ecoregions. Taken from AdviceXplorer. https://github.com/ices-tools-dev/adviceXplorer/
#'
#' @param shape_eco (ecoregions' shapefile)
#' @param eu_shape (europe's shapefile)
#'
#' @return leaflet object
#'
#' @examples
#' \dontrun{
#' map_ecoregion(shape_eco, eu_shape)
#' }
#'@import leaflet
#' @export
#' 
#' 
map_ecoregion <- function(shape_eco, eu_shape) {
  minZoom <- 0.5
  maxZoom <- 14
  resolutions <- 1.8 * (2^(maxZoom:minZoom))
  crs_laea <- leafletCRS(
    crsClass = "L.Proj.CRS", code = "EPSG:3035",
    proj4def = "+proj=laea +x_0=0 +y_0=0 +lon_0= -1.235660 +lat_0=60.346958",
    resolutions = resolutions
  )
  
  leaflet(options = leafletOptions(crs = crs_laea, minZoom = 1, maxZoom = 2)) %>%
    addPolygons(
      data = eu_shape,
      color = "black",
      weight = 1,
      fillOpacity = 1,
      fillColor = "#006494",
      group = "Europe",
      options = pathOptions(clickable = FALSE)
    ) %>%
    addPolygons(
      data = shape_eco,
      fillColor = "#e9ecef",
      fillOpacity = 1,
      color = "black",
      stroke = TRUE,
      weight = 1,
      layerId = ~Ecoregion,
      group = "Eco_regions",
      label = ~Ecoregion
    ) %>%
    addPolygons(
      data = shape_eco,
      fillColor = "#1B98E0",
      fillOpacity = 1,
      weight = 1,
      color = "black",
      stroke = TRUE,
      layerId = ~OBJECTID,
      group = ~Ecoregion
    ) %>%
    setView(lng = -1.235660, lat = 60.346958, zoom = 0.5) %>%
    hideGroup(group = shape_eco$Ecoregion)
}


#' Server side functionality for the landing page map panel
#'
#' @param input 
#' @param output 
#' @param session 
#'
#'@import leaflet
#' @export
map_panel_server <- function(input, output, session) {
  
  output$map1 <- renderLeaflet({
    map_ecoregion(shape_eco, eu_shape)
  })
  
  proxy_1 <- leafletProxy("map1")
  
  # Holds one selected region
  selected <- reactiveVal(NULL)
  
  observeEvent(input$map1_shape_click, {
    
    clicked_id   <- input$map1_shape_click$id
    clicked_group <- input$map1_shape_click$group
    
    # Only react to Eco_regions group (outer polygons)
    if (clicked_group != "Eco_regions")
      return(NULL)
    
    # If the user clicks the same region → deselect
    if (!is.null(selected()) && selected() == clicked_id) {
      proxy_1 %>% hideGroup(group = clicked_id)
      selected(NULL)
      return(NULL)
    }
    
    # If new region selected:
    # 1️⃣ hide previously shown group
    if (!is.null(selected())) {
      proxy_1 %>% hideGroup(group = selected())
    }
    
    # 2️⃣ show the clicked group
    proxy_1 %>% showGroup(group = clicked_id)
    
    # 3️⃣ store selection
    selected(clicked_id)
    
  })
  
  return(reactive(selected()))
}