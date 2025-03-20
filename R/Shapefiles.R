library(sf)

# Read shapefiles, validate, and simplify geometries
shapefiles <- st_read("Data/ICES_ecoregions")

# Extract unique Ecoregions (optional, for display purposes)
ecoregions <- shapefiles %>%
  select(Ecoregion)%>%
  filter(Ecoregion=="Norwegian Sea")

saveRDS(ecoregions,"Data/Ecoregions.RData")
