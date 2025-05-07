# Spatial selector creates the spatial data in the raw_data folder.

# MAR watersheds

require(sf)
require(ggplot2)
require(tidyverse)
require(utils)
require(xfun)
require(leaflet)

direct <- "./raw_data/"

# NAFO
units <- st_read("./raw_data/nafos/DFO_UnitAreas_AtlanticCanada_2023_SHP/DFO_UnitAreas_2023_poly_clipped.shp")
units <- st_transform(units, 4326)
units <- units[!units$UnitArea %in% c("5ZeM", "5ZeJ"),]
units <- units[which(substr(units$UnitArea, 1,1)>3),]
# for clipping watersheds later
nafo_bound <- units %>% dplyr::summarize()

# for PPT geotags
units_ppt <- data.frame(name_en = units$UnitArea,
                      name_fr = units$UnitArea,
                      wkt = st_as_text(st_geometry(units)))
units_ppt$name_en <- paste0("DFO Unit Area - ", units_ppt$name_en)
units_ppt$name_fr <- paste0("DFO Unit Area - ", units_ppt$name_fr)

if(any(nchar(units_ppt$wkt)>100000)) {
  units_ppt <-  units %>%
    st_transform(32619) %>%
    dplyr::group_by(UnitArea) %>%
    st_set_precision(50) %>%
    st_make_valid() %>%
    dplyr::summarise()

  units_ppt <- st_simplify(units_ppt, dTolerance = 50) %>% st_transform(4326)
  units_ppt <- data.frame(name_en = units_ppt$UnitArea,
                          name_fr = units_ppt$UnitArea,
                          wkt = st_as_text(st_geometry(units_ppt)))
  units_ppt$name_en <- paste0("DFO Unit Area - ", units_ppt$name_en)
  units_ppt$name_fr <- paste0("DFO Unit Area - ", units_ppt$name_fr)

}

write.csv(x = units_ppt, "./raw_data/DFOunits_ppt.csv")

#########################
# Watersheds

# to load the pre-processed watershed boundaries:
shp <- st_read("./raw_data/watersheds.shp")


# to regenerate from original SHP file (uncomment the commented code!):
# source("raw_data/watershed_download.R")
#
# shp_mar <- watershed_download() #1,1,0,0
# shp_gul <- watershed_download() #1,2,0,0
# shp_mar <- shp_mar %>% st_transform(4326)
# shp_gul <- shp_gul %>% st_transform(4326)
# shp <- rbind(shp_mar, shp_gul)
# # pick main gulf/mar only
# picks <- unique(shp$WSCSDANAME)[c(1,2,3,4,5,6)]#18,19,20,21,22,23, 24,25,26, 27, 28, 29)]
#
# shp <- shp[shp$WSCSDANAME %in% picks,]
#
# shp <- st_difference(shp, st_make_valid(nafo_bound))
#
# shp <- shp %>%
#   st_transform(32619) %>%
#   dplyr::group_by(WSCSSDANAM) %>%
#   st_set_precision(100) %>%
#   st_make_valid() %>%
#   dplyr::summarise()
#
# shp <- st_simplify(shp, dTolerance = 100) %>% st_transform(4326)
#
# shp <- dplyr::select(shp, WSCSSDANAM, geometry)
# #st_write(obj = shp, dsn = "./raw_data/watersheds.shp", delete_layer=T)
#
# shp_ppt <- data.frame(name_en = shp$WSCSSDANAM,
#                       name_fr = shp$WSCSSDANAM,
#                       wkt = st_as_text(st_geometry(shp)))
# shp_ppt$name_en <- paste0("Watershed boundary - ", shp_ppt$name_en)
# shp_ppt$name_fr <- paste0("Watershed boundary - ", shp_ppt$name_fr)
#
# #write.csv(x = shp_ppt, "./raw_data/watersheds_ppt.csv")
# #write.csv(x = shp_ppt[1:5,], "./raw_data/watersheds_ppt1.csv")
#
# # nchar(shp_ppt$wkt)

#############################
# Plotting

map <- leaflet() %>%
  addProviderTiles(providers$Esri.WorldTopoMap) %>%
  addPolygons(data=shp,
              color = "forestgreen",
              label = ~WSCSSDANAM) %>%
  addPolygons(data=units,
              color="blue",
              label = ~UnitArea)
map

# Save the leaflet map as an HTML file
#htmlwidgets::saveWidget(map, "map2.html", selfcontained = TRUE)

# Open the saved HTML file in a browser
#browseURL("map2.html")

