# Make geodatabase for CDOS
# THIS IS A GUESS AT WHAT THEY MIGHT WANT!
# I am very sorry if you have to edit it.

# You need a "cookie" to read Fieldnotes info from the PPT. Follow the instructions in Jaimie Harbin's dataSPA package for this:
# From Jaimie: This function will only work for authorized Department of Fisheries and Oceans Canada (DFO) employees. As shown in the help documentation for this function (`?getData`), a `cookie` is required. A `cookie` can be obtained by DFO staff and will need to be updated every few weeks by following the steps outlined below:
#
#   **1. Sign into DMApps on the internet**
#
#   **2. Right click in DMApps and click "Inspect Panel".**
#
# **3. Click the + button to the right of "Elements" and click on "Application"**
#
# **4. Under "Cookies" on the left, click on "http://dmapps" to find your csrftoken and sessionid and copy their value**
#
#   **5. Assign a variable in R with the following format:**
#
# cookie <- "csrftoken=YOURTOKEN; sessionid=YOURSESSIONID"
#
# This variable `cookie` will then act as your `cookie` argument in the `getData()` function,


direct <- "./raw_data/"

require(sf)
require(stringr)
require(dataSPA)
require(tidyverse)
fieldnotes <- getData(type="fieldnotes", cookie=cookie, age=1000, keep=T, path = "C:/Users/keyserf/Documents/temp_data/")

# Setting up the target spatial raster
units <- st_read("./raw_data/nafos/DFO_UnitAreas_AtlanticCanada_2023_SHP/DFO_UnitAreas_2023_poly_clipped.shp")
units <- st_transform(units, 4326)
units <- units[!units$UnitArea %in% c("5ZeM", "5ZeJ"),]
units <- units[which(substr(units$UnitArea, 1,1)>3),]
# for clipping watersheds later
nafo_bound <- units %>% dplyr::summarize()
grid <- st_as_sfc(st_bbox(nafo_bound)) %>% st_transform(32619)

# to get gridsize^2 grid
get_grid <- function(gridsize, polygon) {
  require(sf)
  require(raster)
  require(stars)
  nx <- round((st_bbox(polygon)$xmax[[1]] - st_bbox(polygon)$xmin[[1]])/gridsize, 0)
  ny <- round((st_bbox(polygon)$ymax[[1]] -  st_bbox(polygon)$ymin[[1]])/gridsize, 0)
  # grid cells are 5 km2

  # adjust these numbers to get exactly 1km2
  xmin <- st_bbox(polygon)$xmin[[1]]
  xmax <- xmin + ((nx+1)*gridsize)
  ymin <- st_bbox(polygon)$ymin[[1]]
  ymax <- ymin + ((ny+1)*gridsize)
  box <- st_as_sf(x = expand.grid(x=c(xmin,xmax), y=c(ymin, ymax))[c(1,2,4,3),],coords=c(X="x", Y="y"), crs=32619) %>%
    st_combine() %>%
    st_cast("POLYGON")
  r <- st_rasterize(sf = st_sf(box), template = st_as_stars(box, nx = (nx+1), ny = (ny+1)))
  r <- st_as_sf(r)
  r$cell <- 1:nrow(r)
  return(r)
}

#10 x 10 km grid cells = 100 km^2 = 10000 below
grid <- get_grid(10000, grid)
#plot(grid)


#formatting info from Fieldnotes PPT API!
geodatabase <- NULL
attributes <- NULL
for (i in unique(fieldnotes$overview$project)){
  # rasterize geotags into "fishnet" object
  ppt_wkt <- fieldnotes$map[fieldnotes$map$project==i,]
  # rasterize geotags into "fishnet" object
  ppt_wkt <- fieldnotes$map[fieldnotes$map$project==i,]

  # convert WKT from PPT back to sf object
  ppt_wkt <- st_as_sf(ppt_wkt, wkt="wkts", crs=4326) %>%
    st_transform(32619)

  # intersect with grid
  ppt_wkt2 <- st_intersection(grid, ppt_wkt)
  gridded <- grid[grid$cell %in% ppt_wkt2$cell,]
  st_geometry(ppt_wkt2) <- NULL
  gridded <- left_join(gridded, ppt_wkt2)

  ggplot() +
    #geom_sf(data=nafo_bound) +
    geom_sf(data=ppt_wkt, fill="blue") +
    geom_sf(data=gridded, fill=NA, colour="red")

  ### populate the other geodatabase fields
  project <- i
  unique_id <- fieldnotes$overview[fieldnotes$overview$project==i,]$fn_identifier[[1]]
  title_en <- fieldnotes$overview[fieldnotes$overview$project==i,]$title_en[[1]]
  title_fr <- fieldnotes$overview[fieldnotes$overview$project==i,]$title_fr[[1]]
  subtitle_en <- NA
  subtitle_fr <- NA
  category_en <- fieldnotes$overview[fieldnotes$overview$project==i,]$category_en[[1]]
  category_fr <- fieldnotes$overview[fieldnotes$overview$project==i,]$category_fr[[1]]
  environment_type <- NA
  dates <- paste0(fieldnotes$overview[fieldnotes$overview$project==i,]$start_date, " - ", fieldnotes$overview[fieldnotes$overview$project==i,]$end_date)
  locations <- NA
  if(length(fieldnotes$overview[fieldnotes$overview$project==i,]$name)>0){
    locations <- ifelse(test = length(fieldnotes$map[fieldnotes$map$project==i,]$name)>1,
                        yes=paste0(fieldnotes$map[fieldnotes$map$project==i,]$name, sep=", ", collapse = T),
                        no=fieldnotes$map[fieldnotes$map$project==i,]$name)
  }
  vessels <- NA
  if(length(fieldnotes$overview[fieldnotes$overview$project==i,]$vessel[[1]])>0){
    vessels <- ifelse(test = length(fieldnotes$overview[fieldnotes$overview$project==i,]$vessel[[1]])>1,
                      yes=paste0(fieldnotes$overview[fieldnotes$overview$project==i,]$vessel[[1]], sep=", ", collapse = T),
                      no=fieldnotes$overview[fieldnotes$overview$project==i,]$vessel[[1]])
  }
  objectives_en <- fieldnotes$overview[fieldnotes$overview$project==i,]$objectives_en[[1]]
  objectives_fr <- fieldnotes$overview[fieldnotes$overview$project==i,]$objectives_fr[[1]]
  start_year <- fieldnotes$overview[fieldnotes$overview$project==i,]$start_year[[1]]
  frequency_en <- fieldnotes$overview[fieldnotes$overview$project==i,]$frequency_en[[1]]
  frequency_fr <- fieldnotes$overview[fieldnotes$overview$project==i,]$frequency_fr[[1]]
  email <- fieldnotes$contact[fieldnotes$contact$project==i,]$email[1]
  phone <- fieldnotes$contact[fieldnotes$contact$project==i,]$phone[1]
  email2 <- NA
  phone2 <- NA
  if(length(fieldnotes$contact[fieldnotes$contact$project==i,])>1){
    email2 <- fieldnotes$contact[fieldnotes$contact$project==i,]$email[2]
    phone2 <- fieldnotes$contact[fieldnotes$contact$project==i,]$email[2]
  }

  #### NEED TO REPLICATE THESE FOR type_fr and url_fr. Sorry, I ran out of time!
  first_nations <- NA
  if(length(fieldnotes$collaborators[fieldnotes$collaborators$project==i &
                                     fieldnotes$collaborators$type_en=="Indigenous Organizations",]$type_en) > 0) {
    if(length(fieldnotes$collaborators[fieldnotes$collaborators$project==i &
                                       fieldnotes$collaborators$type=="Indigenous Organizations",]$org_en) > 1) {
      first_nations <- paste0(fieldnotes$collaborators[fieldnotes$collaborators$project==i &
                                                         fieldnotes$collaborators$type=="Indigenous Organizations",]$org_en, ", ", collapse=T)
    }
    if(length(fieldnotes$collaborators[fieldnotes$collaborators$project==i &
                                       fieldnotes$collaborators$type=="Indigenous Organizations",]$org_en) == 1) {
      first_nations <- fieldnotes$collaborators[fieldnotes$collaborators$project==i &
                                                  fieldnotes$collaborators$type=="Indigenous Organizations",]$org_en
    }
  }
  gov <- NA
  if(length(fieldnotes$collaborators[fieldnotes$collaborators$project==i &
                                     fieldnotes$collaborators$type_en=="Government",]$type_en) > 0) {
    if(length(fieldnotes$collaborators[fieldnotes$collaborators$project==i &
                                       fieldnotes$collaborators$type=="Government",]$org_en) > 1) {
      gov <- paste0(fieldnotes$collaborators[fieldnotes$collaborators$project==i &
                                               fieldnotes$collaborators$type=="Government",]$org_en, ", ", collapse=T)
    }
    if(length(fieldnotes$collaborators[fieldnotes$collaborators$project==i &
                                       fieldnotes$collaborators$type=="Government",]$org_en) == 1) {
      gov <- fieldnotes$collaborators[fieldnotes$collaborators$project==i &
                                        fieldnotes$collaborators$type=="Government",]$org_en
    }
  }
  academic <- NA
  if(length(fieldnotes$collaborators[fieldnotes$collaborators$project==i &
                                     fieldnotes$collaborators$type_en=="Academic Institutions",]$type_en) > 0) {
    if(length(fieldnotes$collaborators[fieldnotes$collaborators$project==i &
                                       fieldnotes$collaborators$type=="Academic Institutions",]$org_en) > 1) {
      academic <- paste0(fieldnotes$collaborators[fieldnotes$collaborators$project==i &
                                                    fieldnotes$collaborators$type=="Academic Institutions",]$org_en, ", ", collapse=T)
    }
    if(length(fieldnotes$collaborators[fieldnotes$collaborators$project==i &
                                       fieldnotes$collaborators$type=="Academic Institutions",]$org_en) == 1) {
      academic <- fieldnotes$collaborators[fieldnotes$collaborators$project==i &
                                             fieldnotes$collaborators$type=="Academic Institutions",]$org_en
    }
  }
  industry <- NA
  if(length(fieldnotes$collaborators[fieldnotes$collaborators$project==i &
                                     fieldnotes$collaborators$type_en=="Business & Industry Associations",]$type_en) > 0) {
    if(length(fieldnotes$collaborators[fieldnotes$collaborators$project==i &
                                       fieldnotes$collaborators$type=="Business & Industry Associations",]$org_en) > 1) {
      industry <- paste0(fieldnotes$collaborators[fieldnotes$collaborators$project==i &
                                                    fieldnotes$collaborators$type=="Business & Industry Associations",]$org_en, ", ", collapse=T)
    }
    if(length(fieldnotes$collaborators[fieldnotes$collaborators$project==i &
                                       fieldnotes$collaborators$type=="Business & Industry Associations",]$org_en) == 1) {
      industry <- fieldnotes$collaborators[fieldnotes$collaborators$project==i &
                                             fieldnotes$collaborators$type=="Business & Industry Associations",]$org_en
    }
  }
  stewardship <- NA
  if(length(fieldnotes$collaborators[fieldnotes$collaborators$project==i &
                                     fieldnotes$collaborators$type_en=="Stewardship Organizations & Research Institutions",]$type_en) > 0) {
    if(length(fieldnotes$collaborators[fieldnotes$collaborators$project==i &
                                       fieldnotes$collaborators$type_en=="Stewardship Organizations & Research Institutions",]$type_en) > 1) {
      stewardship <- paste0(fieldnotes$collaborators[fieldnotes$collaborators$project==i &
                                                       fieldnotes$collaborators$type_en=="Stewardship Organizations & Research Institutions",]$org_en, ", ", collapse=T)
    }
    if(length(fieldnotes$collaborators[fieldnotes$collaborators$project==i &
                                       fieldnotes$collaborators$type_en=="Stewardship Organizations & Research Institutions",]$type_en) == 1) {
      stewardship <- fieldnotes$collaborators[fieldnotes$collaborators$project==i &
                                                fieldnotes$collaborators$type_en=="Stewardship Organizations & Research Institutions",]$type_en
    }
  }

  open_data_link <- NA
  if(length(fieldnotes$links[fieldnotes$links$project==i &
                             fieldnotes$links$type=="Open Data product",]$url_en) > 0) {
    if(length(fieldnotes$links[fieldnotes$links$project==i &
                               fieldnotes$links$type=="Open Data product",]$url_en) > 1) {
      open_data_link <- paste0(fieldnotes$links[fieldnotes$links$project==i &
                                                  fieldnotes$links$type=="Open Data product",]$url_en, ", ", collapse=T)
    }
    if(length(fieldnotes$links[fieldnotes$links$project==i &
                               fieldnotes$links$type=="Open Data product",]$url_en) == 1) {
      open_data_link <- fieldnotes$links[fieldnotes$links$project==i &
                                           fieldnotes$links$type=="Open Data product",]$url_en
    }
  }

  more_info_title <- fieldnotes$links[fieldnotes$links$project==i &
                                        !fieldnotes$links$type=="Open Data product",]$title_en[1]
  more_info_link <- fieldnotes$links[fieldnotes$links$project==i &
                                       !fieldnotes$links$type=="Open Data product",]$url_en[1]
  more_info_title2 <- NA
  more_info_link2 <- NA
  if(length(fieldnotes$links[fieldnotes$links$project==i &
                             !fieldnotes$links$type=="Open Data product",])>1){
    more_info_title2 <- fieldnotes$links[fieldnotes$links$project==i &
                                           !fieldnotes$links$type=="Open Data product",]$title_en[2]
    more_info_link2 <- fieldnotes$links[fieldnotes$links$project==i &
                                          !fieldnotes$links$type=="Open Data product",]$url_en[2]
  }

  # Need to add link to PDF summary somehow?

  df <- as.data.frame(cbind(project, unique_id, title_en, title_fr,
                   subtitle_en, subtitle_fr,
                   category_en, category_fr, environment_type,
                   dates, locations, vessels,
                   objectives_en, objectives_fr,
                   start_year, frequency_en, frequency_fr,
                   email, phone, email2, phone2,
                   first_nations, gov, academic, industry, stewardship,
                   open_data_link,
                   more_info_title, more_info_link, more_info_title2, more_info_link2))

  # I wasn't able to figure out how to get the attributes attached to the geodatabase and to actually export properly to ArcPro.
  if(!is.null(attributes)) attributes <- full_join(attributes, df)
  if(is.null(attributes)) attributes <- df
  geodatabase <- rbind(geodatabase, gridded)
}

attributes$project <- as.numeric(attributes$project)
geodatabase <- left_join(geodatabase,attributes)

# export to GDB
# requires an ArcGIS license
# https://developers.arcgis.com/r-bridge/installation/
#install.packages("arcgis")
# install.packages(
#   "arcgisbinding",
#   repos = "https://r.esri.com",
#   type = "win.binary"
# )
library(arcgisbinding)
arc.check_product()
#ArcPro app must be CLOSED in order for this to work
arc.write("raw_data/data.gdb/fn", geodatabase, overwrite = T)
# must open with arcPro


# export to txt for translation
write.csv(attributes, file = "raw_data/attribute_table.csv")

