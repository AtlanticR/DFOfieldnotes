#' the run function
#'
#' This function allows you to run the report
#' @param fiscalyear "2024-2025" (default)
#' @param yourpath Location of data downloaded from PPT (default is working directory)
#' @param yourregion "Maritimes" (default)
#' @param token Cookie used by dataSPA to access PPT
#' @param projectids NULL (default), but can list as: c(2615, 941) etc.
#' @param surveypath "C:/Users/keyserf/OneDrive - DFO-MPO/ARtS (MAR) Internal Files/Field Notes/DFO Maritimes Region Fieldnotes - Project Contribution Form.xlsx" is default
#'
#' @return creates PDF document, default file name is output.pdf
#' @export output.pdf
#'
#' @examples
#'
#' run_report(projectids=2615)
#' run_report(projectids=C(941, 2615))

run_report <- function(fiscalyear = "2024-2025",
                      yourpath = "C:/Users/keyserf/Documents/temp_data",
                      yourregion = "Maritimes",
                      token = "csrftoken=YOURCOOKIE; sessionid=YOURSESSIONID",
                      projectids = NULL,
                      surveypath = "C:/Users/keyserf/OneDrive - DFO-MPO/ARtS (MAR) Internal Files/Field Notes/DFO Maritimes Region Fieldnotes - Project Contribution Form.xlsx")
{
  require(xfun)
  require(rmarkdown)
  require(ggplot2)
  require(tidyr)
  require(dplyr)
  require(rnaturalearth)
  require(rnaturalearthdata)
  require(rnaturalearthhires)
  require(patchwork)
  require(sf)
  require(png)
  require(stringr)
  require(dataSPA)
  require(readxl)
  require(tidyverse)
  #first time
  # om <- getData(type="om", cookie=freya, age=1000, keep=T, path = "C:/Users/keyserf/Documents/temp_data/") #Define your own cookie using the vignette help
  # sal <- getData(type="salary", cookie=freya, age=1000, keep=T, path = "C:/Users/keyserf/Documents/temp_data/") #Define your own cookie using the vignette help

  #after
  om <- getData(type="om", cookie=token, age=1000, keep=F, path = yourpath) #Define your own cookie using the vignette help
  #sal <- getData(type="salary", cookie=token, age=1000, keep=F, path = yourpath) #Define your own cookie using the vignette help

  OM <- subsetSPA(om=om, year=fiscalyear, region=yourregion)
  #SAL <- subsetSPA(salary=sal, year=fiscalyear, region=yourregion)

  # OM2 <- subsetSPA(om=om, year="2024-2025", region="Gulf")
  # SAL2 <- subsetSPA(salary=sal, year="2024-2025", region="Gulf")

  # OM3 <- subsetSPA(om=om, year="2024-2025", region="Pacific")
  # SAL3 <- subsetSPA(salary=sal, year="2024-2025", region="Pacific")

  # OM <- rbind(OM, OM2, OM3)
  # SAL <- rbind(SAL, SAL2, SAL3)

  # Query 1
  om1 <- OM[which(OM$category_display == "Field"),]
  field_ids <- om1$project_id
  field_title <- om1$project_title

  field <- data.frame(id=field_ids, title=unlist(field_title))

  # Query 2
  om2 <- OM[which(OM$category_display == "Vessels, Boats"),]

  # Field and Ship
  field_ship_ids <- unique(unique(om1$project_id), unique(om2$project_id))
  om3 <- OM[which(OM$project_id %in% field_ship_ids),]

  projects <- dplyr::select(om3, section_display, functional_group, activity_type, theme, project_title, project_id, overview, lead_staff) %>% dplyr::distinct() %>% dplyr::arrange(section_display, project_id, project_title)

  region <- stringr::str_split(projects$section_display, pattern = " - ")
  projects$region <- unlist(lapply(region, function(x) x[1]))

  division <- stringr::str_split(projects$section_display, pattern = " - ")
  projects$division <- unlist(lapply(division, function(x) x[3]))

  projects <- dplyr::arrange(projects, region, division, project_title, project_id)

  uniquepeople <- stringr::str_split(projects$lead_staff, ",")
  uniquepeople <- purrr::map(uniquepeople, function(x) stringr::str_flatten_comma(unique(stringr::str_trim(x))))
  projects$lead_staff <- uniquepeople

  #projects <- projects[1:10,]
  if(is.null(projectids)) projectids <- unique(projects$project_id)
  project_id <- projectids

  projects <- projects[projects$project_id %in% project_id,]

  ### run scallop example ###
  #source("project941_example.R")

  #sharepoint responses
  responses <- read_excel(surveypath)
  responses <- dplyr::select(responses, 7:25)
  names(responses)[which(grepl(x = names(responses), pattern = "ID"))] <- "project_id"
  names(responses)[which(grepl(x = names(responses), pattern = "summary"))] <- "summary"
  names(responses)[which(grepl(x = names(responses), pattern = "approval"))] <- "approval"
  names(responses)[which(grepl(x = names(responses), pattern = "photos"))] <- "photos"
  names(responses)[which(grepl(x = names(responses), pattern = "location"))] <- paste0("location", 0:3)
  names(responses)[which(grepl(x = names(responses), pattern = "polygon names"))] <- "polygon"
  names(responses)[which(grepl(x = names(responses), pattern = "Sharepoint"))] <- "polylink"
  names(responses)[which(grepl(x = names(responses), pattern = "freq"))] <- "freq"
  names(responses)[which(grepl(x = names(responses), pattern = "start date"))] <- "startdate"
  names(responses)[which(grepl(x = names(responses), pattern = "end date"))] <- "enddate"
  names(responses)[which(grepl(x = names(responses), pattern = "Open Data"))] <- "data"
  names(responses)[which(grepl(x = names(responses), pattern = "CSAS"))] <- "csas"
  names(responses)[which(grepl(x = names(responses), pattern = "technical report"))] <- "techreport"
  names(responses)[which(grepl(x = names(responses), pattern = "phone"))] <- "phone"
  names(responses)[which(grepl(x = names(responses), pattern = "email"))] <- "email"
  names(responses)[which(grepl(x = names(responses), pattern = "primary contact"))] <- "contact"
  names(responses)[which(grepl(x = names(responses), pattern = "title"))] <- "title"

  filename <- str_locate(string = responses$photos, pattern="you/")
  responses$photos <- str_sub(string=responses$photos, start=(filename[,2]+1), end=nchar(responses$photos))
  responses$photos <- gsub(x = responses$photos, pattern="%20", replacement=" ", fixed=T)
  responses$photos <- paste0("/images/Sharepoint photos/", responses$photos)

  responses$project_id <- as.numeric(responses$project_id)
  projects <- left_join(projects, responses)

  #projects
  #######################################################

  # map for plotting

  land.all <- ne_countries(scale = "large", returnclass = "sf",continent = "North America")
  land.all <- st_transform(land.all, 32619)

  # UPDATE THIS TO PULL FROM PPT
  # NO! This is no longer necessary since David F can make PDFs in the Project Planning Tool!

  #get polys
  # source("spatial_selector.R")
  # shp contains watersheds
  # units contains NAFOs
  # shp$label <- shp$WSCSSDANAM
  # units$label <- units$UnitArea
  # spatial <- rbind(select(shp, label, geometry), select(units, label, geometry))
  # spatial$label2 <- tolower(gsub(x=spatial$label, pattern=" ", replacement=""))


  for(i in 1:length(projects$project_id)){
    project <- projects[projects$project_id==projects$project_id[i],]

    # replace location DF with spatial info

    # if(!is.na(project$location1)) location <- data.frame(x=str_split(string = project$location1, pattern=",")[[1]][1],
    #                                                      y=str_split(string = project$location1, pattern=",")[[1]][2],
    #                                                      label=str_split(string = project$location1, pattern=",")[[1]][3])
    # if(!is.na(project$location2)) location <- rbind(location,
    #                                                 data.frame(x=str_split(string = project$location2, pattern=",")[[1]][1],
    #                                                            y=str_split(string = project$location2, pattern=",")[[1]][2],
    #                                                            label=str_split(string = project$location2, pattern=",")[[1]][3]))
    # if(!is.na(project$location3)) location <- rbind(location,
    #                                                 data.frame(x=str_split(string = project$location3, pattern=",")[[1]][1],
    #                                                      y=str_split(string = project$location3, pattern=",")[[1]][2],
    #                                                      label=str_split(string = project$location3, pattern=",")[[1]][3]))
    # if(is.na(project$location1)) location <- data.frame(x=-66,
    #                                                     y=45,
    #                                                     label="placeholder")

    # if(!is.na(project$polygon)){
    #   polys <- tolower(gsub(x=str_split(string=project$polygon, pattern=",")[[1]], pattern=" ", replacement = ""))
    #   polys <- spatial[spatial$label2 %in% polys,] %>% st_transform(32619)
    #
    #   bound <- st_as_sfc(st_bbox(st_buffer(st_as_sfc(st_bbox(polys)), 500000)))
    #   zoom <- st_crop(land.all, bound)
    #
    #   map <- ggplot() + geom_sf(data=zoom) +
    #     geom_sf(data=bound, fill=NA, colour=NA)+
    #     geom_sf(data=polys, size=5, fill="forestgreen") +
    #     geom_sf_label(data=polys, aes(label=label), hjust=-0.2)+
    #     coord_sf(expand=F) +
    #     theme_bw() +
    #     xlab(NULL)+
    #     ylab(NULL)
    # }
    if(is.na(project$polygon)){
      spatial <- st_as_sfc("POINT(-65 46)", crs=4326) %>% st_transform(32619)
    }
    if(!is.na(project$location1)) {
      spatial <- st_as_sfc(project$location1, crs=4326) %>% st_transform(32619)
    }
    if(!is.na(project$location2)) {
      spatial2 <- st_as_sfc(project$location2, crs=4326) %>% st_transform(32619)
    }
    if(is.na(project$location2)) spatial2 <- NULL
    bound <- st_as_sfc(st_bbox(st_buffer(st_as_sfc(st_bbox(spatial)), 250000)))
    zoom <- st_crop(land.all, bound)

    map <- ggplot() + geom_sf(data=zoom) +
      geom_sf(data=bound, fill=NA, colour=NA)+
      geom_sf(data=spatial, fill="red", colour="red", alpha=0.5) +
      geom_sf(data=spatial2, fill="red", colour="red", alpha=0.5) +
      coord_sf(expand=F) +
      theme_bw() +
      xlab(NULL)+
      ylab(NULL)
    #}

    if(!file.exists(paste0(yourpath, "/images/", projects$project_id[i]))) dir.create(path = paste0(yourpath, "/images/",projects$project_id[i]))

    png(paste0(yourpath, "/images/", projects$project_id[i], "/map.png"), height=8, width=10, units = "cm", res=400)
    print(map)
    dev.off()
  }

  # Store image properly
  # currently just storing as

  for(i in 1:length(projects$project_id)){

    project <- projects[projects$project_id==projects$project_id[i],]

    if(!file.exists(paste0(yourpath, "/images/", projects$project_id[i]))) dir.create(path = paste0(yourpath, "/images/",projects$project_id[i]))

    if(!is.na(project$photos)) {
      # Read the PNG image
      img <- readPNG(paste0(yourpath, project$photos))
      # Save the image with a different name
      writePNG(img, paste0(yourpath, "/images/", projects$project_id[i], "/img1.png"))
    }

    if(is.na(project$photos) | !file.exists(paste0(yourpath, "/images/", projects$project_id[i], "/img1.png"))) {
      # Read the PNG image
      img <- readPNG(yourpath, "/images/img1.png")
      # Save the image with a different name
      writePNG(img, paste0(yourpath, "/images/", projects$project_id[i], "/img1.png"))
    }
  }

  ######################################################
  #### build the book!
  # Modify render_markdowns.R based on the banks/survey you desire. Also make sure the Rdata file in parameterised_report.Rmd is right.
  render_markdowns(ids=projectids)
  ########################################################
  #1 minute to run all projects
}

