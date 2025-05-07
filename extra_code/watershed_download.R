###function to find watershed shapefiles given point
###perhaps using graphical interaction with a plot of watershed boundaries?
###First: function to download list of watershed boundaries, and display a list of watersheds, perhaps using province to filter.
###select watershed number to create a directory and then download the shapefiles.

watershed_download <- function()
{
  options(timeout=100)
  # if (require(sf)==FALSE){install.packages("sf")}
  require(sf)
  require(utils)
  require(xfun)
  # if (dir.exists("temp_zip") == F) {
  # site <- "https://ftp.maps.canada.ca/pub/nrcan_rncan/vector/geobase_nhn_rhn/index/NHN_INDEX_WORKUNIT_LIMIT_2.zip"
  # temp <- tempfile()
  # utils::download.file(site, temp)
  # utils::unzip(zipfile=temp, exdir="temp_zip") }
  test <- sf::st_read("./raw_data/NHN_INDEX_22_INDEX_WORKUNIT_LIMIT_2.shp")

  # ###get shapefiles for the provinces
  # if (dir.exists("canada_zip") == F) {
  # site <- "https://www12.statcan.gc.ca/census-recensement/2011/geo/bound-limit/files-fichiers/2016/lpr_000b16a_e.zip"
  # temp <- tempfile()
  # download.file(site, temp)
  # unzip(zipfile=temp, exdir="canada_zip") }
  # canada <- read_sf("canada_zip/lpr_000b16a_e.shp")


    oceans <- data.frame(Selection=1:length(unique(test$OCEAN[!is.na(test$OCEAN)])), Ocean=unique(test$OCEAN[!is.na(test$OCEAN)]))
    print("Select the geographic region below...")
    print(oceans)
    ocean.select <- readline(prompt = "Enter Selection: ")
    ocean.select <- oceans$Ocean[oceans$Selection==ocean.select]
    test <- test[test$OCEAN==ocean.select,]

    #now to regional drainage
    regions <- data.frame(Selection=0:length(unique(test$WSCMDANAME[!is.na(test$WSCMDANAME)])), Region=c("all", unique(test$WSCMDANAME[!is.na(test$WSCMDANAME)])))
    print("Select the geographic region below...")
    print(regions)
    region.select <- readline(prompt = "Enter Selection: ")
    if(region.select>0) {
      region.select <- regions$Region[regions$Selection==region.select]
      test <- test[test$WSCMDANAME==region.select,]
    }

    #now to subregional drainages
    regions <- data.frame(Selection=0:length(unique(test$WSCSDANAME[!is.na(test$WSCSDANAME)])), Region=c("all", unique(test$WSCSDANAME[!is.na(test$WSCSDANAME)])))
    print("Select the geographic region below...")
    print(regions)
    region.select <- readline(prompt = "Enter Selection: ")
    if(region.select>0) {
      region.select <- regions$Region[regions$Selection==region.select]
      test <- test[test$WSCSDANAME==region.select,]
    }

    #now to watersheds
    regions <- data.frame(Selection=0:length(unique(test$WSCSSDANAM[!is.na(test$WSCSSDANAM)])), Region=c("all", unique(test$WSCSSDANAM[!is.na(test$WSCSSDANAM)])))
    print("Select the river below...")
    print(regions)
    region.select <- readline(prompt = "Enter Selection: ")
    if(region.select>0) {
      region.select <- regions$Region[regions$Selection==region.select]
      test <- test[test$WSCSSDANAM==region.select,]
    } else{
      region.select <- regions$Region[-1]
    }

    return(test[test$WSCSSDANAM %in% region.select & !is.na(test$WSCSSDANAM),])

 }
#watershed_download()
