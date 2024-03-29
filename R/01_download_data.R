# Download Open Street Map road network data from Geofabrik

gla_pbf_url <- "https://download.geofabrik.de/europe/great-britain/england/greater-london-latest.osm.pbf"
download.file(gla_pbf_url, destfile = here::here("data_gla/greater-london-latest.osm.pbf"))

gla_gtfs_url <- "https://data.bus-data.dft.gov.uk/timetable/download/gtfs-file/london/"
download.file(gla_gtfs_url, destfile = here::here("data_gla/itm_london_gtfs.zip"))
