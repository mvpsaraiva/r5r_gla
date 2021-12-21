# Download Open Street Map road network data from Geofabrik

gla_pbf_url <- "https://download.geofabrik.de/europe/great-britain/england/greater-london-latest.osm.pbf"
download.file(gla_pbf_url, destfile = here::here("data/greater-london-latest.osm.pbf"))

gla_gtfs_url <- "https://data.bus-data.dft.gov.uk/timetable/download/gtfs-file/london/"
download.file(gla_pbf_url, destfile = here::here("data/itm_london_gtfs.zip"))
