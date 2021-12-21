options(java.parameters = "-Xmx6G")

# utils::remove.packages('r5r')
# devtools::install_github("ipeaGIT/r5r", subdir = "r-package", ref = "dev")


library("r5r")
library("tidyverse")
library("tictoc")
library("mapview")
library("sf")
library("data.table")

od_zones <- st_read(here::here("data", "od_zones_gla.gpkg"))
od_zones %>% mapview()

od_points_sf <- st_read(here::here("data", "od_points_gla.gpkg"))
od_points_sf %>% mapview()


# r5r_core <- setup_r5(system.file("extdata", package = "r5r"), verbose = FALSE)
tic()
r5r_core <- setup_r5(here::here("data"), verbose = TRUE)
toc()

transit <- transit_network_to_sf(r5r_core)

transit$routes %>% 
  mapview(zcol = "mode")

access_df <- accessibility(r5r_core,
                           origins = od_points_sf,
                           destinations = od_points_sf,
                           mode = c("WALK", "TRANSIT"),
                           opportunities_colname = "jobs",
                           departure_datetime = as.POSIXct("14-12-2021 09:00:00", format = "%d-%m-%Y %H:%M:%S"),
                           cutoffs = c(30, 45),
                           verbose = FALSE)

access_df %>%
  left_join(od_zones, by = c("from_id"="code")) %>%
  arrange(accessibility) %>%
  ggplot(aes(fill=accessibility)) +
  geom_sf(aes(geometry=geom), color=NA) +
  scale_fill_distiller(palette = "Spectral") +
  facet_wrap(~cutoff)

access_df %>%
  left_join(od_zones, by = c("from_id"="code")) %>%
  filter(cutoff == 30) %>%
  st_as_sf() %>%
  mapview(zcol="accessibility")

