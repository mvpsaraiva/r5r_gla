options(java.parameters = "-Xmx6G")

# utils::remove.packages('r5r')
# devtools::install_github("ipeaGIT/r5r", subdir = "r-package", ref = "dev")


library("r5r")
library("tidyverse")
library("tictoc")
library("mapview")
library("sf")


od_zones <- st_read(here::here("data/gla/inputs", "od_zones_sg.shp"))
od_zones %>% mapview()

od_codes <- unique(od_zones$code)

od_df <- read_csv(here::here("data/gla/inputs", "wu03ew_v2_transport.csv")) %>%
  janitor::clean_names() %>%
  filter(area_of_workplace %in% od_codes)

head(od_df)

jobs_df <- od_df %>%
  group_by(area_of_workplace) %>%
  summarise(jobs = sum(all_categories_method_of_travel_to_work), .groups = "drop")

od_points_sf <- od_zones %>%
  left_join(jobs_df, by=c("code"="area_of_workplace")) %>%
  st_centroid() %>%
  st_transform(4326) %>%
  select(id = code, jobs, geometry)



# r5r_core <- setup_r5(system.file("extdata", package = "r5r"), verbose = FALSE)
tic()
r5r_core <- setup_r5(here::here("data/gla"), verbose = TRUE)
toc()

transit <- transit_network_to_sf(r5r_core)

transit$routes %>% 
  mapview(zcol = "mode")

access_df <- accessibility(r5r_core,
                           origins = od_points_sf,
                           destinations = od_points_sf,
                           mode = c("WALK", "BUS"),
                           opportunities_colname = "jobs",
                           departure_datetime = as.POSIXct("14-12-2021 09:00:00", format = "%d-%m-%Y %H:%M:%S"),
                           cutoffs = c(30, 45),
                           verbose = FALSE)

access_df %>%
  left_join(od_zones, by = c("from_id"="code")) %>%
  arrange(accessibility) %>%
  ggplot(aes(fill=accessibility)) +
  geom_sf(aes(geometry=geometry), color=NA) +
  scale_fill_distiller(palette = "Spectral") +
  facet_wrap(~cutoff)


st_write(od_points_sf, here::here("data/gla/inputs/od_points_gla.gpkg"))
st_write(od_zones, here::here("data/gla/inputs/od_zones_gla.gpkg"))
