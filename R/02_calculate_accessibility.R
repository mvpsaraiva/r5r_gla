options(java.parameters = "-Xmx16G")

# utils::remove.packages('r5r')
# devtools::install_github("ipeaGIT/r5r", subdir = "r-package", ref = "dev")

library("r5r")
library("tidyverse")
library("tictoc")
library("mapview")
library("sf")
library("data.table")

od_zones <- st_read(here::here("data_gla", "od_zones_gla.gpkg"))
od_zones %>% mapview()

od_points_sf <- st_read(here::here("data_gla", "od_points_gla.gpkg"))
od_points_sf %>% mapview(zcol="jobs")


# r5r_core <- setup_r5(system.file("extdata", package = "r5r"), verbose = FALSE)
tic()
r5r_core <- setup_r5(here::here("data_gla"), verbose = TRUE)
toc()

transit <- transit_network_to_sf(r5r_core)

transit$routes %>% 
  mapview(zcol = "mode")

access_walk_df <- accessibility(r5r_core,
                                   origins = od_points_sf,
                                   destinations = od_points_sf,
                                   mode = c("WALK"),
                                   opportunities_colname = "jobs",
                                   departure_datetime = as.POSIXct("02-03-2022 09:00:00", format = "%d-%m-%Y %H:%M:%S"),
                                   cutoffs = c(15, 30, 60, 90),
                                   verbose = FALSE)

access_bike_df <- accessibility(r5r_core,
                                origins = od_points_sf,
                                destinations = od_points_sf,
                                mode = c("BICYCLE"),
                                opportunities_colname = "jobs",
                                departure_datetime = as.POSIXct("02-03-2022 09:00:00", format = "%d-%m-%Y %H:%M:%S"),
                                cutoffs = c(15, 30, 60, 90),
                                verbose = FALSE)

access_bus_df <- accessibility(r5r_core,
                                   origins = od_points_sf,
                                   destinations = od_points_sf,
                                   mode = c("WALK", "BUS"),
                                   opportunities_colname = "jobs",
                                   departure_datetime = as.POSIXct("02-03-2022 09:00:00", format = "%d-%m-%Y %H:%M:%S"),
                                   cutoffs = c(15, 30, 60, 90),
                                   verbose = FALSE)

access_transit_df <- accessibility(r5r_core,
                           origins = od_points_sf,
                           destinations = od_points_sf,
                           mode = c("WALK", "TRANSIT"),
                           opportunities_colname = "jobs",
                           departure_datetime = as.POSIXct("02-03-2022 09:00:00", format = "%d-%m-%Y %H:%M:%S"),
                           cutoffs = c(15, 30, 60, 90),
                           verbose = FALSE)

access_walk_df$mode <- "walk"
access_bike_df$mode <- "bike"
access_bus_df$mode <- "bus"
access_transit_df$mode <- "transit"

access_df <- rbind(access_walk_df, access_bike_df, access_bus_df, access_transit_df)

access_wide_df <- access_df %>%
  select(-percentile) %>%
  pivot_wider(names_from = c(mode, cutoff), values_from = accessibility)


access_df %>%
  left_join(od_zones, by = c("from_id"="code")) %>%
  arrange(accessibility) %>%
  ggplot(aes(fill=accessibility)) +
  geom_sf(aes(geometry=geom), color=NA) +
  scale_fill_distiller(palette = "Spectral") +
  facet_grid(mode~cutoff)

access_sf <- left_join(od_zones, access_wide_df, by = c("code"="from_id")) 

st_write(access_sf, here::here("output", "gla_accessibility.shp"))

# Map

map(unique(access_df$mode), function(m) {
  map(unique(access_df$cutoff), function(c, m) {
    p <- access_df %>%
      filter(mode == m, cutoff == c) %>%
      left_join(od_zones, by = c("from_id" = "code")) %>%
      ggplot(aes(fill=accessibility/1000)) +
      geom_sf(aes(geometry=geom), size=0.2) +
      coord_sf(datum=NA) +
      scale_fill_distiller(palette = "Spectral") +
      labs(fill = "Accessibility\nJobs (x1000)") +
      theme_light() +
      theme(legend.position = "right") +
      labs(title = "Accessibility to Jobs in the GLA",
           subtitle = paste0("mode = ", m, ", travel time cutoff = ", c, " minutes"))
    
    ggsave(plot = p, filename = here::here("output/maps", paste0("map_gla_", m, "_", c, "min.png")), 
           dpi = 300, width = 20, height = 15, units = "cm")
  }, m )
})



access_sf %>%
  ggplot(aes(fill=accessibility/1000)) +
  geom_sf(aes(geometry=geom), size=0.2) +
  coord_sf(datum=NA) +
  scale_fill_distiller(palette = "Spectral") +
  labs(fill = "Accessibility\nJobs (x1000)") +
  theme_light() +
  theme(legend.position = "bottom") +
  facet_wrap(~cutoff, labeller = labeller(cutoff = function(s) return(paste(s, "minutes"))))


