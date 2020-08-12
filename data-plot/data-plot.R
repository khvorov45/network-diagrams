cat("plot the data")

library(tidyverse)

data_dir <- "data"
data_plot_dir <- "data-plot"

# Functions ===================================================================

read_data <- function(name) {
  read_csv(file.path(data_dir, paste0(name, ".csv")), col_types = cols())
}

remap_range <- function(old_vec, new_min, new_max) {
  (old_vec - min(old_vec)) *
    ((new_max - new_min) / (max(old_vec) - min(old_vec))) +
    new_min
}

# Script ======================================================================

indiv <- read_data("indiv")
cluster <- read_data("cluster")

conns <- indiv %>%
  group_by(id) %>%
  filter(length(unique(cluster)) > 1) %>%
  select(id, cluster) %>%
  inner_join(cluster, by = "cluster")

cluster %>%
  ggplot(aes(long, lat)) +
  ggdark::dark_theme_bw(verbose = FALSE) +
  scale_alpha_continuous("Connections", range = c(0.2, 1)) +
  scale_size_continuous("Individuals") +
  geom_point(
    aes(size = n_indiv, stroke = remap_range(n_conn, 0.2, 3)),
    shape = 1
  ) +
  geom_path(
    data = conns, mapping = aes(group = "id", alpha = n_conn)
  )

victoria_map <- geojsonsf::geojson_sf(
  "https://data.gov.au/geoserver/vic-suburb-locality-boundaries-psma-administrative-boundaries/wfs?request=GetFeature&typeName=ckan_af33dd8c_0534_4e18_9245_fc64440f742e&outputFormat=json"
)

victoria_map %>%
  ggplot() +
  theme_bw() +
  theme(panel.grid = element_blank()) +
  geom_sf(size = 0.1, col = "gray70", fill = "gray95") +
  coord_sf(xlim = c(143.5, 147), ylim = c(-39, -36)) +
  scale_alpha_continuous("Connections", range = c(0.2, 1)) +
  scale_size_continuous("Individuals") +
  xlab("Longitude") +
  ylab("Lattitude") +
  geom_point(
    data = cluster,
    mapping = aes(
      x = long, y = lat, size = n_indiv, stroke = remap_range(n_conn, 0.2, 3)
    ),
    shape = 1
  ) +
  geom_path(
    data = conns, mapping = aes(x = long, y = lat, group = "id", alpha = n_conn)
  )
