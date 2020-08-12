cat("extract data for analysis")

library(tidyverse)

data_raw_dir <- "data-raw"
data_dir <- "data"

# Functions ===================================================================

read_raw <- function(name, col_types = cols()) {
  read_csv(file.path(data_raw_dir, paste0(name, ".csv")), col_types = col_types)
}

save_csv <- function(data, name) {
  write_csv(data, file.path(data_dir, paste0(name, ".csv")))
}

# Script ======================================================================

test <- read_raw("test") %>%
  select(id = ID, cluster = cluster_ID, lat = Latitude, long = Longitude)

save_csv(test, "indiv")

cluster <- test %>%
  group_by(cluster) %>%
  summarise(
    lat = unique(lat), long = unique(long), n_indiv = length(unique(id)),
    .groups = "drop"
  )

connections <- test %>%
  group_by(id) %>%
  mutate(n_conn = length(unique(cluster)) - 1) %>%
  group_by(cluster) %>%
  summarise(n_conn = sum(n_conn), .groups = "drop")

cluster_full <- inner_join(cluster, connections, by = "cluster")

save_csv(cluster_full, "cluster")
