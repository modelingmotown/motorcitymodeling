# Graphhopper / Geospatial

# java -jar C:/Users/satur/Document5/ghopper/graphhopper-web-11.0.jar server C:/Users/satur/Document5/ghopper/miconfig.yaml

# ""

library(sf)          # Support for simple feature access, encode and analyze spatial vector data
library(httr2)       # Perform HTTP Requests and Process the Responses
library(tidyverse)   # data wrangling & ggplot2
library(janitor)     # functions for examining and cleaning dirty data
library(here)        # Find files within a project
library(tigris)      # Load Census TIGER/Line Shapefiles
library(osmextract)  # Download and Import Open Street Map Data Extracts


graphhopper_url <- "http://localhost:8989"

gh_info <- request(paste0(graphhopper_url, "/info")) |>
  req_perform() |> resp_body_json(simplifyVector = TRUE)

#gh_info$version ; gh_info$profiles ; gh_info$bbox

options(tigris_use_cache = TRUE)

# dir.create(here("geospatial/osm"), showWarnings = FALSE)
# dir.create(here("geospatial/osm/wayne_pois"), showWarnings = FALSE)

# Census county geography
mi_counties <- tigris::counties(
  state = "MI",
  cb = TRUE,
  year = 2025,
  class = "sf"
)

wayne <- mi_counties |> filter(GEOID == "26163") |> st_transform(4326)

# Buffer by 15 km in the Detroit-area projected CRS.
wayne_buffer <- wayne |>
  st_transform(26917) |>
  st_buffer(15000) |>
  st_transform(4326)

st_write(
  wayne,
  here("geospatial/osm", "wayne_county.geojson"),
  delete_dsn = TRUE,
  quiet = TRUE
)

st_write(
  wayne_buffer, here("geospatial/osm", "wayne_county_buffer_15km.geojson"),
  delete_dsn = TRUE, quiet = TRUE
)

poi_tags <- c(
  "amenity",  "healthcare", "leisure", "operator", "brand", "entrance", "emergency", "access",
  "wheelchair", "addr:housenumber", "addr:street", "website", "phone", "highway", "aeroway",
  "natural", "water", "route", "shop", "tourism"
)

osm_points <- osmextract::oe_read(
  file_path = "C:/Users/satur/Document5/ghopper/michigan-260729.osm.pbf",
  layer = "points",
  boundary = wayne_buffer,
  boundary_type = "spat",
  extra_tags = poi_tags,
  quiet = FALSE
)

osm_polygons <- osmextract::oe_read(
  file_path = "C:/Users/satur/Document5/ghopper/michigan-260729.osm.pbf",
  layer = "multipolygons", boundary = wayne_buffer, boundary_type = "spat",
  extra_tags = poi_tags, quiet = FALSE
)

water_tags <- c("natural", "water", "waterway", "name")

osm_water_polygons <- osmextract::oe_read(
  file_path = "C:/Users/satur/Document5/ghopper/michigan-260729.osm.pbf",
  layer = "multipolygons", boundary = wayne_buffer, boundary_type = "spat",
  extra_tags = water_tags, quiet = FALSE
)

river_line_tags <- c("waterway", "name")

osm_water_lines <- osmextract::oe_read(
  file_path = "C:/Users/satur/Document5/ghopper/michigan-260729.osm.pbf", layer = "lines",
  boundary = wayne_buffer, boundary_type = "spat", extra_tags = river_line_tags, quiet = FALSE
)

detroit_river_centerline <- osm_water_lines |>
  filter(waterway == "river", name == "Detroit River") # !is.na(name), trimws(name) == "Detroit River"

river_water_candidates <- osm_water_polygons |>
  filter((natural == "water" & water == "river") | waterway == "riverbank") |> st_make_valid()

metric_crs <- 26917

river_centerline_m <- detroit_river_centerline |> st_transform(metric_crs)

river_water_candidates_m <- river_water_candidates |>  st_transform(metric_crs)

detroit_river_corridor <- river_centerline_m |> st_union() |> st_buffer(1500)

detroit_river_water <- st_filter(
  river_water_candidates_m, detroit_river_corridor, .predicate = st_intersects
)

river_candidates_m <- river_water_candidates |> st_transform(metric_crs)

river_centerline_m <- detroit_river_centerline |> st_transform(metric_crs)

detroit_river_water <- st_filter(
  river_candidates_m, river_centerline_m, .predicate = st_intersects
)

detroit_river_water_union <- detroit_river_water |>
  summarise(geometry = st_union(geometry)) |> st_make_valid()

plot(st_geometry(detroit_river_water_union), main = "Detroit River water area")
plot(st_geometry(river_centerline_m), add = TRUE, lwd = 2)

river_boundary <- st_boundary(st_geometry(detroit_river_water_union))
river_boundary <- st_sf(geometry = river_boundary)

wayne_m <- wayne |> st_make_valid() |> st_transform(metric_crs)
wayne_river_shoreline <- st_intersection(river_boundary, st_buffer(wayne_m, 25))
wayne_river_shoreline <- st_cast(wayne_river_shoreline, "LINESTRING", warn = FALSE)
plot(st_geometry(wayne_river_shoreline), main = "Wayne County Detroit River Shoreline")

###

hospital_points <- osm_points |>
  filter(
    amenity == "hospital" |
      healthcare == "hospital"
  ) |>
  mutate(
    feature_type = "hospital",
    source_geometry = "point"
  )

hospital_polygons <- osm_polygons |>
  filter(
    amenity == "hospital" |
      healthcare == "hospital"
  ) |> mutate(feature_type = "hospital", source_geometry = "polygon")
###
park_points <- osm_points |>
  filter(leisure == "park") |> mutate(feature_type = "park", source_geometry = "point")

park_polygons <- osm_polygons |>
  filter(leisure == "park") |> mutate(feature_type = "park", source_geometry = "polygon")
###
highway_points <- osm_points |>
  filter(highway == "motorway_junction" | highway == "motorway_link") |>
  mutate(feature_type = "highway", source_geometry = "point")

highway_polygons <- osm_polygons |>
  filter(highway == "motorway_junction" | highway == "motorway_link") |>
  mutate(feature_type = "highway", source_geometry = "polygon")
###
hospital_points <- hospital_points[
  lengths(st_intersects(hospital_points, wayne_buffer)) > 0,
]

hospital_polygons <- hospital_polygons[
  lengths(st_intersects(hospital_polygons, wayne_buffer)) > 0,
]

park_points <- park_points[lengths(st_intersects(park_points, wayne_buffer)) > 0,]

park_polygons <- park_polygons[lengths(st_intersects(park_polygons, wayne_buffer)) > 0,]

highway_points <- highway_points[lengths(st_intersects(highway_points, wayne_buffer)) > 0,]
highway_polygons <- highway_polygons[lengths(st_intersects(highway_polygons, wayne_buffer)) > 0,]

st_write(
  hospital_points,
  here("geospatial/osm/wayne_pois", "hospital_points.gpkg"),
  layer = "hospital_points",
  delete_dsn = TRUE,
  quiet = TRUE
)

st_write(
  hospital_polygons, here("geospatial/osm/wayne_pois", "hospital_polygons.gpkg"),
  layer = "hospital_polygons", delete_dsn = TRUE, quiet = TRUE
)

st_write(
  park_points, here("geospatial/osm/wayne_pois", "park_points.gpkg"),
  layer = "park_points", delete_dsn = TRUE, quiet = TRUE
)

st_write(
  park_polygons, here("geospatial/osm/wayne_pois", "park_polygons.gpkg"),
  layer = "park_polygons", delete_dsn = TRUE, quiet = TRUE
)

st_write(
  highway_points, here("geospatial/osm/wayne_pois", "highway_points.gpkg"),
  layer = "highway_points", delete_dsn = TRUE, quiet = TRUE
)

st_write(
  highway_polygons, here("geospatial/osm/wayne_pois", "highway_polygons.gpkg"),
  layer = "highway_polygons", delete_dsn = TRUE, quiet = TRUE
)

st_write(
  detroit_river_water_union, here("geospatial/osm/wayne_pois", "river_polygon.gpkg"),
  layer = "detroit_river_water_union", delete_dsn = TRUE, quiet = TRUE
)

st_write(
  wayne_river_shoreline, here("geospatial/osm/wayne_pois", "river_border.gpkg"),
  layer = "wayne_river_shoreline", delete_dsn = TRUE, quiet = TRUE
)

hospital_polygon_labels <- hospital_polygons |> st_point_on_surface()

park_polygon_labels <- park_polygons |> st_point_on_surface()

hospital_labels <- bind_rows(
  hospital_points,
  hospital_polygon_labels
) |> mutate(label = coalesce(na_if(name, ""), na_if(operator, ""), na_if(brand, "")))

park_labels <- bind_rows(
  park_points,
  park_polygon_labels
) |> mutate(label = coalesce(na_if(name, ""), na_if(operator, "")))

highway_labels <- bind_rows(highway_points) |>
  mutate(label = coalesce(na_if(name, ""), na_if(operator, "")))

add_lon_lat <- function(x) {
  x <- st_transform(x, 4326)
  xy <- st_coordinates(x)
  x |> mutate(longitude = xy[, "X"], latitude = xy[, "Y"])
}

hospital_labels <- add_lon_lat(hospital_labels)
park_labels <- add_lon_lat(park_labels)
highway_labels <- add_lon_lat(highway_labels)

hospital_park_highway_labels <- bind_rows(hospital_labels, park_labels, highway_labels) |>
  select(
    feature_type, osm_id, label, name, operator, source_geometry, longitude, latitude, geometry
  )

st_write(
  hospital_park_highway_labels, here("geospatial/osm/wayne_pois", "hospital_park_highway_labels.gpkg"),
  layer = "labels", delete_dsn = TRUE, quiet = TRUE
)

hospital_park_highway_labels |>
  st_drop_geometry() |>
  write.csv(here("geospatial/osm/wayne_pois", "hospital_park_highway_labels.csv"), row.names = FALSE)

###

load(file = here("data/rdata", "cod_2022-2025geo_23July.RData"))
df <- sales_df

df_checked <- df |>
  mutate(
    .row_id = row_number(),
    x_coord = suppressWarnings(as.numeric(as.character(x_coord))),
    y_coord = suppressWarnings(as.numeric(as.character(y_coord))),
    coordinate_valid =
      !is.na(x_coord) & !is.na(y_coord) & is.finite(x_coord) & is.finite(y_coord)
  )

df_checked |> count(coordinate_valid)

parcel_points <- df_checked |> filter(coordinate_valid) |>
  st_as_sf(
    coords = c("x_coord", "y_coord"),
    crs = 4326,
    remove = FALSE
  )

summary(df_checked$x_coord)
summary(df_checked$y_coord)

class(parcel_points)
# "sf" "data.frame"

st_geometry_type(parcel_points)
# POINT

parcel_points <- parcel_points |> dplyr::group_by(parcelno) |>
  dplyr::slice_max(order_by = saledate, n = 1, with_ties = FALSE) |> dplyr::ungroup()

nearest_linear_feature <- function(
    parcels, features, parcel_id, feature_id, feature_name = NULL, metric_crs = 26917
) {
  stopifnot(inherits(parcels, "sf"))
  stopifnot(inherits(features, "sf"))
  
  if (nrow(features) == 0L) {stop("The feature layer contains no rows.")}
  
  parcels_m <- st_transform(parcels, metric_crs)
  features_m <- st_transform(features, metric_crs)
  
  nearest_index <- st_nearest_feature(parcels_m, features_m)
  
  distance_m <- st_distance(parcels_m, features_m[nearest_index, ], by_element = TRUE)
  
  result <- tibble(
    parcel_id = as.character(parcels[[parcel_id]]),
    feature_id = as.character(features[[feature_id]][nearest_index]),
    linear_distance_m = as.numeric(distance_m)
  )
  
  if (!is.null(feature_name)) {
    result$feature_name <-
      as.character(features[[feature_name]][nearest_index])
  } else {result$feature_name <- NA_character_}
  result
}

hospitals <- osm_polygons |>
  filter(
    amenity == "hospital" |
      healthcare == "hospital"
  ) |> st_make_valid() |> filter(!st_is_empty(geometry)) |>
  mutate(hospital_id = paste0("polygon_", osm_id), name = na_if(name, "")) |>
  select(hospital_id, name, geometry)

class(hospitals)
nrow(hospitals)
st_geometry_type(hospitals)
st_crs(hospitals)

parks <- osm_polygons |>
  filter(leisure == "park") |> st_make_valid() |> filter(!st_is_empty(geometry)) |>
  mutate(park_id = paste0("polygon_", osm_id), name = na_if(name, "")) |>
  select(park_id, name, geometry)

highways <- osm_points |>
  filter(highway == "motorway_junction" | highway == "motorway_link") |>
  st_make_valid() |> filter(!st_is_empty(geometry)) |>
  mutate(highway_id = paste0("point_", osm_id), name = na_if(name, "")) |>
  select(highway_id, name, geometry)

hospital_linear <- nearest_linear_feature(
  parcels      = parcel_points,
  features     = hospitals,
  parcel_id    = "parcelno",
  feature_id   = "hospital_id",
  feature_name = "name",
  metric_crs   = metric_crs
) |>
  rename(
    nearest_hospital_linear_id   = feature_id,
    nearest_hospital_linear_name = feature_name,
    hospital_linear_m            = linear_distance_m
  )

park_linear <- nearest_linear_feature(
  parcels = parcel_points, features = parks, parcel_id = "parcelno",
  feature_id = "park_id", feature_name = "name", metric_crs = metric_crs
) |>
  rename(
    nearest_park_linear_id   = feature_id,
    nearest_park_linear_name = feature_name, park_linear_m = linear_distance_m
  )

highway_linear <- nearest_linear_feature(
  parcels = parcel_points, features = highways, parcel_id = "parcelno",
  feature_id = "highway_id", feature_name = "name", metric_crs = metric_crs
) |>
  rename(
    nearest_highway_entrance_linear_id   = feature_id,
    nearest_highway_entrance_linear_name = feature_name, highway_entrance_linear_m = linear_distance_m
  )

hospital_polygons_clean <- osm_polygons |>
  filter(
    amenity == "hospital" |
      healthcare == "hospital"
  ) |> st_make_valid() |> filter(!st_is_empty(geometry)) |>
  mutate(
    hospital_id = paste0("polygon_", osm_id),
    hospital_geometry_type = "polygon"
  ) |> select(hospital_id, name, hospital_geometry_type, geometry)

hospital_points_clean <- osm_points |>
  filter(
    amenity == "hospital" |
      healthcare == "hospital"
  ) |> filter(!st_is_empty(geometry)) |>
  mutate(
    hospital_id = paste0("point_", osm_id),
    hospital_geometry_type = "point"
  ) |> select(hospital_id, name, hospital_geometry_type, geometry)

hospitals <- rbind(hospital_polygons_clean, hospital_points_clean)

###

`%||%` <- function(x, y) {if (is.null(x) || length(x) == 0L) y else x}

graphhopper_route <- function(
    from_lon,
    from_lat,
    to_lon,
    to_lat,
    base_url = "http://localhost:8989",
    profile = "car",
    snap_preventions = c("ferry"),
    timeout_seconds = 60
) {
  route_url <- paste0(sub("/+$", "", base_url), "/route")
  
  body <- list(
    points = list(
      c(as.numeric(from_lon), as.numeric(from_lat)),
      c(as.numeric(to_lon),   as.numeric(to_lat))
    ), profile = profile, instructions = FALSE, calc_points = FALSE, points_encoded = FALSE
  )
  
  if (length(snap_preventions) > 0L) {body$snap_preventions <- unname(snap_preventions)}
  
  response <- request(route_url) |> req_method("POST") |>
    req_body_json(body, auto_unbox = TRUE) |> req_timeout(timeout_seconds) |>
    req_retry(max_tries = 3) |> req_perform()
  
  result <- resp_body_json(response, simplifyVector = FALSE)
  
  if (is.null(result$paths) || length(result$paths) == 0L) {
    stop(result$message %||% "GraphHopper returned no route.")
  }
  
  path <- result$paths[[1]]
  snapped <- path$snapped_waypoints$coordinates
  
  tibble(
    network_distance_m = as.numeric(path$distance),
    network_time_min = as.numeric(path$time) / 60000,
    snapped_from_lon = snapped[[1]][[1]], snapped_from_lat = snapped[[1]][[2]],
    snapped_to_lon = snapped[[2]][[1]], snapped_to_lat = snapped[[2]][[2]]
  )
}

nearest_network_feature <- function(
    parcels,
    access_points,
    parcel_id,
    feature_id = "feature_id",
    feature_name = "feature_name",
    k = 10,
    base_url = "http://localhost:8989",
    profile = "car",
    metric_crs = 26917,
    snap_preventions = c("ferry"),
    choose_by = c("distance", "time")
) {
  choose_by <- match.arg(choose_by)
  
  stopifnot(inherits(parcels, "sf"))
  stopifnot(inherits(access_points, "sf"))
  
  if (nrow(access_points) == 0L) {stop("access_points contains no destination points.")}
  
  geometry_types <- unique(as.character(st_geometry_type(access_points)))
  
  if (!all(geometry_types %in% c("POINT", "MULTIPOINT"))) {
    stop("access_points must contain point geometry.")
  }
  
  parcels_m <- st_transform(parcels, metric_crs)
  access_m  <- st_transform(access_points, metric_crs)
  
  parcels_ll <- st_transform(parcels, 4326)
  access_ll  <- st_transform(access_points, 4326)
  
  parcel_xy <- st_coordinates(parcels_ll)
  access_xy <- st_coordinates(access_ll)
  
  map_dfr(seq_len(nrow(parcels_ll)), function(i) {
    
    candidate_linear_m <- as.numeric(st_distance(parcels_m[i, ], access_m))
    
    candidate_indices <- head(order(candidate_linear_m), min(k, nrow(access_m)))
    
    route_results <- map_dfr(candidate_indices, function(j) {
      
      route <- tryCatch(
        graphhopper_route(
          from_lon = parcel_xy[i, "X"], from_lat = parcel_xy[i, "Y"],
          to_lon = access_xy[j, "X"], to_lat = access_xy[j, "Y"],
          base_url = base_url, profile = profile, snap_preventions = snap_preventions
        ),
        error = function(e) {
          tibble(
            network_distance_m = NA_real_, network_time_min = NA_real_,
            snapped_from_lon = NA_real_, snapped_from_lat = NA_real_,
            snapped_to_lon = NA_real_, snapped_to_lat = NA_real_
          )
        }
      )
      
      route |>
        mutate(
          candidate_index = j, feature_id = as.character(access_points[[feature_id]][j]),
          feature_name = if (!is.null(feature_name)) {
            as.character(access_points[[feature_name]][j])
          } else {NA_character_}, candidate_linear_m = candidate_linear_m[j]
        )
    })
    
    valid_routes <- route_results |>
      filter(is.finite(network_distance_m), is.finite(network_time_min))
    
    if (nrow(valid_routes) == 0L) {
      return(
        tibble(
          parcel_id = as.character(parcels[[parcel_id]][i]), feature_id = NA_character_,
          feature_name = NA_character_, candidate_linear_m = NA_real_,
          network_distance_m = NA_real_, network_time_min = NA_real_,
          snapped_from_lon = NA_real_, snapped_from_lat = NA_real_,
          snapped_to_lon = NA_real_, snapped_to_lat = NA_real_
        )
      )
    }
    
    if (choose_by == "distance") {
      best <- valid_routes |> slice_min(network_distance_m, n = 1, with_ties = FALSE)
    } else {
      best <- valid_routes |> slice_min(network_time_min, n = 1, with_ties = FALSE)
    }
    
    best |>
      transmute(
        parcel_id = as.character(parcels[[parcel_id]][i]), feature_id, feature_name,
        candidate_linear_m, network_distance_m, network_time_min,
        snapped_from_lon, snapped_from_lat, snapped_to_lon, snapped_to_lat
      )
  })
}

###

metric_crs <- 26917

hospital_access <- hospitals |>
  st_transform(metric_crs) |>
  st_point_on_surface() |>
  st_transform(4326) |>
  transmute(
    feature_id = as.character(hospital_id),
    feature_name = as.character(name),
    geometry
  )

hospital_network <- nearest_network_feature(
  parcels = parcel_points,
  access_points = hospital_access,
  parcel_id = "parcelno",
  feature_id = "feature_id",
  feature_name = "feature_name",
  k = 10,
  base_url = graphhopper_url,
  profile = "car",
  metric_crs = metric_crs,
  snap_preventions = c("motorway", "trunk", "ferry"),
  choose_by = "distance"
) |>
  rename(
    nearest_hospital_network_id = feature_id,
    nearest_hospital_network_name = feature_name,
    hospital_candidate_linear_m = candidate_linear_m,
    hospital_network_m = network_distance_m,
    hospital_network_min = network_time_min
  )

###

hospital_network |>
  select(
    parcel_id,
    nearest_hospital_network_name,
    hospital_candidate_linear_m,
    hospital_network_m,
    hospital_network_min,
    snapped_to_lon,
    snapped_to_lat
  ) |>
  head(20)

hospital_access_xy <- st_coordinates(hospital_access)

hospital_access <- hospital_access |>
  mutate(
    requested_lon = hospital_access_xy[, "X"],
    requested_lat = hospital_access_xy[, "Y"]
  )

###

park_access <- parks |> st_transform(metric_crs) |> st_point_on_surface() |>
  st_transform(4326) |>
  transmute(feature_id = as.character(park_id), feature_name = as.character(name), geometry)

park_network <- nearest_network_feature(
  parcels = parcel_points, access_points = park_access, parcel_id = "parcelno",
  feature_id = "feature_id", feature_name = "feature_name", k = 10, base_url = graphhopper_url,
  profile = "car", metric_crs = metric_crs, snap_preventions = c("motorway", "trunk", "ferry"),
  choose_by = "distance"
) |>
  rename(
    nearest_park_network_id = feature_id, nearest_park_network_name = feature_name,
    park_candidate_linear_m = candidate_linear_m,
    park_network_m = network_distance_m, park_network_min = network_time_min
  )

park_network |>
  select(
    parcel_id, nearest_park_network_name, park_candidate_linear_m, park_network_m,
    park_network_min, snapped_to_lon, snapped_to_lat
  ) |> head(20)

park_access_xy <- st_coordinates(park_access)

park_access <- park_access |>
  mutate(requested_lon = park_access_xy[, "X"], requested_lat = park_access_xy[, "Y"])

###

highway_access <- highways |> st_transform(metric_crs) |> st_point_on_surface() |>
  st_transform(4326) |>
  transmute(feature_id = as.character(highway_id), feature_name = as.character(name), geometry)

highway_network <- nearest_network_feature(
  parcels = parcel_points, access_points = highway_access, parcel_id = "parcelno",
  feature_id = "feature_id", feature_name = "feature_name", k = 10, base_url = graphhopper_url,
  profile = "car", metric_crs = metric_crs, snap_preventions = c("motorway", "trunk", "ferry"),
  choose_by = "distance"
) |>
  rename(
    highway_entrance_network_id = feature_id, highway_entrance_network_name = feature_name,
    highway_entrance_candidate_linear_m = candidate_linear_m,
    highway_entrance_network_m = network_distance_m, highway_entrance_network_min = network_time_min
  )

highway_network |>
  select(
    parcel_id, highway_entrance_network_name, highway_entrance_candidate_linear_m,
    highway_entrance_network_m, highway_entrance_network_min, snapped_to_lon, snapped_to_lat
  ) |> head(20)

highway_access_xy <- as.data.frame(sf::st_coordinates(highway_access))
stopifnot(nrow(highway_access_xy) == nrow(highway_access))
highway_access$requested_lon <- highway_access_xy$X
highway_access$requested_lat <- highway_access_xy$Y

parcel_spatial_features <- parcel_points |>
  st_drop_geometry() |>
  mutate(parcel_id = as.character(parcelno)) |>
  left_join(hospital_linear, by = "parcel_id") |>
  left_join(hospital_network, by = "parcel_id") |>
  left_join(park_linear, by = "parcel_id") |>
  left_join(park_network, by = "parcel_id") |>
  left_join(highway_linear, by = "parcel_id") |>
  left_join(highway_network, by = "parcel_id")

count_duplicate_parcels <- function(x, table_name) {
  x_df <- if (inherits(x, "sf")) {st_drop_geometry(x)} else {x}
  id_column <- if ("parcel_id" %in% names(x_df)) {"parcel_id"
  } else if ("parcelno" %in% names(x_df)) {"parcelno"
  } else {stop(table_name, " contains neither `parcel_id` nor `parcelno`.")
  }
  
  duplicate_ids <- x_df |> mutate(parcel_id = as.character(.data[[id_column]])) |>
    count(parcel_id, name = "rows_per_parcel") |> filter(rows_per_parcel > 1)
  
  tibble(
    table = table_name, duplicated_parcel_ids = nrow(duplicate_ids),
    maximum_rows_per_parcel = if (nrow(duplicate_ids) == 0L) {1L
    } else {max(duplicate_ids$rows_per_parcel)}
  )
}

tables_to_check <- list(
  parcel_points    = parcel_points,  hospital_linear = hospital_linear,
  hospital_network = hospital_network,  park_linear  = park_linear,
  park_network = park_network,  highway_linear = highway_linear,
  highway_network  = highway_network
)

duplicate_summary <- imap_dfr(tables_to_check,  count_duplicate_parcels)
duplicate_summary

find_duplicate_parcels <- function(x) {
  x_df <- if (inherits(x, "sf")) {st_drop_geometry(x)} else {x}
  id_column <- if ("parcel_id" %in% names(x_df)) {"parcel_id"} else if ("parcelno" %in% names(x_df))
    {"parcelno"} else {stop("Object contains neither `parcel_id` nor `parcelno`.")}
  
  x_df |> mutate(parcel_id = as.character(.data[[id_column]])) |>
    count(parcel_id, name = "rows_per_parcel") |>
    filter(rows_per_parcel > 1) |> arrange(desc(rows_per_parcel))
}

find_duplicate_parcels(parcel_points)
find_duplicate_parcels(hospital_network)
find_duplicate_parcels(park_network_car)

###

st_crs(parcel_points)
st_crs(detroit_river_water_union)

# parcel_points_m <- parcel_points |> st_transform(st_crs(detroit_river_water_union))
parcel_points_m <- parcel_points |> st_transform(metric_crs)

river_linear_distance <- st_distance(parcel_points_m, detroit_river_water_union)

detroit_river_linear <- tibble(
  parcel_id = as.character(parcel_points_m$parcelno),
  detroit_river_linear_m = as.numeric(river_linear_distance)
)

river_lines <- wayne_river_shoreline |>
  summarise(geometry = st_union(geometry)) |> st_cast("LINESTRING", warn = FALSE)

river_samples <- st_line_sample(st_geometry(river_lines), density = 1 / 150)
river_points <- st_cast(river_samples, "POINT")

river_access <- st_sf(
  feature_id = paste0("river_", seq_along(river_points)), feature_name = "Detroit River",
  geometry = river_points) |> st_transform(4326)

###

river_network <- nearest_network_feature(
  parcels = parcel_points, access_points = river_access, parcel_id = "parcelno",
  feature_id = "feature_id", feature_name = "feature_name", k = 20,
  base_url = graphhopper_url, profile = "car", metric_crs = metric_crs,
  snap_preventions = c("motorway", "trunk", "ferry"), choose_by = "distance"
)

river_network <- river_network |>
  transmute(
    parcel_id, nearest_river_drive_point = feature_id,
    detroit_river_drive_m = network_distance_m, detroit_river_drive_min = network_time_min
  )

###

grocery_tags <- c("shop", "name", "brand", "operator", "access", "entrance")

osm_grocery_points <- osmextract::oe_read(    # Point POIs
  file_path = "C:/Users/satur/Document5/ghopper/michigan-260729.osm.pbf", layer = "points",
  boundary = wayne_buffer, boundary_type = "spat", extra_tags = grocery_tags, quiet = FALSE
)

osm_grocery_polygons <- osmextract::oe_read(    # Building/property polygons
  file_path = "C:/Users/satur/Document5/ghopper/michigan-260729.osm.pbf",
  layer = "multipolygons", boundary = wayne_buffer, boundary_type = "spat",
  extra_tags = grocery_tags, quiet = FALSE
)

grocery_points <- osm_grocery_points |>
  filter(shop %in% c("supermarket", "grocery")) |> filter(!st_is_empty(geometry)) |>
  mutate(
    grocery_id = paste0("point_", osm_id),
    grocery_name = coalesce(na_if(name, ""), na_if(brand, ""), na_if(operator, ""),
      "Unnamed grocery store"), source_geometry = "point"
  )

grocery_polygons <- osm_grocery_polygons |>
  filter(shop %in% c("supermarket", "grocery")) |> st_make_valid() |>
  filter(!st_is_empty(geometry)) |>
  mutate(
    grocery_id = paste0("polygon_", osm_id),
    grocery_name = coalesce(na_if(name, ""), na_if(brand, ""), na_if(operator, ""),
      "Unnamed grocery store"), source_geometry = "polygon"
  )

grocery_points |> st_drop_geometry() |> count(grocery_name, sort = TRUE)
grocery_polygons |> st_drop_geometry() |> count(grocery_name, sort = TRUE)

grocery_points_linear <- grocery_points |> transmute(grocery_id, name = grocery_name, geometry)
grocery_polygons_linear <- grocery_polygons |> transmute(grocery_id, name = grocery_name, geometry)
grocery_stores <- rbind(grocery_points_linear, grocery_polygons_linear)

nrow(grocery_stores)
table(st_geometry_type(grocery_stores))

grocery_linear <- nearest_linear_feature(
  parcels = parcel_points, features = grocery_stores, parcel_id = "parcelno",
  feature_id = "grocery_id", feature_name = "name", metric_crs = metric_crs
) |>
  rename(nearest_grocery_linear_id = feature_id,
    nearest_grocery_linear_name = feature_name, grocery_linear_m = linear_distance_m)

grocery_polygon_access <- grocery_polygons |> st_transform(metric_crs) |>
  st_point_on_surface() |> st_transform(4326) |>
  transmute(feature_id = grocery_id, feature_name = grocery_name, geometry)

grocery_point_access <- grocery_points |> st_transform(4326) |>
  transmute(feature_id = grocery_id, feature_name = grocery_name, geometry)

grocery_access <- rbind(grocery_point_access, grocery_polygon_access)

table(st_geometry_type(grocery_access))
st_crs(grocery_access)
nrow(grocery_access)

grocery_network <- nearest_network_feature(
  parcels = parcel_points, access_points = grocery_access, parcel_id = "parcelno",
  feature_id = "feature_id", feature_name = "feature_name", k = 10,
  base_url = graphhopper_url, profile = "car", metric_crs = metric_crs,
  snap_preventions = c("motorway", "trunk", "ferry"), choose_by = "distance"
) |>
  rename(
    nearest_grocery_drive_id = feature_id, nearest_grocery_drive_name = feature_name,
    grocery_candidate_linear_m = candidate_linear_m,
    grocery_drive_m = network_distance_m, grocery_drive_min = network_time_min
  )

###

transit_tags <- c(
  "public_transport", "highway", "railway", "bus", "tram",
  "name", "ref", "operator", "network", "route_ref"
)

osm_transit_points <- osmextract::oe_read(
  file_path = "C:/Users/satur/Document5/ghopper/michigan-260729.osm.pbf", layer = "points",
  boundary = wayne_buffer, boundary_type = "spat", extra_tags = transit_tags, quiet = FALSE
)

sort(unique(na.omit(osm_transit_points$public_transport)))
sort(unique(na.omit(osm_transit_points$highway)))
sort(unique(na.omit(osm_transit_points$railway)))

bus_stops <- osm_transit_points |>
  filter(highway == "bus_stop" | (public_transport == "platform" & bus == "yes")) |>
  filter(!st_is_empty(geometry)) |>
  mutate(transit_id = paste0("bus_", osm_id),
    transit_name = coalesce(na_if(name, ""), na_if(ref, ""), paste0("Bus stop ", osm_id)),
    transit_type = "bus") |>
  select(
    transit_id, transit_name, transit_type, name, ref, operator,
    network, route_ref, geometry
  ) |> st_transform(4326)

nrow(bus_stops)
bus_stops |> st_drop_geometry() |> count(operator, sort = TRUE)

tram_candidates <- osm_transit_points |>
  filter(railway == "tram_stop" | (public_transport == "platform" & tram == "yes")) |>
  filter(!st_is_empty(geometry))

qline_stops <- tram_candidates |>
  filter(grepl("qline|q-line|m-1", paste(coalesce(name, ""), coalesce(network, ""), coalesce(operator, "")),
      ignore.case = TRUE)) |>
  mutate(transit_id = paste0("qline_", osm_id),
    transit_name = coalesce(na_if(name, ""), na_if(ref, ""), paste0("QLINE stop ", osm_id)),
    transit_type = "qline") |>
  select(
    transit_id, transit_name, transit_type, name, ref, operator, network, geometry
  ) |> st_transform(4326)

plot(st_geometry(bus_stops), main = "Extracted Bus Stops")
plot(st_geometry(qline_stops), main = "Extracted QLINE Stops")

bus_linear <- nearest_linear_feature(
  parcels = parcel_points, features = bus_stops, parcel_id = "parcelno",
  feature_id = "transit_id", feature_name = "transit_name", metric_crs = metric_crs
) |>
  rename(
    nearest_bus_stop_id = feature_id,
    nearest_bus_stop_name = feature_name, bus_linear_m = linear_distance_m
  )

qline_linear <- nearest_linear_feature(
  parcels = parcel_points, features = qline_stops, parcel_id = "parcelno",
  feature_id = "transit_id", feature_name = "transit_name", metric_crs = metric_crs
) |>
  rename(
    nearest_qline_stop_id = feature_id,
    nearest_qline_stop_name = feature_name, qline_linear_m = linear_distance_m
  )

bus_network <- nearest_network_feature(
  parcels = parcel_points, access_points = bus_stops, parcel_id = "parcelno",
  feature_id = "transit_id", feature_name = "transit_name", k = 10,
  base_url = graphhopper_url, profile = "foot", metric_crs = metric_crs,
  snap_preventions = c("motorway", "trunk", "ferry"), choose_by = "distance"
) |>
  rename(
    nearest_bus_walk_id = feature_id, nearest_bus_walk_name = feature_name,
    bus_candidate_linear_m = candidate_linear_m, bus_walk_m = network_distance_m,
    bus_walk_min = network_time_min
  )

qline_network <- nearest_network_feature(
  parcels = parcel_points, access_points = qline_stops, parcel_id = "parcelno",
  feature_id = "transit_id", feature_name = "transit_name", k = 10, base_url = graphhopper_url,
  profile = "foot", metric_crs = metric_crs,
  snap_preventions = c("motorway", "trunk", "ferry"), choose_by = "distance"
) |>
  rename(
    nearest_qline_walk_id = feature_id, nearest_qline_walk_name = feature_name,
    qline_candidate_linear_m = candidate_linear_m,
    qline_walk_m = network_distance_m, qline_walk_min = network_time_min
  )

transit_stops <- rbind(
  bus_stops |> select(transit_id, transit_name, transit_type, geometry),
  qline_stops |> select(transit_id, transit_name, transit_type, geometry)
)

# DO
transit_linear <- nearest_linear_feature(
  parcels = parcel_points, features = transit_stops, parcel_id = "parcelno",
  feature_id = "transit_id", feature_name = "transit_name", metric_crs = metric_crs
) |>
  rename(
    nearest_transit_id = feature_id,
    nearest_transit_name = feature_name, transit_linear_m = linear_distance_m
  )

# DO
transit_network <- nearest_network_feature(
  parcels = parcel_points, access_points = transit_stops, parcel_id = "parcelno",
  feature_id = "transit_id", feature_name = "transit_name", k = 10, base_url = graphhopper_url,
  profile = "foot", metric_crs = metric_crs,
  snap_preventions = c("motorway", "trunk", "ferry"), choose_by = "distance"
) |>
  rename(
    nearest_transit_walk_id = feature_id, nearest_transit_walk_name = feature_name,
    transit_walk_m = network_distance_m, transit_walk_min = network_time_min
  )

###

emergency_tags <- c(
  "amenity", "name", "operator", "operator:type", "emergency", "access",
  "addr:housenumber", "addr:street"
)

osm_emergency_points <- osmextract::oe_read(    # Point features
  file_path = "C:/Users/satur/Document5/ghopper/michigan-260729.osm.pbf", layer = "points",
  boundary = wayne_buffer, boundary_type = "spat", extra_tags = emergency_tags, quiet = FALSE
)

osm_emergency_polygons <- osmextract::oe_read(    # Polygon/building features
  file_path = "C:/Users/satur/Document5/ghopper/michigan-260729.osm.pbf",
  layer = "multipolygons", boundary = wayne_buffer, boundary_type = "spat",
  extra_tags = emergency_tags, quiet = FALSE
)

police_points <- osm_emergency_points |> filter(amenity == "police") |>
  filter(!st_is_empty(geometry)) |>
  mutate(
    police_id = paste0("police_point_", osm_id),
    police_name = coalesce(
      na_if(name, ""), na_if(operator, ""), paste0("Police station ", osm_id)
    )
  )

police_polygons <- osm_emergency_polygons |> filter(amenity == "police") |> st_make_valid() |>
  filter(!st_is_empty(geometry)) |>
  mutate(
    police_id = paste0("police_polygon_", osm_id),
    police_name = coalesce(
      na_if(name, ""), na_if(operator, ""), paste0("Police station ", osm_id)
    )
  )

bind_rows(st_drop_geometry(fire_points), st_drop_geometry(fire_polygons)) |>
  select(fire_id, fire_name, operator)

fire_points <- osm_emergency_points |> filter(amenity == "fire_station") |>
  filter(!st_is_empty(geometry)) |>
  mutate(
    fire_id = paste0("fire_point_", osm_id),
    fire_name = coalesce(
      na_if(name, ""), na_if(operator, ""), paste0("Fire station ", osm_id)
    )
  )

fire_polygons <- osm_emergency_polygons |> filter(amenity == "fire_station") |>
  st_make_valid() |> filter(!st_is_empty(geometry)) |>
  mutate(
    fire_id = paste0("fire_polygon_", osm_id),
    fire_name = coalesce(na_if(name, ""), na_if(operator, ""), paste0("Fire station ", osm_id))
  )

police_stations <- rbind(
  police_points |> transmute(feature_id = police_id, feature_name = police_name, geometry),
  police_polygons |> transmute(feature_id = police_id, feature_name = police_name, geometry)
)

fire_stations <- rbind(
  fire_points |> transmute(feature_id = fire_id, feature_name = fire_name, geometry),
  fire_polygons |> transmute(feature_id = fire_id, feature_name = fire_name, geometry)
)

# TO DO

police_linear <- nearest_linear_feature(
  parcels = parcel_points, features = police_stations, parcel_id = "parcelno",
  feature_id = "feature_id", feature_name = "feature_name", metric_crs = metric_crs
) |>
  rename(
    nearest_police_linear_id = feature_id,
    nearest_police_linear_name = feature_name, police_linear_m = linear_distance_m
  )
# to do
fire_linear <- nearest_linear_feature(
  parcels = parcel_points, features = fire_stations, parcel_id = "parcelno",
  feature_id = "feature_id", feature_name = "feature_name", metric_crs = metric_crs
) |>
  rename(
    nearest_fire_linear_id = feature_id,
    nearest_fire_linear_name = feature_name, fire_linear_m = linear_distance_m
  )

#

police_polygon_access <- police_polygons |> st_transform(metric_crs) |> st_point_on_surface() |>
  st_transform(4326) |> transmute(feature_id = police_id, feature_name = police_name, geometry)

police_point_access <- police_points |> st_transform(4326) |>
  transmute(feature_id = police_id, feature_name = police_name, geometry)

police_access <- rbind(police_point_access, police_polygon_access)

fire_polygon_access <- fire_polygons |> st_transform(metric_crs) |> st_point_on_surface() |>
  st_transform(4326) |> transmute(feature_id = fire_id, feature_name = fire_name, geometry)

fire_point_access <- fire_points |> st_transform(4326) |>
  transmute(feature_id = fire_id, feature_name = fire_name, geometry)

fire_access <- rbind(fire_point_access, fire_polygon_access)

table(st_geometry_type(police_access))
table(st_geometry_type(fire_access))
st_crs(police_access)
st_crs(fire_access)

# TO DO

police_network <- nearest_network_feature(
  parcels = parcel_points, access_points = police_access, parcel_id = "parcelno",
  feature_id = "feature_id", feature_name = "feature_name", k = 10,
  base_url = graphhopper_url, profile = "car", metric_crs = metric_crs,
  snap_preventions = c("motorway", "trunk", "ferry"), choose_by = "distance"
) |>
  rename(
    nearest_police_drive_id = feature_id, nearest_police_drive_name = feature_name,
    police_candidate_linear_m = candidate_linear_m,
    police_drive_m = network_distance_m, police_drive_min = network_time_min
  )

# to do

fire_network <- nearest_network_feature(
  parcels = parcel_points, access_points = fire_access, parcel_id = "parcelno",
  feature_id = "feature_id", feature_name = "feature_name", k = 10, base_url = graphhopper_url,
  profile = "car", metric_crs = metric_crs,
  snap_preventions = c("motorway", "trunk", "ferry"), choose_by = "distance"
) |>
  rename(
    nearest_fire_drive_id = feature_id,
    nearest_fire_drive_name = feature_name, fire_candidate_linear_m = candidate_linear_m,
    fire_drive_m = network_distance_m, fire_drive_min = network_time_min
  )
#TO DO
emergency_features <- police_linear |>
  left_join(police_network, by = "parcel_id", relationship = "one-to-one") |>
  left_join(fire_linear,    by = "parcel_id", relationship = "one-to-one") |>
  left_join(fire_network,   by = "parcel_id", relationship = "one-to-one")

###

education_tags <- c(
  "amenity", "name", "operator", "operator:type", "school", "grades", "religion",
  "denomination", "access", "addr:housenumber", "addr:street"
)

osm_education_points <- osmextract::oe_read(    # Point features
  file_path = "C:/Users/satur/Document5/ghopper/michigan-260729.osm.pbf", layer = "points",
  boundary = wayne_buffer, boundary_type = "spat", extra_tags = education_tags, quiet = FALSE
)

osm_education_polygons <- osmextract::oe_read(    # Campus/property polygons
  file_path = "C:/Users/satur/Document5/ghopper/michigan-260729.osm.pbf", layer = "multipolygons",
  boundary = wayne_buffer, boundary_type = "spat", extra_tags = education_tags, quiet = FALSE
)

school_points <- osm_education_points |> filter(amenity == "school") |>
  filter(!st_is_empty(geometry)) |>
  mutate(school_id = paste0("school_point_", osm_id),
    school_name = coalesce(na_if(name, ""), na_if(operator, ""), paste0("School ", osm_id))
  )

school_polygons <- osm_education_polygons |> filter(amenity == "school") |> st_make_valid() |>
  filter(!st_is_empty(geometry)) |>
  mutate(school_id = paste0("school_polygon_", osm_id),
    school_name = coalesce(na_if(name, ""), na_if(operator, ""), paste0("School ", osm_id))
  )

bind_rows(st_drop_geometry(school_points), st_drop_geometry(school_polygons)) |>
  select(school_id,  school_name,  operator,  school)

#

university_points <- osm_education_points |> filter(amenity %in% c("college", "university")) |>
  filter(!st_is_empty(geometry)) |>
  mutate(
    university_id = paste0("highered_point_", osm_id),
    university_name = coalesce(
      na_if(name, ""), na_if(operator, ""), paste0("College/university ", osm_id)
    ), institution_type = amenity
  )

university_polygons <- osm_education_polygons |> filter(amenity %in% c("college", "university")) |>
  st_make_valid() |> filter(!st_is_empty(geometry)) |>
  mutate(university_id = paste0("highered_polygon_", osm_id),
    university_name = coalesce(na_if(name, ""), na_if(operator, ""),
      paste0("College/university ", osm_id)
    ), institution_type = amenity
  )

bind_rows(st_drop_geometry(university_points),  st_drop_geometry(university_polygons)) |>
  select(university_id,  university_name, institution_type, operator) 

schools <- rbind(
  school_points |> transmute(feature_id = school_id, feature_name = school_name, geometry),
  school_polygons |> transmute(feature_id = school_id, feature_name = school_name, geometry)
)

universities <- rbind(
  university_points |>
    transmute(feature_id = university_id, feature_name = university_name,
              institution_type, geometry),
  university_polygons |>
    transmute(
      feature_id = university_id, feature_name = university_name, institution_type, geometry)
)

# TO DO

school_linear <- nearest_linear_feature(
  parcels = parcel_points, features = schools, parcel_id = "parcelno",
  feature_id = "feature_id", feature_name = "feature_name", metric_crs = metric_crs) |>
  rename(nearest_school_linear_id = feature_id,
    nearest_school_linear_name = feature_name, school_linear_m = linear_distance_m)
# TO DO
university_linear <- nearest_linear_feature(
  parcels = parcel_points, features = universities, parcel_id = "parcelno",
  feature_id = "feature_id", feature_name = "feature_name", metric_crs = metric_crs) |>
  rename(nearest_university_linear_id = feature_id,
    nearest_university_linear_name = feature_name, university_linear_m = linear_distance_m)

#

school_point_access <- school_points |>  st_transform(4326) |>
  transmute(feature_id = school_id, feature_name = school_name, geometry)

school_polygon_access <- school_polygons |>  st_transform(metric_crs) |>
  st_point_on_surface() |>  st_transform(4326) |>
  transmute(feature_id = school_id, feature_name = school_name, geometry)

school_access <- rbind(school_point_access, school_polygon_access)

table(st_geometry_type(school_access))
st_crs(school_access)

university_point_access <- university_points |>  st_transform(4326) |>
  transmute(feature_id = university_id, feature_name = university_name, geometry)

university_polygon_access <- university_polygons |> st_transform(metric_crs) |>
  st_point_on_surface() |> st_transform(4326) |>
  transmute(feature_id = university_id, feature_name = university_name, geometry)

university_access <- rbind(university_point_access, university_polygon_access)

table(st_geometry_type(university_access))
st_crs(university_access)

#

# TO DO
school_network <- nearest_network_feature(
  parcels = parcel_points, access_points = school_access, parcel_id = "parcelno",
  feature_id = "feature_id", feature_name = "feature_name", k = 10,
  base_url = graphhopper_url, profile = "foot", metric_crs = metric_crs, # profile = "car"
  snap_preventions = c("motorway", "trunk", "ferry"), choose_by = "distance") |>
  rename(
    nearest_school_walk_id = feature_id, nearest_school_walk_name = feature_name,
    school_candidate_linear_m = candidate_linear_m, school_walk_m = network_distance_m,
    school_walk_min = network_time_min
  )


# TO DO

university_network <- nearest_network_feature(
  parcels = parcel_points, access_points = university_access, parcel_id = "parcelno",
  feature_id = "feature_id", feature_name = "feature_name", k = 10,
  base_url = graphhopper_url, profile = "foot",  metric_crs = metric_crs, # profile = "car"
  snap_preventions = c("motorway", "trunk", "ferry"), choose_by = "distance") |>
  rename(
    nearest_university_walk_id = feature_id, nearest_university_walk_name = feature_name,
    university_candidate_linear_m = candidate_linear_m,
    university_walk_m = network_distance_m, university_walk_min = network_time_min)

education_access <- rbind(
  school_access |> mutate(education_type = "school"),
  university_access |> mutate(education_type = "higher_education")
)

education_features_linear <- rbind(
  schools |> mutate(education_type = "school"),
  universities |>
    mutate(education_type = "higher_education") |>
    select(feature_id, feature_name, education_type, geometry)
)


# TO DO
education_linear <- nearest_linear_feature(
  parcels = parcel_points, features = education_features_linear, parcel_id = "parcelno",
  feature_id = "feature_id", feature_name = "feature_name",  metric_crs = metric_crs) |>
  rename(
    nearest_education_id = feature_id, nearest_education_name = feature_name,
    education_linear_m = linear_distance_m
  )

# to do

education_network <- nearest_network_feature(
  parcels = parcel_points, access_points = education_access, parcel_id = "parcelno",
  feature_id = "feature_id", feature_name = "feature_name",  k = 10,
  base_url = graphhopper_url, profile = "foot", metric_crs = metric_crs,
  snap_preventions = c("motorway", "trunk", "ferry"), choose_by = "distance") |>
  rename(
    nearest_education_walk_id = feature_id, nearest_education_walk_name = feature_name,
    education_walk_m = network_distance_m,  education_walk_min = network_time_min
  )

# to do

education_features <- school_linear |>
  left_join(school_network, by = "parcel_id", relationship = "one-to-one") |>
  left_join(university_linear, by = "parcel_id", relationship = "one-to-one") |>
  left_join(university_network, by = "parcel_id", relationship = "one-to-one")


















