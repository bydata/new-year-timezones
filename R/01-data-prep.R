library(sf)
library(dplyr)
library(lubridate)
library(readr)
library(terra)
library(here)

data_path <- here("data")

#' Source: https://github.com/evansiroky/timezone-boundary-builder
#' Licence: MIT Licence

# Download and unpack the archive
geojson_url <- "https://github.com/evansiroky/timezone-boundary-builder/releases/download/2024b/timezones-now.geojson.zip"
local_path_zip <- here(data_path, "timezones-now.geojson.zip")
download.file(geojson_url, destfile = local_path_zip)
unzip(local_path_zip, exdir = data_path)

timezones <- st_read(here(data_path, "combined-now.json"))
st_crs(timezones)

# Calculate the difference from UTC in hours
get_utc_difference <- function(tz) {
  gmt_offset <- as.numeric(force_tz(Sys.time(), "UTC") - force_tz(Sys.time(), tz), units = "hours")
  round(gmt_offset, 2)
}

# Calculate whether it's the new year already in a given timezone
is_it_new_year <- function(tz, new_year = 2025) {
  stopifnot(tz %in% unique(timezones$tzid))
  year(with_tz(Sys.time(), tz)) == new_year
}
timezones$tzid[34]
is_it_new_year(timezones$tzid[34])
is_it_new_year("Pacific/Auckland")

#' Download the population density data from 
#' https://human-settlement.emergency.copernicus.eu/download.php?ds=pop
#' https://ghsl.jrc.ec.europa.eu/download/GHSL_data_54009_shapefile.zip
ghsl_raster_path <- 
  file.path(data_path,
            "GHS_POP_E2020_GLOBE_R2023A_54009_1000_V1_0", 
            "GHS_POP_E2020_GLOBE_R2023A_54009_1000_V1_0.tif")
ghsl_raster <- rast(ghsl_raster_path)
raster_crs <- crs(ghsl_raster)

ghsl_raster_aggregated <- aggregate(
  ghsl_raster, fact = 100, fun = sum, na.rm = TRUE)
names(ghsl_raster_aggregated) <- "pop"

# Transform raster data to sf dataframe
ghsl_df <- as.data.frame(ghsl_raster_aggregated, xy = TRUE) |> 
  st_as_sf(coords = c("x", "y"), crs = crs(ghsl_raster))
colnames(ghsl_df) <- c("pop", "geometry")

timezones <- st_transform(timezones, raster_crs) |> 
  mutate(tz_difference = get_utc_difference(tzid))

timezones_with_pop_units <- timezones |> 
  st_join(ghsl_df) |> 
  st_drop_geometry() |> 
  group_by(tzid, tz_difference) |> 
  summarize(pop = sum(pop, na.rm = TRUE), 
            .groups = "drop")

crs <- "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
timezones_with_pop <- timezones |> 
  inner_join(timezones_with_pop_units, by = join_by(tzid, tz_difference)) |> 
  st_transform(crs = crs)

timezones_with_pop_simplified <- timezones_with_pop |> 
  st_simplify(dTolerance = 10000) 
  
write_rds(timezones_with_pop_simplified, here(data_path, "timezones-with-population.rds"))
