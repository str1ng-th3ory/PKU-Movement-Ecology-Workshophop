## ----setup, include=FALSE-------------------------------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)


## ----Clean Libraries, message=FALSE, warning=FALSE------------------------------------------------------------------------------
# Clear all objects from the workspace to ensure a clean environment
rm(list=ls())

# Load required libraries
library(sf)         # Simple features for R
library(rgdal)      # Geospatial data abstraction library
library(sp)         # Spatial data classes
library(rgeos)      # Interface to geometry engine
library(dplyr)      # Data manipulation
library(raster)     # Raster data manipulation
library(viridis)    # Color scales
library(ggplot2)    # Data visualization
library(mapview)    # Interactive maps
library(terra)      # Raster and vector data manipulation
library(lubridate)  # Date and time manipulation

##########################################
#Version 2: only use sf and terra package

# Load required libraries
#library(sf)         # Simple features for R
#library(dplyr)      # Data manipulation
#library(terra)      # Raster and vector data manipulation
#library(ggplot2)    # Data visualization
#library(mapview)    # Interactive maps
#library(lubridate)  # Date and time manipulation


## ----Load Data, message=FALSE, warning=FALSE------------------------------------------------------------------------------------
# Load the pre-processed animal movement data from an RDS file
WB <- readRDS("wildebeest_3hr_adehabitat.rds")

# Check the basic structure and class of the data to understand its format
class(WB) 
str(WB)

# Filter out rows with missing coordinates to ensure all data points are valid
WB <- WB %>%
  filter(!is.na(x) & !is.na(y))

#Convert the data frame to an sf object and set the CRS to UTM zone 37 S
WB_utm <- WB %>%
  st_as_sf(coords = c("x", "y"), crs = 32737)


## ----Create MCP, message=FALSE, warning=FALSE-----------------------------------------------------------------------------------
# Convert the projection to WGS 84 (EPSG:4326)
WB_84 <- st_transform(WB_utm, crs = 4326)

# Create a minimum convex polygon (MCP) to cover the moving areas of all individuals
WB_84_mcp <- st_convex_hull(st_union(WB_84))

# Save the MCP as a shapefile
st_write(WB_84_mcp, "Boundary.shp")

# Load the MCP shapefile to confirm is was saved correctly
MCP=st_read("Boundary.shp")


## ----load-landcover, message=FALSE, warning=FALSE-------------------------------------------------------------------------------
# Load land cover data for different years
landcover_2010 <- raster("./Landcover/MCD12Q1.061_LC_Type1_doy2010001_aid0001.tif")
landcover_2011 <- raster("./Landcover/MCD12Q1.061_LC_Type1_doy2011001_aid0001.tif")
landcover_2012 <- raster("./Landcover/MCD12Q1.061_LC_Type1_doy2012001_aid0001.tif")

# Stack the land cover data
landcover <- stack(landcover_2010, landcover_2011, landcover_2012)

##########################################
#Version 2: only use sf and terra package

# Load land cover data for different years
#landcover_2010 <- rast("./Landcover/MCD12Q1.061_LC_Type1_doy2010001_aid0001.tif")
#landcover_2011 <- rast("./Landcover/MCD12Q1.061_LC_Type1_doy2011001_aid0001.tif")
#landcover_2012 <- rast("./Landcover/MCD12Q1.061_LC_Type1_doy2012001_aid0001.tif")

# Stack the land cover data
#landcover <- c(landcover_2010, landcover_2011, landcover_2012)


## ----rename-landcover, message=FALSE, warning=FALSE-----------------------------------------------------------------------------
# Define land cover types
landcover_levels <- c(
  "Evergreen needleleaf forests", "Evergreen broadleaf forests", "Deciduous needleleaf forests",
  "Deciduous broadleaf forests", "Mixed forests", "Closed shrublands", "Open shrublands",
  "Woody savannas", "Savannas", "Grasslands", "Permanent wetlands", "Croplands",
  "Urban and built-up lands", "Cropland/natural vegetation mosaics", "Snow and ice",
  "Barren", "Water bodies"
)

# Convert stacked land cover data to data frame
landcover_df <- as.data.frame(landcover, xy = TRUE, na.rm=TRUE)

# Rename land cover types for each year
landcover_df$landcover_2010 <- factor(landcover_df$MCD12Q1.061_LC_Type1_doy2010001_aid0001, levels = 1:17, labels = landcover_levels)
landcover_df$landcover_2011 <- factor(landcover_df$MCD12Q1.061_LC_Type1_doy2011001_aid0001, levels = 1:17, labels = landcover_levels)
landcover_df$landcover_2012 <- factor(landcover_df$MCD12Q1.061_LC_Type1_doy2012001_aid0001, levels = 1:17, labels = landcover_levels)


## ----visualize-landcover, message=FALSE, warning=FALSE--------------------------------------------------------------------------
# Plot land cover types for 2010
ggplot(landcover_df) +
  geom_raster(aes(x = x, y = y, fill = landcover_2010)) +
  geom_sf(data = MCP, inherit.aes = FALSE, fill = NA) +
  scale_fill_manual(values = c(
    "Evergreen needleleaf forests" = "#05450a", "Evergreen broadleaf forests" = "#086a10",
    "Deciduous needleleaf forests" = "#54a708", "Deciduous broadleaf forests" = "#78d203",
    "Mixed forests" = "#009900", "Closed shrublands" = "#c6b044", "Open shrublands" = "#dcd159",
    "Woody savannas" = "#dade48", "Savannas" = "#fbff13", "Grasslands" = "#b6ff05",
    "Permanent wetlands" = "#27ff87", "Croplands" = "#c24f44", "Urban and built-up lands" = "#a5a5a5",
    "Cropland/natural vegetation mosaics" = "#ff6d4c", "Snow and ice" = "#69fff8",
    "Barren" = "#f9ffa4", "Water bodies" = "#1c0dff"
  )) +
  labs(title = "Land Cover 2010", fill = "Land Cover Type", x = "Longitude", y = "Latitude") +
  theme_minimal()+
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1))


## ----calculate-distance, message=FALSE, warning=FALSE---------------------------------------------------------------------------
# Since the animal data projection is UTM 37S (EPSG:32737), we use WB_utm.
WB_sp <- as(WB_utm, "Spatial")

# Identify urban areas (land cover type 13)
urban_mask <- calc(landcover_2010, fun = function(x) { x == 13 })
urban_cells <- Which(urban_mask, cells = TRUE)
keyXY <- xyFromCell(landcover_2010, urban_cells, spatial = TRUE)

# Convert to UTM 37S (EPSG:32737)
utm_crs <- CRS("+proj=utm +zone=37 +south +datum=WGS84 +units=m +no_defs")
keyXY_utm <- spTransform(keyXY, utm_crs)

# Calculate the shortest distance from each GPS location to the nearest urban pixel
distances <- gDistance(WB_sp, keyXY_utm, byid = TRUE)
min_distances <- apply(distances, 2, min)

# Add distances to the original data
WB_utm$min_distance_to_urban <- min_distances

# View the results
head(WB_utm)

##########################################
#Version 2: only use sf and terra package

# Identify urban areas (land cover type 13)
#urban_mask <- classify(landcover_2010, cbind(13, 1), others = NA)

# Convert urban_mask to SpatRaster and then to SpatVector
#urban_points <- as.points(urban_mask, values=FALSE)

# Ensure the projection is UTM 37S (EPSG:32737)
#utm_crs <- "+proj=utm +zone=37 +south +datum=WGS84 +units=m +no_defs"

# Ensure urban_points CRS is the same as WB_sf
#urban_points <- project(urban_points, utm_crs)

# Convert urban_points to sf object
#urban_points_sf <- st_as_sf(urban_points)

# Ensure both sf objects have the same CRS
#urban_points_sf <- st_transform(urban_points_sf, crs = st_crs(WB_utm))

# Calculate the shortest distance from each GPS location to the nearest urban pixel
#distances <- st_distance(WB_utm, urban_points_sf, by_element = FALSE)

# Find the minimum distance for each GPS point
#min_distances <- apply(distances, 1, min)

# Add distances to the original data
#WB_utm$min_distance_to_urban <- min_distances

# View the results
#head(WB_utm)


## ----load-elevation, message=FALSE, warning=FALSE-------------------------------------------------------------------------------
# Load elevation data
elev <- raster("./Elevation/SRTMGL1_NC.003_SRTMGL1_DEM_doy2000042_aid0001.tif")

# Reproject to UTM 37S (EPSG:32737)
elev_utm <- projectRaster(elev, crs = utm_crs)

# Check the spatial resolution of the elevation data
res(elev_utm)

##########################################
#Version 2: only use sf and terra package

# Load elevation data
#elev <- rast("./Elevation/SRTMGL1_NC.003_SRTMGL1_DEM_doy2000042_aid0001.tif")

# Reproject to UTM 37S (EPSG:32737)
#elev_utm <- project(elev, "EPSG:32737")

# Check the spatial resolution of the elevation data
#res(elev_utm)


## ----elevation-mean, message=FALSE, warning=FALSE-------------------------------------------------------------------------------
# Calculate the mean and min value for the whole raster layer
cellStats(elev_utm, stat='mean',na.rm=TRUE)
cellStats(elev_utm, stat='min',na.rm=TRUE)

##########################################
#Version 2: only use sf and terra package

# Calculate the mean and min value for the whole raster layer
#global(elev_utm, c("mean", "min"), na.rm=TRUE)


## ----aggregate-elevation, message=FALSE, warning=FALSE--------------------------------------------------------------------------
# Load elevation raster
plot(elev_utm, main = "Original Elevation Data (res:30m)")

# Aggregate raster data to 300m resolution
elev_utm_300m <- aggregate(elev_utm, fact=10, fun=mean, expand=TRUE, na.rm=TRUE)
plot(elev_utm_300m, main = "Aggregated Elevation Data (res:300m)")


## ----terrain-indices, message=FALSE, warning=FALSE------------------------------------------------------------------------------
# Calculate terrain indices
terrain_elev <- terrain(elev_utm, opt=c("slope", "aspect", "TPI", "TRI", 
                                         "roughness", "flowdir"), unit='degrees')
# Set up plotting layout for 2 rows and 3 columns
par(mfrow = c(2, 3))

# Plot each terrain index with individual titles using a loop
for (i in 1:nlayers(terrain_elev)) {
  plot(terrain_elev[[i]], main = names(terrain_elev)[i])
}

# Reset plotting layout to default
par(mfrow = c(1, 1))

##########################################
#Version 2: only use sf and terra package

# Calculate terrain indices
#terrain_elev <- terrain(elev_utm, v=c("slope", "aspect", "TPI", "TRI", 
#                                         "roughness", "flowdir"), unit='degrees')
# Set up plotting layout for 2 rows and 3 columns
#par(mfrow = c(2, 3))

# Plot each terrain index with individual titles using a loop
#for (i in 1:nlyr(terrain_elev)) {
#  plot(terrain_elev[[i]], main = names(terrain_elev)[i])
#}

# Reset plotting layout to default
#par(mfrow = c(1, 1))


## ----terrain-indices-300m, message=FALSE, warning=FALSE-------------------------------------------------------------------------
# To observe the pattern more clearly, we can plot the elevation data in 30 m res as well.
terrain_elev_utm_300m <- terrain(elev_utm_300m, opt=c("slope", "aspect", "TPI", "TRI", 
                                         "roughness", "flowdir"), unit='degrees')
# Set up plotting layout for 2 rows and 3 columns
par(mfrow = c(2, 3))

# Plot each terrain index with individual titles using a loop
for (i in 1:nlayers(terrain_elev)) {
  plot(terrain_elev_utm_300m[[i]], main = names(terrain_elev_utm_300m)[i])
}

# Reset plotting layout to default
par(mfrow = c(1, 1))

##########################################
#Version 2: only use sf and terra package

# To observe the pattern more clearly, we can plot the elevation data in 30 m res as well.
#terrain_elev_utm_300m <- terrain(elev_utm_300m, v=c("slope", "aspect", "TPI", "TRI", 
#                                         "roughness", "flowdir"), unit='degrees')
# Set up plotting layout for 2 rows and 3 columns
#par(mfrow = c(2, 3))

# Plot each terrain index with individual titles using a loop
#for (i in 1:nlyr(terrain_elev)) {
#  plot(terrain_elev_utm_300m[[i]], main = names(terrain_elev_utm_300m)[i])
#}

# Reset plotting layout to default
#par(mfrow = c(1, 1))


## ----save-elevation-data, message=FALSE, warning=FALSE--------------------------------------------------------------------------
# Create output directory
dir.create("output")

#Subset specific terrain index layers
slope <- subset(terrain_elev, "slope")
aspect <- subset(terrain_elev, "aspect")
TPI <- subset(terrain_elev, "tpi")
roughness <- subset(terrain_elev, "roughness")

# Save processed elevation data
# A raster layer
writeRaster(slope, filename="output/slope.tif", format="GTiff", overwrite=TRUE)
# A layer from a raster stack/brick
writeRaster(terrain_elev[[1]], filename="output/tri", format="GTiff", overwrite=TRUE)
# A whole stack/brick
writeRaster(terrain_elev, filename="output/terrain_elev", format="GTiff", overwrite=TRUE)
# A whole raster stack/brick by layer
writeRaster(terrain_elev, filename=paste("output/", names(terrain_elev), sep = ""), format="GTiff", bylayer=T, overwrite=TRUE)


## ----extract-elevation-data, message=FALSE, warning=FALSE-----------------------------------------------------------------------
# Put all the environmental layer into a list
raster_list <- list(
  elevation = elev_utm,
  TPI = TPI,
  slope = slope,
  aspect = aspect,
  roughness = roughness
)
# Extract based on individual's position
extracted_values <- lapply(raster_list, function(r) extract(r, WB_sp))

# Add the value back to the movement data
for (name in names(extracted_values)) {
  WB_utm[[name]] <- extracted_values[[name]]
}

# Check the modified animal movement data 
head(WB_utm)

##########################################
#Version 2: only use sf and terra package

# Put all the environmental layers into a list
#raster_list <- list(
#  elevation = elev_utm,
#  TPI = terrain_elev$TPI,
#  slope = terrain_elev$slope,
#  aspect = terrain_elev$aspect,
#  roughness = terrain_elev$roughness
#)

# Extract based on individual's position
#extracted_values <- lapply(raster_list, function(r) terra::extract(r, WB_utm, ID=FALSE))

# Combine extracted values into a data frame
#extracted_df <- do.call(cbind, extracted_values)

# Add column names to the extracted data frame
#colnames(extracted_df) <- names(raster_list)

# Ensure the extracted values are in the correct format
#head(extracted_df)

# Add the values back to the movement data
#WB_utm <- cbind(WB_utm, extracted_df)

# Check the modified animal movement data
#head(WB_utm)



## ----load-ndvi, message=FALSE, warning=FALSE------------------------------------------------------------------------------------

# Load NDVI data
ndvi_files <- list.files(path = "./NDVI", pattern = "*_16_days_NDVI_.*\\.tif$", full.names = TRUE)

# Function to extract dates from NDVI filenames
extract_date_from_filename <- function(filename) {
  date_str <- sub(".*doy([0-9]{7}).*", "\\1", filename)
  year <- as.numeric(substr(date_str, 1, 4))
  day_of_year <- as.numeric(substr(date_str, 5, 7))
  date <- ymd(paste0(year, "-01-01")) + days(day_of_year - 1)
  as.POSIXct(date, tz="EAT")
}

# Extract dates from NDVI filenames
# Note: the time zone of MODIS data is UTC
ndvi_dates <- sapply(ndvi_files, extract_date_from_filename)
readable_ndvi_dates <- as.character(as.POSIXct(ndvi_dates, origin="1970-01-01", tz="UTC"))
ndvi_info <- data.frame(filename = basename(ndvi_files), date = readable_ndvi_dates)

# Load NDVI raster stack
ndvi_stack <- stack(ndvi_files)

# Reproject to UTM 37S (EPSG:32737)
ndvi_stack_utm <- projectRaster(ndvi_stack, crs = utm_crs)

##########################################
#Version 2: only use sf and terra package

# Load NDVI data
#ndvi_files <- list.files(path = "./NDVI", pattern = "*_16_days_NDVI_.*\\.tif$", full.names = TRUE)

# Function to extract dates from NDVI filenames
#extract_date_from_filename <- function(filename) {
#  date_str <- sub(".*doy([0-9]{7}).*", "\\1", filename)
#  year <- as.numeric(substr(date_str, 1, 4))
#  day_of_year <- as.numeric(substr(date_str, 5, 7))
#  date <- ymd(paste0(year, "-01-01")) + days(day_of_year - 1)
#  as.POSIXct(date, tz="EAT")
#}

# Extract dates from NDVI filenames
# Note: the time zone of MODIS data is UTC
#ndvi_dates <- sapply(ndvi_files, extract_date_from_filename)
#readable_ndvi_dates <- as.character(as.POSIXct(ndvi_dates, origin="1970-01-01", tz="UTC"))
#ndvi_info <- data.frame(filename = basename(ndvi_files), date = readable_ndvi_dates)

# Load NDVI raster stack
#ndvi_stack <- rast(ndvi_files)

# Reproject to UTM 37S (EPSG:32737)
#ndvi_stack_utm <- project(ndvi_stack, utm_crs)


## ----match-ndvi, message=FALSE, warning=FALSE-----------------------------------------------------------------------------------
# Ensure timestamp is POSIXct format with EAT timezone
WB_utm$date <- as.POSIXct(WB_utm$date, format = "%Y-%m-%d %H:%M:%S", tz="EAT")

# Function to find the nearest NDVI date
find_nearest_date <- function(date, readable_ndvi_dates) {
  diffs <- abs(difftime(readable_ndvi_dates, date, units = "days"))
  readable_ndvi_dates[which.min(diffs)]
}

# Find the nearest NDVI date for each timestamp
nearest_dates <- sapply(WB_utm$date, find_nearest_date, readable_ndvi_dates)

# Extract NDVI values for each timestamp
ndvi_values_list <- lapply(1:nlayers(ndvi_stack_utm), function(i) {
  extract(ndvi_stack_utm[[i]], WB_sp)
})

# Add NDVI values to the animal movement data
WB_utm$NDVI <- NA

for (i in 1:nrow(WB_utm)) {
  ndvi_layer_index <- which(readable_ndvi_dates == nearest_dates[i])
  if (length(ndvi_layer_index) > 0) {
    WB_utm$NDVI[i] <- ndvi_values_list[[ndvi_layer_index]][i]
  }
}

# Multiple the scale factor (0.0001) of the NDVI product MOD13Q1 V061
WB_utm$NDVI=WB_utm$NDVI*0.0001
# Check the data we made
head(WB_utm)


## ----save-ndvi, message=FALSE, warning=FALSE------------------------------------------------------------------------------------
# Save the combined data
save(WB_utm, file = "WB_environmental_information.RData")

