library(sp)
library(rgdal)
library(dplyr)
library(sf)
library(tidyverse)
library(RSQLite)
library(terra)
library(tiff)
library(raster)
library(leaflet)
library(ggplot2)
library(dismo)
library(exactextractr)
library(rgeos)
library(hsdar)
library(EBImage)
library(RStoolbox)
library(gridExtra)
library(magick)


#BERLIN
# We have add image folder 3 and 4 because for our study area we download 2 different images 
# which are combined later on this R studio file
image_folder3 <- "C:\\Users\\magai\\Desktop\\master thesis\\sentinell2\\S2B_MSIL2A_20230508T100559_N0509_R022_T33UVU_20230508T131238.Berlin\\GRANULE\\L2A_T33UVU_A032223_20230508T101125\\IMG_DATA\\R20m"
image_folder4 <- "C:\\Users\\magai\\Desktop\\master thesis\\sentinell2\\S2B_MSIL2A_20230508T100559_N0509_R022_T33UVU_20230508T131238.Berlin\\GRANULE\\L2A_T33UVU_A032223_20230508T101125\\IMG_DATA\\R20m"

# Fucntion to load all the Sentinel-2 images without any error
load_raster_with_error_handling <- function(file_path) {
  tryCatch(
    {
      raster::raster(file_path)
    },
    error = function(e) {
      cat("Error loading file:", file_path, "\n")
      stop(e)
    },
    warning = function(w) {
      cat("Warning while loading file:", file_path, "\n")
      print(w)
    }
  )
} 
# Load the Sentinel-2B bands as a raster brick
berlin1 <- stack(lapply(list.files(image_folder3, pattern = "\\.jp2$", full.names = TRUE), load_raster_with_error_handling))
berlin2 <- stack(lapply(list.files(image_folder4, pattern = "\\.jp2$", full.names = TRUE), load_raster_with_error_handling))

# Merging both images into one
# Merging both images into one
x1 <- list(berlin1, berlin2)
names(x1) <- c("x", "y")
berlin <- do.call(merge, x1)


plotRGB(berlin, r = 1, g = 2, b = 3, axes = TRUE, stretch = "lin", main = "Sentinel Subset") #first plot the images

nlayers(berlin) # Check the image layers

# Add the  gpkg of the study area 
berlin_gpkg <- st_read("C:\\Users\\magai\\Desktop\\master thesis\\study area\\gebiet_sch.berlin.gpkg") %>% st_transform(32633)

st_crs(berlin_gpkg) == st_crs(berlin)

# Define the coordinates of  study area (xmin, xmax, ymin, ymax)

# Define the study area extent to crop the image with the gpkg file

study_area_extent <- c(391858.5   , 443268.6   , 5850119   , 5897408)

cropped_image_berlin <- crop(berlin, study_area_extent)

nlayers(cropped_image_berlin)

#save the new crop image
output_file <- "C:\\Users\\magai\\Desktop\\master thesis\\sentinell2\\S2B_MSIL2A_20230508T100559_N0509_R022_T33UVU_20230508T131238.Berlin\\GRANULE"

# Save the cropped image as a GeoTIFF
writeRaster(cropped_image_berlin, filename = output_file, format = "GTiff", overwrite = TRUE)

# Confirmation message
cat("Cropped image saved successfully at:", output_file, "\n") #success

# Load the cropped image 
cropped_image_berlin <- rast("C:\\Users\\magai\\Desktop\\master thesis\\sentinell2\\S2B_MSIL2A_20230508T100559_N0509_R022_T33UVU_20230508T131238.Berlin\\GRANULE\\cropped image berlin 20m.tif")

cropped_image_berlin <- cropped_image_berlin/10000
# Plot the study area (bounding box)
plot(berlin_gpkg, add = TRUE, lwd = 2, border = "red")
#############################################################

plotRGB(cropped_image_berlin, r = 1, g = 2, b = 3, axes = TRUE, stretch = "lin", main = "Sentinel Subset")
plot(selected_points, col = "red" , cex = 1, pch = 16,  add = T)

#Bexis_tree data (reference data) 
# Bexis tree data are tree measurements from the study area
bexis_tree <- st_read("C:\\Users\\magai\\Desktop\\master thesis\\bexis_trees\\bexis_trees.shp")

plotRGB(cropped_image_berlin, r = 3, g = 2, b = 1, axes = TRUE, stretch = "lin", main = "Senitnel True Color Composite")

bexis_tree <- read.csv("C:\\Users\\magai\\Desktop\\master thesis\\bexis_trees\\bexis_tree_new.csv")

random_points_new <- # Filter the data for "Explorator SCH" in the second column (assuming the column name is "Field2")

# We choose only the measurements for Berlin area
filtered_data <- subset(bexis_tree, fid == "SCH")

csv_file_path <- "C:\\Users\\magai\\Desktop\\master thesis\\sentinell2\\S2B_MSIL2A_20230508T100559_N0509_R022_T33UVU_20230508T131238.Berlin\\GRANULE\\random_points_new.csv"
write.csv(random_points_new, file = csv_file_path, row.names = FALSE)

# Function to classify the tree species into conifers and broadleaves
classify_tree <- function(tree_species) {
  # Define a vector of conifer species names
  conifer_species <- c("Picea_abies", "Pinus_sylvestris", "Abies_alba", "Larix_decidua", "Pseudotsuga_menziesii")
  
  # Use ifelse to handle vector inputs
  category <- ifelse(tree_species %in% conifer_species, "conifers", "broadleaves")
  
  return(category)
}

# Create a new column in our data with the category of the trees
bexis_tree <- bexis_tree %>%
  mutate(category = classify_tree(species))


selected_points <- bexis_tree[bexis_tree$Explorator == "SCH",] %>% st_transform(32633)
selected_points <- bexis_tree_data[bexis_tree_data$Explorator == "SCH", ] 

selected_points <- selected_points[selected_points$fid != 14, ]

st_crs(cropped_image_berlin) == st_crs(selected_points)
#selected_points

random_points <- selected_points %>%
  group_by(EP) %>%
  sample_n(points_per_plot)

# Function to calculate basel area
calculate_basal_area <- function(df) {
  # Create an empty vector to store the calculated basel area
  basal_area <- numeric(length = nrow(df))
  
  # Iterate over each row in the data frame
  for (i in 1:nrow(df)) {
    # Calculate basel area based on the dbh using the equation dbh^2 * 3.14 / 4
    basal_area[i] <- (df$d[i]^2) * 3.142 / 4
  }
  
  # Return the calculated basel area vector
  return(basal_area)
}


# Call the calculate_basel_area function
basal_area <- calculate_basal_area(bexis_tree)

# Add the calculated basel area as a new column to the data frame
bexis_tree$basal_area <- basal_area

# Print the updated data frame with the basel area column
print(bexis_tree)


library(readxl)

# Convert the data frame to an sf object
#bexis_tree_sf <- st_as_sf(bexis_tree, coords = c("x", "y"), crs = 4326)

# Set the file path for the output CSV file
output_csv <- "C:\\Users\\magai\\Desktop\\master thesis\\sentinell2\\S2B_MSIL2A_20230508T100559_N0509_R022_T33UVU_20230508T131238.Berlin\\GRANULE\\random_points_new.cvs"

# Save the sf object as a CSV file
write.csv(bexis_tree, file = output_csv, row.names = FALSE)

# Convert to CSV
csv_file_path <- "C:\\Users\\magai\\Desktop\\master thesis\\sentinell2\\S2B_MSIL2A_20230504T102559_STUTTGART\\random_points.csv"
write.csv(random_points, file = csv_file_path, row.names = FALSE)

#New image 
csv_file_path <- "C:\\Users\\magai\\Desktop\\master thesis\\sentinell2\\S2B_MSIL2A_20230508T100559_N0509_R022_T33UVU_20230508T131238.Berlin\\GRANULE\\random_points.csv"
write.csv(selected_points, file = csv_file_path, row.names = FALSE)

#file.choose()
#random_points <- read.csv("C:\\Users\\magai\\Desktop\\master thesis\\sentinell2\\S2B_MSIL2A_20230613T102609_N0509_R108_T32UNU_20230613T151004.SAFE\\random_points.csv")
# Extract values from the random points
df <- extract(cropped_image_stuttgard ,random_points)

df <- extract(cropped_image_berlin, selected_points)
#names(cropped_image_stuttgard) <- c("Coastal Aerosol", "blue", "green", "red", 'rededge1','rededge2','rededge3','NIR','SWIR1','SWIR2')
names(df) <- c("Coastal Aerosol", "blue", "green", "red", 'rededge1','rededge2','rededge3','NIR','SWIR1','SWIR2')
head(df)
df <- df[ ,2:10]
csv_file_path <- "C:\\Users\\magai\\Desktop\\master thesis\\sentinell2\\S2B_MSIL2A_20230508T100559_N0509_R022_T33UVU_20230508T131238.Berlin\\GRANULE\\random_points_df.csv"
write.csv(df, file = csv_file_path, row.names = FALSE)

# Extract values from the random points
df <- extract(cropped_image_berlin ,random_points)
names(df) <- c('blue','green','red','rededge1','rededge2','rededge3','NIR','rededge4','SWIR1','SWIR2')
colnames(df) <- c("Coastal Aerosol", "blue", "green", "red", 'rededge1','rededge2','rededge3','NIR','SWIR1','SWIR2')


head(df)

df <- round(df)
# Create an empty data frame to store the mean spectra
# Convert values in df to numeric
df <- apply(df, 2, function(x) as.numeric(as.character(x)))
# Create an empty data frame to store the mean spectra
ms <- matrix(NA, nrow = nrow(df), ncol = ncol(df))

for (i in 1:nrow(df)) {
  x <- df[i, ]
  ms[i, ] <- colMeans(matrix(x, nrow = 1))
}

# Specify the row names
rownames(ms) <- 1:nrow(df)
colnames(ms) <- colnames(df)

# We will create a vector of color for the land use land cover classes and will reuse it for other plots
mycolor <- c('cyan', 'darkgreen', 'yellow', 'burlywood', 'darkred', 'darkgray', 'blue', 'lightgreen')

# Create an empty plot
plot(1, ylim = c(0, max(df)), xlim = c(1, ncol(df)), xlab = "Bands", ylab = "Reflectance", xaxt = 'n')


# Custom X-axis
axis(1, at = 1:ncol(df), lab = colnames(df))

plot(1, ylim = c(0, 1), xlim = c(1, ncol(df)), xlab = "Bands", ylab = "Reflectance", xaxt = 'n')
plot(1, ylim = c(0, max(df, na.rm = TRUE)), xlim = c(1, ncol(df)), xlab = "Bands", ylab = "Reflectance", xaxt = 'n')
# Add the spectra
for (i in 1:nrow(df)) {
  lines(df[i, ], type = "o", lwd = 2, col = mycolor[i])
}

title(main = "Spectral Profile", font.main = 2)  # Title
legend("topleft", 1:nrow(ms), cex = 0.8, col = mycolor, lty = 1, lwd = 3, bty = "n")  # Finally the legend


# Basic mathematical operations for NDVI 
vi <- function(img, i, k) {
  bi <- img[[i]]
  bk <- img[[k]]
  vi <- (bk - bi) / (bk + bi)
  return(vi)
}

# For Sentinel NIR = 7, red = 3.
ndvi <- vi(cropped_image_berlin, 3, 7)
plot(ndvi, col = rev(terrain.colors(30)), main = 'NDVI from Sentinel') #until here it work

