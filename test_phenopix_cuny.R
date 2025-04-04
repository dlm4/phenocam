# test_phenopix_cuny.R
#library(usethis)
#use_github()

library(phenopix)
library(lubridate)
library(terra)
library(sf)
library(data.table)
library(tidyverse)
library(exactextractr)

# structureFolder()
# DrawMULTIROI()
# extractVIs()
# extractDateFilename()
# autoFilter()
# FitDoubleLogElmore()
# PhenoExtract()

# this is clunky and doesn't seem to work, so I'm going to do it manually
phenocam_polys <- DrawMULTIROI("/Users/dlm356/dlm356_files/nyc_trees/phenocam/cuny_source/phenocamdata_RGB_2022/cuny/2022/10/cuny_2022_10_19_115706.jpg",
                               "/Users/dlm356/dlm356_files/nyc_trees/phenocam/cuny_source/phenocom_rois",
                               nroi = 1, roi.names = c("tree_1"), file.type = ".jpg")


#data(bartlett2009)
#fit <- FitDoubleLogElmore(bartlett2009$gcc, bartlett2009$time.stamp)


img <- rast("/Users/dlm356/dlm356_files/nyc_trees/phenocam/cuny_source/phenocamdata_RGB_2022/cuny/2022/10/cuny_2022_10_19_115706.jpg")
plot(img)
img_flipped <- flip(img, direction = "vertical")
plot(img_flipped)

# Define tree polygons

# PC_Poly_ID_1, TNC_Poly_ID 298136, Fraxinus pennsylvanica - Green ash
x1 <- 550
x2 <- 570
y1 <- 550
y2 <- 570
coords01 <- matrix(c(x1,y1, x2,y1, x2,y2, x1,y2, x1,y1), ncol=2, byrow=TRUE)  # Define polygon coordinates

# PC_Poly_ID_2, TNC_Poly_ID 298129, Gleditsia triacanthos var. inermis - Thornless honeylocust
x1 <- 660
x2 <- 680
y1 <- 590
y2 <- 610
coords02 <- matrix(c(x1,y1, x2,y1, x2,y2, x1,y2, x1,y1), ncol=2, byrow=TRUE)

# PC_Poly_ID_3, TNC_Poly_ID 298202, Platanus x acerifolia - London planetree
x1 <- 510
x2 <- 530
y1 <- 310
y2 <- 330
coords03 <- matrix(c(x1,y1, x2,y1, x2,y2, x1,y2, x1,y1), ncol=2, byrow=TRUE)

# PC_Poly_ID_4, TNC_Poly_ID 298203, Acer platanoides - Norway maple
x1 <- 680
x2 <- 700
y1 <- 350
y2 <- 370
coords04 <- matrix(c(x1,y1, x2,y1, x2,y2, x1,y2, x1,y1), ncol=2, byrow=TRUE)

# PC_Poly_ID_5, TNC_Poly_ID 298196, Prunus serotina - black cherry
x1 <- 270
x2 <- 290
y1 <- 340
y2 <- 360
coords05 <- matrix(c(x1,y1, x2,y1, x2,y2, x1,y2, x1,y1), ncol=2, byrow=TRUE)

# PC_Poly_ID_6, TNC_Poly_ID 298165, Acer saccharinum - silver maple
x1 <- 780
x2 <- 800
y1 <- 530
y2 <- 550
coords06 <- matrix(c(x1,y1, x2,y1, x2,y2, x1,y2, x1,y1), ncol=2, byrow=TRUE)

# PC_Poly_ID_7, TNC_Poly_ID 298214, Quercus palustris - pin oak
x1 <- 790
x2 <- 810
y1 <- 240
y2 <- 260
coords07 <- matrix(c(x1,y1, x2,y1, x2,y2, x1,y2, x1,y1), ncol=2, byrow=TRUE)

# PC_Poly_ID_8, TNC_Poly_ID 298206, Fraxinus pennsylvanica - Green ash
x1 <- 1090
x2 <- 1110
y1 <- 400
y2 <- 420
coords08 <- matrix(c(x1,y1, x2,y1, x2,y2, x1,y2, x1,y1), ncol=2, byrow=TRUE)

# PC_Poly_ID_9, TNC_Poly_ID 298208, Quercus ellipsoidalis - northern pin oak
x1 <- 1180
x2 <- 1200
y1 <- 400
y2 <- 420
coords09 <- matrix(c(x1,y1, x2,y1, x2,y2, x1,y2, x1,y1), ncol=2, byrow=TRUE)

# PC_Poly_ID_10, TNC_Poly_ID 297183, Platanus x acerifolia - London planetree
x1 <- 80
x2 <- 90
y1 <- 600
y2 <- 610
coords10 <- matrix(c(x1,y1, x2,y1, x2,y2, x1,y2, x1,y1), ncol=2, byrow=TRUE)

# PC_Poly_ID_11, TNC_Poly_ID 298131, Quercus acutissima - sawtooth oak
x1 <- 90
x2 <- 100
y1 <- 575
y2 <- 585
coords11 <- matrix(c(x1,y1, x2,y1, x2,y2, x1,y2, x1,y1), ncol=2, byrow=TRUE)

# PC_Poly_ID_12, TNC_Poly_ID 298128, Platanus x acerifolia - London planetree
x1 <- 800
x2 <- 820
y1 <- 620
y2 <- 640
coords12 <- matrix(c(x1,y1, x2,y1, x2,y2, x1,y2, x1,y1), ncol=2, byrow=TRUE)

# PC_Poly_ID_13, TNC_Poly_ID 298217, Ulmus americana - American elm
x1 <- 970
x2 <- 990
y1 <- 160
y2 <- 180
coords13 <- matrix(c(x1,y1, x2,y1, x2,y2, x1,y2, x1,y1), ncol=2, byrow=TRUE)

# PC_Poly_ID_14, TNC_Poly_ID 298213, Quercus palustris - pin oak
x1 <- 1080
x2 <- 1100
y1 <- 310
y2 <- 330
coords14 <- matrix(c(x1,y1, x2,y1, x2,y2, x1,y2, x1,y1), ncol=2, byrow=TRUE)

# PC_Poly_ID_15, TNC_Poly_ID 298164, Zelkova serrata - Japanese zelkova
x1 <- 240
x2 <- 260
y1 <- 410
y2 <- 430
coords15 <- matrix(c(x1,y1, x2,y1, x2,y2, x1,y2, x1,y1), ncol=2, byrow=TRUE)

# PC_Poly_ID_16, TNC_Poly_ID 298133, Liriodendron tulipifera - tuliptree
x1 <- 130
x2 <- 150
y1 <- 545
y2 <- 565
coords16 <- matrix(c(x1,y1, x2,y1, x2,y2, x1,y2, x1,y1), ncol=2, byrow=TRUE)

# PC_Poly_ID_17, TNC_Poly_ID 298195, Platanus x acerifolia - London planetree
x1 <- 1040
x2 <- 1060
y1 <- 470
y2 <- 490
coords17 <- matrix(c(x1,y1, x2,y1, x2,y2, x1,y2, x1,y1), ncol=2, byrow=TRUE)

# PC_Poly_ID_18, TNC_Poly_ID 297189, Platanus x acerifolia - London planetree
x1 <- 905
x2 <- 925
y1 <- 610
y2 <- 630
coords18 <- matrix(c(x1,y1, x2,y1, x2,y2, x1,y2, x1,y1), ncol=2, byrow=TRUE)

polygon_sf <- st_sfc(st_polygon(list(coords01)),
                     st_polygon(list(coords02)),
                     st_polygon(list(coords03)),
                     st_polygon(list(coords04)),
                     st_polygon(list(coords05)),
                     st_polygon(list(coords06)),
                     st_polygon(list(coords07)),
                     st_polygon(list(coords08)),
                     st_polygon(list(coords09)),
                     st_polygon(list(coords10)),
                     st_polygon(list(coords11)),
                     st_polygon(list(coords12)),
                     st_polygon(list(coords13)),
                     st_polygon(list(coords14)),
                     st_polygon(list(coords15)),
                     st_polygon(list(coords16)),
                     st_polygon(list(coords17)),
                     st_polygon(list(coords18)))  # Convert to Simple Features (sf)

plot(img_flipped)
plot(polygon_sf, border = "green", lwd = 2, add = TRUE)


# 
masked_img_flipped <- mask(img_flipped, vect(polygon_sf))  # Mask the raster with the polygon
plot(masked_img_flipped)  # View the result
# 
# values_inside <- extract(img_flipped, vect(polygon_sf), fun = "mean")
# values_inside <- exact_extract(img_flipped, polygon_sf, fun = "mean")
# print(values_inside)

getExposure <- function(img_file_list, i){
  meta_file <- paste0(unlist(strsplit(img_file_list[i], "\\."))[1], ".meta")
  img_meta <- fread(meta_file)
  exposure_elements <- grep("exposure=", img_meta$`agc=1`, value = TRUE)
  exposure_string <- exposure_elements[!grepl("auto_exposure=", exposure_elements)]
  exposure <- as.numeric(unlist(strsplit(exposure_string, "="))[2])
  return(exposure)
}

setwd("/Users/dlm356/dlm356_files/nyc_trees/phenocam/cuny_source/phenocamdata_RGB_2022")
img_file_list <- list.files(pattern = glob2rx("*.jpg"), recursive = TRUE)

col_list <- c("PC_Poly_ID", "red", "green", "blue", "exposure","file")
img_data_rgb <- data.frame(matrix(data = NA, nrow = length(img_file_list)*length(polygon_sf), ncol = length(col_list)))
colnames(img_data_rgb) <- col_list

#img_data_rgb$PC_Poly_ID <- 1 # will update for multiple polygons

row_start <- 1
row_end <- length(polygon_sf)

for(i in 1:length(img_file_list)){
  print(i)
  img <- rast(img_file_list[i])
  img_flipped <- flip(img, direction = "vertical")
  img_data_rgb[row_start:row_end, 1] <- seq(1, length(polygon_sf))
  img_data_rgb[row_start:row_end, 2:4] <- exact_extract(img_flipped, polygon_sf, fun = "mean")
  img_data_rgb$file[row_start:row_end] <- unlist(strsplit(img_file_list[i], "/"))[4]
  img_data_rgb$exposure[row_start:row_end] <- getExposure(img_file_list, i)
  row_start <- row_start + length(polygon_sf)
  row_end <- row_end + length(polygon_sf)
}

#img_data_rgb$gcc <- img_data_rgb$green / (img_data_rgb$red + img_data_rgb$green + img_data_rgb$blue)

# CNIR MONO
setwd("/Users/dlm356/dlm356_files/nyc_trees/phenocam/cuny_source/phenocamdata_IR_2022")
img_file_list <- list.files(pattern = glob2rx("*.jpg"), recursive = TRUE)

col_list <- c("PC_Poly_ID", "cnir_mono", "exposure", "file")
img_data_ir <- data.frame(matrix(data = NA, nrow = length(img_file_list)*length(polygon_sf), ncol = length(col_list)))
colnames(img_data_ir) <- col_list

row_start <- 1
row_end <- length(polygon_sf)

for(i in 1:length(img_file_list)){
  print(i)
  img <- rast(img_file_list[i])
  img_flipped <- flip(img, direction = "vertical")
  img_data_ir[row_start:row_end, 1] <- seq(1, length(polygon_sf))
  img_data_ir[row_start:row_end, 2] <- exact_extract(img_flipped, polygon_sf, fun = "mean")
  img_data_ir$file[row_start:row_end] <- unlist(strsplit(img_file_list[i], "/"))[4]
  img_data_ir$exposure[row_start:row_end] <- getExposure(img_file_list, i)
  row_start <- row_start + length(polygon_sf)
  row_end <- row_end + length(polygon_sf)
}

# NEED to merge by date time, some might be missing between the two
img_data_rgb$datetime_string <- substr(img_data_rgb$file, 6, 22)
img_data_ir$datetime_string <- substr(img_data_ir$file, 9, 25)

img_data_rgb$datetime_string_poly <- paste0(img_data_rgb$datetime_string, "_poly", img_data_rgb$PC_Poly_ID)
img_data_ir$datetime_string_poly <- paste0(img_data_ir$datetime_string, "_poly", img_data_ir$PC_Poly_ID)

img_data_merged <- merge(img_data_rgb, img_data_ir, by = "datetime_string_poly")
# fix column names
colnames(img_data_merged)[which(colnames(img_data_merged) == "exposure.x")] <- "exposure_rgb"
colnames(img_data_merged)[which(colnames(img_data_merged) == "exposure.y")] <- "exposure_ir"
colnames(img_data_merged)[which(colnames(img_data_merged) == "file.x")] <- "file_rgb"
colnames(img_data_merged)[which(colnames(img_data_merged) == "file.y")] <- "file_ir"
colnames(img_data_merged)[which(colnames(img_data_merged) == "PC_Poly_ID.x")] <- "PC_Poly_ID"
colnames(img_data_merged)[which(colnames(img_data_merged) == "datetime_string.x")] <- "datetime_string"
img_data_merged <- img_data_merged %>% select(-c(PC_Poly_ID.y, datetime_string.y))

# GCC
img_data_merged$gcc <- img_data_merged$green / (img_data_merged$red + img_data_merged$green + img_data_merged$blue)

# NDVIc
Rdn <- img_data_merged$red
Gdn <- img_data_merged$green
Bdn <- img_data_merged$blue
Zdn <- img_data_merged$cnir_mono
Ez <- img_data_merged$exposure_ir
Ey <- img_data_merged$exposure_rgb

Ydn <- 0.3*Rdn + 0.59*Gdn + 0.11*Bdn # visible sum
Z_p_dn <- Zdn / sqrt(Ez) # exposure adjusted rgb+ir
R_p_dn <- Rdn / sqrt(Ey) # exposure adjusted red
Y_p_dn <- Ydn / sqrt(Ey) # exposure adjusted rgb (visible sum)
X_p_dn <- Z_p_dn - Y_p_dn # exposure adjusted nir only
NDVIc <- (X_p_dn - R_p_dn)/(X_p_dn + R_p_dn)

img_data_merged$NDVIc <- NDVIc

# Will likely need to go back and might need to include more metadata for filtering and retention of good observations
dt_split <- strsplit(img_data_merged$datetime_string, "_")
img_data_merged$datetime <- lubridate::ymd_hms(paste0(sapply(dt_split, '[[', 1), sapply(dt_split, '[[', 2), sapply(dt_split, '[[', 3), "_", sapply(dt_split, '[[', 4)))

ggplot(img_data_merged, aes(x = datetime, y = gcc)) +
  geom_point(aes(col = as.factor(PC_Poly_ID)), alpha = 0.1) +
  #geom_smooth(aes(col = as.factor(PC_Poly_ID))) +
  facet_wrap(~as.factor(PC_Poly_ID)) +
  theme_bw()

ggplot(img_data_merged, aes(x = datetime, y = NDVIc)) +
  geom_point(aes(col = as.factor(PC_Poly_ID)), alpha = 0.1) +
  #geom_smooth(aes(col = as.factor(PC_Poly_ID))) +
  facet_wrap(~as.factor(PC_Poly_ID)) +
  theme_bw()
