# phenocam_cuny_compare.R

#library(phenopix)
library(lubridate)
library(terra)
library(sf)
library(data.table)
library(tidyverse)
library(exactextractr)
library(phenofit)

img <- rast("/Users/dlm356/dlm356_files/nyc_trees/phenocam/cuny_source/phenocamdata_RGB_2022_0800_1600/cuny/2022/10/cuny_2022_10_19_115706.jpg")
plot(img)
img_flipped <- flip(img, direction = "vertical")
plot(img_flipped)

# Define tree polygons

# # PC_Poly_ID_1, TNC_Poly_ID 298136, Fraxinus pennsylvanica - Green ash
# x1 <- 550
# x2 <- 570
# y1 <- 550
# y2 <- 570
# coords01 <- matrix(c(x1,y1, x2,y1, x2,y2, x1,y2, x1,y1), ncol=2, byrow=TRUE)  # Define polygon coordinates

# # PC_Poly_ID_2, TNC_Poly_ID 298129, Gleditsia triacanthos var. inermis - Thornless honeylocust
# x1 <- 660
# x2 <- 680
# y1 <- 590
# y2 <- 610
# coords02 <- matrix(c(x1,y1, x2,y1, x2,y2, x1,y2, x1,y1), ncol=2, byrow=TRUE)

# PC_Poly_ID_3, TNC_Poly_ID 298202, Platanus x acerifolia - London planetree
x1 <- 510
x2 <- 530
y1 <- 310
y2 <- 330
coords03 <- matrix(c(x1,y1, x2,y1, x2,y2, x1,y2, x1,y1), ncol=2, byrow=TRUE)

# # PC_Poly_ID_4, TNC_Poly_ID 298203, Acer platanoides - Norway maple
# x1 <- 680
# x2 <- 700
# y1 <- 350
# y2 <- 370
# coords04 <- matrix(c(x1,y1, x2,y1, x2,y2, x1,y2, x1,y1), ncol=2, byrow=TRUE)

# # PC_Poly_ID_5, TNC_Poly_ID 298196, Prunus serotina - black cherry
# x1 <- 270
# x2 <- 290
# y1 <- 340
# y2 <- 360
# coords05 <- matrix(c(x1,y1, x2,y1, x2,y2, x1,y2, x1,y1), ncol=2, byrow=TRUE)

# PC_Poly_ID_6, TNC_Poly_ID 298165, Acer saccharinum - silver maple
x1 <- 780
x2 <- 800
y1 <- 530
y2 <- 550
coords06 <- matrix(c(x1,y1, x2,y1, x2,y2, x1,y2, x1,y1), ncol=2, byrow=TRUE)

# # PC_Poly_ID_7, TNC_Poly_ID 298214, Quercus palustris - pin oak
# x1 <- 790
# x2 <- 810
# y1 <- 240
# y2 <- 260
# coords07 <- matrix(c(x1,y1, x2,y1, x2,y2, x1,y2, x1,y1), ncol=2, byrow=TRUE)

# # PC_Poly_ID_8, TNC_Poly_ID 298206, Fraxinus pennsylvanica - Green ash
# x1 <- 1090
# x2 <- 1110
# y1 <- 400
# y2 <- 420
# coords08 <- matrix(c(x1,y1, x2,y1, x2,y2, x1,y2, x1,y1), ncol=2, byrow=TRUE)

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

# # PC_Poly_ID_11, TNC_Poly_ID 298131, Quercus acutissima - sawtooth oak
x1 <- 95
x2 <- 105
y1 <- 565
y2 <- 575
coords11 <- matrix(c(x1,y1, x2,y1, x2,y2, x1,y2, x1,y1), ncol=2, byrow=TRUE)

# PC_Poly_ID_12, TNC_Poly_ID 298128, Platanus x acerifolia - London planetree
x1 <- 800
x2 <- 820
y1 <- 620
y2 <- 640
coords12 <- matrix(c(x1,y1, x2,y1, x2,y2, x1,y2, x1,y1), ncol=2, byrow=TRUE)

# # PC_Poly_ID_13, TNC_Poly_ID 298217, Ulmus americana - American elm
# x1 <- 970
# x2 <- 990
# y1 <- 160
# y2 <- 180
# coords13 <- matrix(c(x1,y1, x2,y1, x2,y2, x1,y2, x1,y1), ncol=2, byrow=TRUE)

# # PC_Poly_ID_14, TNC_Poly_ID 298213, Quercus palustris - pin oak
# x1 <- 1080
# x2 <- 1100
# y1 <- 310
# y2 <- 330
# coords14 <- matrix(c(x1,y1, x2,y1, x2,y2, x1,y2, x1,y1), ncol=2, byrow=TRUE)

# # PC_Poly_ID_15, TNC_Poly_ID 298164, Zelkova serrata - Japanese zelkova
# x1 <- 240
# x2 <- 260
# y1 <- 410
# y2 <- 430
# coords15 <- matrix(c(x1,y1, x2,y1, x2,y2, x1,y2, x1,y1), ncol=2, byrow=TRUE)

# # PC_Poly_ID_16, TNC_Poly_ID 298133, Liriodendron tulipifera - tuliptree
# x1 <- 130
# x2 <- 150
# y1 <- 545
# y2 <- 565
# coords16 <- matrix(c(x1,y1, x2,y1, x2,y2, x1,y2, x1,y1), ncol=2, byrow=TRUE)

# PC_Poly_ID_17, TNC_Poly_ID 298195, Platanus x acerifolia - London planetree
# x1 <- 1040
# x2 <- 1060
# y1 <- 470
# y2 <- 490
x1 <- 1020
x2 <- 1040
y1 <- 490
y2 <- 510
coords17 <- matrix(c(x1,y1, x2,y1, x2,y2, x1,y2, x1,y1), ncol=2, byrow=TRUE)

# PC_Poly_ID_18, TNC_Poly_ID 297189, Platanus x acerifolia - London planetree
x1 <- 905
x2 <- 925
y1 <- 610
y2 <- 630
coords18 <- matrix(c(x1,y1, x2,y1, x2,y2, x1,y2, x1,y1), ncol=2, byrow=TRUE)

# this was the wrong tree
# # PC_Poly_ID_19, TNC_Poly_ID 298175, Platanus x acerifolia - London planetree
# x1 <- 50
# x2 <- 70
# y1 <- 420
# y2 <- 440
# coords19 <- matrix(c(x1,y1, x2,y1, x2,y2, x1,y2, x1,y1), ncol=2, byrow=TRUE)

# PC_Poly_ID_20 298163, Tilia cordata - littleleaf linden
x1 <- 960
x2 <- 970
y1 <- 530
y2 <- 540
coords20 <- matrix(c(x1,y1, x2,y1, x2,y2, x1,y2, x1,y1), ncol=2, byrow=TRUE)

# PC_Poly_ID_21 298180, Quercus coccinea - scarlet oak
x1 <- 1080
x2 <- 1090
y1 <- 510
y2 <- 520
coords21 <- matrix(c(x1,y1, x2,y1, x2,y2, x1,y2, x1,y1), ncol=2, byrow=TRUE)

# PC_Poly_ID_22 298057, Platanus x acerifolia - London planetree
x1 <- 270
x2 <- 280
y1 <- 700
y2 <- 710
coords22 <- matrix(c(x1,y1, x2,y1, x2,y2, x1,y2, x1,y1), ncol=2, byrow=TRUE)

# # PC_Poly_ID_23 298204, Quercus ellipsoidalis - northern pin oak
# x1 <- 960
# x2 <- 980
# y1 <- 370
# y2 <- 390
# coords23 <- matrix(c(x1,y1, x2,y1, x2,y2, x1,y2, x1,y1), ncol=2, byrow=TRUE)

# # PC_Poly_ID_24 298150, Acer saccharinum - silver maple
# x1 <- 270
# x2 <- 290
# y1 <- 490
# y2 <- 510
# coords24 <- matrix(c(x1,y1, x2,y1, x2,y2, x1,y2, x1,y1), ncol=2, byrow=TRUE)

# # PC_Poly_ID_25 298040,  Quercus palustris - pin oak
# x1 <- 460
# x2 <- 470
# y1 <- 730
# y2 <- 740
# coords25 <- matrix(c(x1,y1, x2,y1, x2,y2, x1,y2, x1,y1), ncol=2, byrow=TRUE)

# # PC_Poly_ID_26 297336, Platanus x acerifolia - London planetree
x1 <- 855
x2 <- 865
y1 <- 660
y2 <- 670
coords26 <- matrix(c(x1,y1, x2,y1, x2,y2, x1,y2, x1,y1), ncol=2, byrow=TRUE)

# # PC_Poly_ID_27 298139, Platanus x acerifolia - London planetree
x1 <- 15
x2 <- 25
y1 <- 530
y2 <- 540
coords27 <- matrix(c(x1,y1, x2,y1, x2,y2, x1,y2, x1,y1), ncol=2, byrow=TRUE)

# # PC_Poly_ID_28
x1 <- 10
x2 <- 30
y1 <- 200
y2 <- 220
coords28 <- matrix(c(x1,y1, x2,y1, x2,y2, x1,y2, x1,y1), ncol=2, byrow=TRUE)

# # PC_Poly_ID_29
x1 <- 560
x2 <- 580
y1 <- 10
y2 <- 30
coords29 <- matrix(c(x1,y1, x2,y1, x2,y2, x1,y2, x1,y1), ncol=2, byrow=TRUE)

polygon_sf <- st_sfc(#st_polygon(list(coords01)),
                     #st_polygon(list(coords02)),
                     st_polygon(list(coords03)),
                     #st_polygon(list(coords04)),
                     #st_polygon(list(coords05)),
                     st_polygon(list(coords06)),
                     #st_polygon(list(coords07)),
                     #st_polygon(list(coords08)),
                     st_polygon(list(coords09)),
                     st_polygon(list(coords10)),
                     st_polygon(list(coords11)),
                     st_polygon(list(coords12)),
                     #st_polygon(list(coords13)),
                     #st_polygon(list(coords14)),
                     #st_polygon(list(coords15)),
                     #st_polygon(list(coords16)),
                     st_polygon(list(coords17)),
                     st_polygon(list(coords18)),
                     #st_polygon(list(coords19)),
                     st_polygon(list(coords20)),
                     st_polygon(list(coords21)),
                     st_polygon(list(coords22)),
                     #st_polygon(list(coords23)),
                     #st_polygon(list(coords24)),
                     #st_polygon(list(coords25)),  # Convert to Simple Features (sf)
                     st_polygon(list(coords26)),
                     st_polygon(list(coords27)),
                     st_polygon(list(coords28)),
                     st_polygon(list(coords29)))  # Convert to Simple Features (sf)

polygon_sf <- st_as_sf(polygon_sf)
polygon_sf$poly_id <- as.character(c(3, 6, 9, 10, 11, 12, 17, 18, 20, 21, 22, 26, 27, 28, 29))

plot(img_flipped)
plot(polygon_sf, border = "green", fill = NA, lwd = 2, add = TRUE)
text(st_coordinates(st_centroid(polygon_sf)), labels = polygon_sf$poly_id, cex = 1, col = "white")

# 
masked_img_flipped <- mask(img_flipped, vect(polygon_sf))  # Mask the raster with the polygon
plot(masked_img_flipped)  # View the result

getExposure <- function(img_file_list, i){
  meta_file <- paste0(unlist(strsplit(img_file_list[i], "\\."))[1], ".meta")
  img_meta <- fread(meta_file)
  exposure_elements <- grep("exposure=", img_meta$`agc=1`, value = TRUE)
  exposure_string <- exposure_elements[!grepl("auto_exposure=", exposure_elements)]
  exposure <- as.numeric(unlist(strsplit(exposure_string, "="))[2])
  return(exposure)
}

setwd("/Users/dlm356/dlm356_files/nyc_trees/phenocam/cuny_source/phenocamdata_RGB_2022_0800_1600")
img_file_list <- list.files(pattern = glob2rx("*.jpg"), recursive = TRUE)

col_list <- c("PC_Poly_ID", "red", "green", "blue", "exposure","file")
img_data_rgb <- data.frame(matrix(data = NA, nrow = length(img_file_list)*nrow(polygon_sf), ncol = length(col_list))) # needed to switch length of polygon_sf to nrow
colnames(img_data_rgb) <- col_list

#img_data_rgb$PC_Poly_ID <- 1 # will update for multiple polygons

row_start <- 1
row_end <- nrow(polygon_sf)

for(i in 1:length(img_file_list)){
  print(i)
  img <- rast(img_file_list[i])
  img_flipped <- flip(img, direction = "vertical")
  #img_data_rgb[row_start:row_end, 1] <- seq(1, nrow(polygon_sf))
  img_data_rgb[row_start:row_end, 1:4] <- exact_extract(img_flipped, polygon_sf, fun = "mean", append_cols = "poly_id")
  img_data_rgb$file[row_start:row_end] <- unlist(strsplit(img_file_list[i], "/"))[4]
  img_data_rgb$exposure[row_start:row_end] <- getExposure(img_file_list, i)
  row_start <- row_start + nrow(polygon_sf)
  row_end <- row_end + nrow(polygon_sf)
}

#img_data_rgb$gcc <- img_data_rgb$green / (img_data_rgb$red + img_data_rgb$green + img_data_rgb$blue)

# CNIR MONO
setwd("/Users/dlm356/dlm356_files/nyc_trees/phenocam/cuny_source/phenocamdata_IR_2022_0800_1600")
img_file_list <- list.files(pattern = glob2rx("*.jpg"), recursive = TRUE)

col_list <- c("PC_Poly_ID", "cnir_mono", "exposure", "file")
img_data_ir <- data.frame(matrix(data = NA, nrow = length(img_file_list)*nrow(polygon_sf), ncol = length(col_list)))
colnames(img_data_ir) <- col_list

row_start <- 1
row_end <- nrow(polygon_sf)

for(i in 1:length(img_file_list)){
  print(i)
  img <- rast(img_file_list[i])
  img_flipped <- flip(img, direction = "vertical")
  #img_data_ir[row_start:row_end, 1] <- seq(1, length(polygon_sf))
  img_data_ir[row_start:row_end, 1:2] <- exact_extract(img_flipped, polygon_sf, fun = "mean", append_cols = "poly_id")
  img_data_ir$file[row_start:row_end] <- unlist(strsplit(img_file_list[i], "/"))[4]
  img_data_ir$exposure[row_start:row_end] <- getExposure(img_file_list, i)
  row_start <- row_start + nrow(polygon_sf)
  row_end <- row_end + nrow(polygon_sf)
}

# NEED to merge by date time, some might be missing between the two
img_data_rgb$datetime_string <- substr(img_data_rgb$file, 6, 22)
img_data_ir$datetime_string <- substr(img_data_ir$file, 9, 25)

img_data_rgb$datetime_string_poly <- paste0(img_data_rgb$datetime_string, "_poly", img_data_rgb$PC_Poly_ID)
img_data_ir$datetime_string_poly <- paste0(img_data_ir$datetime_string, "_poly", img_data_ir$PC_Poly_ID)

for (i in 1:nrow(img_data_rgb)){
  if(nchar(img_data_rgb$datetime_string_poly[i]) < 24){
    img_data_rgb$datetime_string_poly[i] <- paste0(substr(img_data_rgb$datetime_string_poly[i], 1, 22), "0", substr(img_data_rgb$datetime_string_poly[i], 23, 23))
  }
}

for (i in 1:nrow(img_data_ir)){
  if(nchar(img_data_ir$datetime_string_poly[i]) < 24){
    img_data_ir$datetime_string_poly[i] <- paste0(substr(img_data_ir$datetime_string_poly[i], 1, 22), "0", substr(img_data_ir$datetime_string_poly[i], 23, 23))
  }
}

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
X_p_dn[X_p_dn < 0] <- NA # remove any adjusted NIR that are lower than possible from total
NDVIc <- (X_p_dn - R_p_dn)/(X_p_dn + R_p_dn)

img_data_merged$NDVIc <- NDVIc

# filter for bright or dark images, Richardson et al 2018
img_data_merged$sum_rgb <- img_data_merged$red + img_data_merged$green + img_data_merged$blue
img_data_merged_bf <- img_data_merged %>% filter(sum_rgb <= 665 & sum_rgb >= 100)

# Will likely need to go back and might need to include more metadata for filtering and retention of good observations
dt_split <- strsplit(img_data_merged_bf$datetime_string, "_")
img_data_merged_bf$datetime <- lubridate::ymd_hms(paste0(sapply(dt_split, '[[', 1), sapply(dt_split, '[[', 2), sapply(dt_split, '[[', 3), "_", sapply(dt_split, '[[', 4)))

ggplot(img_data_merged_bf, aes(x = datetime, y = gcc)) +
  geom_point(aes(col = as.factor(PC_Poly_ID)), alpha = 0.1) +
  #geom_smooth(aes(col = as.factor(PC_Poly_ID))) +
  facet_wrap(~as.factor(PC_Poly_ID), ncol = 5) +
  theme_bw()

ggplot(img_data_merged_bf, aes(x = datetime, y = NDVIc)) +
  geom_point(aes(col = as.factor(PC_Poly_ID)), alpha = 0.1) +
  #geom_smooth(aes(col = as.factor(PC_Poly_ID))) +
  facet_wrap(~as.factor(PC_Poly_ID), ncol = 5) +
  theme_bw()

# time range to get data
daterange <- interval(ymd_hms("20220101 00:00:01"), ymd_hms("20221231 23:59:59"))

unq_polys <- unique(img_data_merged_bf$PC_Poly_ID)

doy <- rep(1:365, length(unq_polys)) # for 2022
poly_ids <- rep(unq_polys, each = 365)
df_smoothed_dates <- cbind.data.frame(doy, poly_ids)
df_smoothed_dates$gcc90 <- NA
df_smoothed_dates$NDVIc90 <- NA
df_smoothed_dates$gcc75 <- NA
df_smoothed_dates$NDVIc75 <- NA
df_smoothed_dates$gcc50 <- NA
df_smoothed_dates$NDVIc50 <- NA

for (id_ind in 1:length(unq_polys)){
  date_start <- ymd_hms("20220101 00:00:01")
  date_end <- ymd_hms("20220103 23:59:59")
  id_inds <- which(df_smoothed_dates$poly_ids == unq_polys[id_ind])
  
  for (i in 2:364){
    daterange <- interval(date_start, date_end)
    
    img_data_merged_bf_sub <- img_data_merged_bf[which(img_data_merged_bf$datetime %within% daterange & img_data_merged_bf$PC_Poly_ID == unq_polys[id_ind]),]
    
    df_smoothed_dates$gcc90[id_inds[i]] <- quantile(img_data_merged_bf_sub$gcc, c(0.9), na.rm = TRUE)
    df_smoothed_dates$NDVIc90[id_inds[i]] <- quantile(img_data_merged_bf_sub$NDVIc, c(0.9), na.rm = TRUE)
    
    df_smoothed_dates$gcc75[id_inds[i]] <- quantile(img_data_merged_bf_sub$gcc, c(0.75), na.rm = TRUE)
    df_smoothed_dates$NDVIc75[id_inds[i]] <- quantile(img_data_merged_bf_sub$NDVIc, c(0.75), na.rm = TRUE)
    
    df_smoothed_dates$gcc50[id_inds[i]] <- quantile(img_data_merged_bf_sub$gcc, c(0.5), na.rm = TRUE)
    df_smoothed_dates$NDVIc50[id_inds[i]] <- quantile(img_data_merged_bf_sub$NDVIc, c(0.5), na.rm = TRUE)
    
    date_start <- date_start + days(1)
    date_end <- date_end + days(1)
  }
}

df_smoothed_dates$date <- ymd("20220101") + days(df_smoothed_dates$doy - 1)

df_smoothed_dates$gcc <- df_smoothed_dates$gcc90
df_smoothed_dates$NDVIc <- df_smoothed_dates$NDVIc90

ggplot(img_data_merged_bf %>% filter(PC_Poly_ID == 6), aes(x = as.Date(datetime), y = gcc)) +
  geom_point(alpha = 0.5, col = "red") +
  geom_line(data = df_smoothed_dates %>% filter(poly_ids == 6),
            aes(x = date, y = gcc90), col = "black") +
  geom_line(data = df_smoothed_dates %>% filter(poly_ids == 6),
            aes(x = date, y = gcc75), col = "gray30") +
  geom_line(data = df_smoothed_dates %>% filter(poly_ids == 6),
            aes(x = date, y = gcc50), col = "gray60") +
  theme_bw()

ggplot(df_smoothed_dates) +
  geom_line(aes(x = date, y = gcc, col = poly_ids)) +
  facet_wrap(~as.factor(poly_ids))

#

ggplot(img_data_merged_bf %>% filter(PC_Poly_ID == 6), aes(x = as.Date(datetime), y = NDVIc)) +
  geom_point(alpha = 0.5, col = "red") +
  geom_line(data = df_smoothed_dates %>% filter(poly_ids == 6),
            aes(x = date, y = NDVIc)) +
  theme_bw()

ggplot(df_smoothed_dates) +
  geom_line(aes(x = date, y = NDVIc, col = poly_ids)) +
  facet_wrap(~as.factor(poly_ids))


#####


setupPhenoElmoreModGcc <- function(tree_ts_df_complete, year_val){
  # make sure the labeling of the "date" column is formatted correctly
  tree_ts_df_complete_oneyear <- tree_ts_df_complete[which(year(tree_ts_df_complete$date) ==  year_val),]
  t <- tree_ts_df_complete_oneyear$date
  vi <- tree_ts_df_complete_oneyear$gcc
  vi_checked <- check_input(t, vi)
  tout <- seq(1, 366, 1) # need to be the same length, so assume a year of 366 days to be directly comparable to PlanetScope
  c_out <- curvefit(vi_checked$y, yday(vi_checked$t), tout, w = vi_checked$w, methods = c("Elmore")) # needed the t_out variable, this fixed it. Not sure why
  return(c_out)
}

setupPhenoElmoreModNDVIc <- function(tree_ts_df_complete, year_val){
  # make sure the labeling of the "date" column is formatted correctly
  tree_ts_df_complete_oneyear <- tree_ts_df_complete[which(year(tree_ts_df_complete$date) ==  year_val),]
  t <- tree_ts_df_complete_oneyear$date
  vi <- tree_ts_df_complete_oneyear$NDVIc + 1 # shift NDVIc to all positive
  vi_checked <- check_input(t, vi)
  tout <- seq(1, 366, 1) # need to be the same length, so assume a year of 366 days to be directly comparable to PlanetScope
  c_out <- curvefit(vi_checked$y, yday(vi_checked$t), tout, w = vi_checked$w, methods = c("Elmore")) # needed the t_out variable, this fixed it. Not sure why
  return(c_out)
}

runPhenoElmoreModGcc <- function(df_tree){
  fits <- list('2022' = setupPhenoElmoreModGcc(df_tree, 2022))
  
  l_pheno   <- get_pheno(fits, method = "Elmore", TRS = c(0.5), asymmetric = TRUE)
  
  l_pheno_sub <- l_pheno$date$Elmore[,c("flag", "origin", "TRS5.sos", "TRS5.eos")]
  
  l_pheno_sub$SOS_50 <-  yday(ymd(paste(l_pheno_sub$flag, month(l_pheno_sub$TRS5.sos), day(l_pheno_sub$TRS5.sos), sep = "-")))
  l_pheno_sub$EOS_50 <- yday(ymd(paste(l_pheno_sub$flag, month(l_pheno_sub$TRS5.eos), day(l_pheno_sub$TRS5.eos), sep = "-")))
  
  gof <- get_GOF(fits)
  pheno_output<- merge(l_pheno_sub[, c("flag", "SOS_50", "EOS_50")], gof, by = "flag")
  #pheno_output$Object_ID <- df_tree$Object_ID[1] # will only be one tree ID
  pheno_output$poly_ids <- df_tree$poly_ids[1] # will only be one tree ID
  return(pheno_output)
}

runPhenoElmoreModNDVIc <- function(df_tree){
  fits <- list('2022' = setupPhenoElmoreModNDVIc(df_tree, 2022))
  
  l_pheno   <- get_pheno(fits, method = "Elmore", TRS = c(0.5), asymmetric = TRUE)
  
  l_pheno_sub <- l_pheno$date$Elmore[,c("flag", "origin", "TRS5.sos", "TRS5.eos")]
  
  l_pheno_sub$SOS_50 <-  yday(ymd(paste(l_pheno_sub$flag, month(l_pheno_sub$TRS5.sos), day(l_pheno_sub$TRS5.sos), sep = "-")))
  l_pheno_sub$EOS_50 <- yday(ymd(paste(l_pheno_sub$flag, month(l_pheno_sub$TRS5.eos), day(l_pheno_sub$TRS5.eos), sep = "-")))
  
  gof <- get_GOF(fits)
  pheno_output<- merge(l_pheno_sub[, c("flag", "SOS_50", "EOS_50")], gof, by = "flag")
  #pheno_output$Object_ID <- df_tree$Object_ID[1] # will only be one tree ID
  pheno_output$poly_ids <- df_tree$poly_ids[1] # will only be one tree ID
  return(pheno_output)
}

# Visualize fits
for (i in as.numeric(unq_polys)){
  df_1tree <- df_smoothed_dates %>% filter(poly_ids == i)
  fits <- list('2022' = setupPhenoElmoreModNDVIc(df_1tree, 2022))
  get_pheno(fits, method = "Elmore", TRS = c(0.5), asymmetric = TRUE, IsPlot = TRUE)
}
# Does OK for NDVIc once shifted

for (i in as.numeric(unq_polys)){
  df_1tree <- df_smoothed_dates %>% filter(poly_ids == i)
  fits <- list('2022' = setupPhenoElmoreModGcc(df_1tree, 2022))
  get_pheno(fits, method = "Elmore", TRS = c(0.5), asymmetric = TRUE, IsPlot = TRUE)
}
# Fails to track GCC shape in almost every case


#pheno_output_1tree <- runPhenoElmoreModGcc(df_1tree)
#pheno_output_1tree <- runPhenoElmoreModNDVIc(df_1tree)

# run for NDVIc
loop_num <- 1
for (i in as.numeric(unq_polys)){
  df_1tree <- df_smoothed_dates %>% filter(poly_ids == i)
  pheno_output <- runPhenoElmoreModNDVIc(df_1tree)
  if (loop_num == 1){
    pheno_output_all <- pheno_output
  } else {
    pheno_output_all <- rbind.data.frame(pheno_output_all, pheno_output)
  }
  loop_num <- loop_num + 1
}

colnames(pheno_output_all)[1] <- "Year"
colnames(pheno_output_all) <- paste0("PhenoCam_", colnames(pheno_output_all))

# merge in planetscope data
phenocam_tree_info <- read.csv("/Users/dlm356/dlm356_files/nyc_trees/phenocam/cuny_pheno_tree_poly_list_new.csv") # updated csv
planetscope_tree_pheno1 <- read.csv("/Volumes/NYC_geo/Planet/tests/nyc_daily_stack_highsunonly_cal_pheno/trees_pheno_output_objset1_297001_298000_point.csv")
planetscope_tree_pheno2 <- read.csv("/Volumes/NYC_geo/Planet/tests/nyc_daily_stack_highsunonly_cal_pheno/trees_pheno_output_objset1_298001_299000_point.csv")
planetscope_tree_pheno3 <- read.csv("/Volumes/NYC_geo/Planet/tests/nyc_daily_stack_highsunonly_cal_pheno/trees_pheno_output_objset1_296001_297000_point.csv")
planetscope_tree_pheno <- rbind.data.frame(planetscope_tree_pheno1, planetscope_tree_pheno2, planetscope_tree_pheno3)

planetscope_tree_pheno_sub <- planetscope_tree_pheno[which(planetscope_tree_pheno$Object_ID %in% phenocam_tree_info$TNC_Poly_ID),]
planetscope_tree_pheno_sub <- planetscope_tree_pheno_sub %>% filter(Year == 2022)
colnames(planetscope_tree_pheno_sub ) <- paste0("PlanetScope_", colnames(planetscope_tree_pheno_sub ))

# Compare SOS and EOS for:
# PlanetScope NDVI vs PhenoCam GCC - these fits are bad, will need to circle back to it
# PlanetScope NDVI vs PhenoCam NDVIc

pcps_merge <- merge(phenocam_tree_info, pheno_output_all, by.x = "PC_Poly_ID", by.y = "PhenoCam_poly_ids")
pcps_merge <- merge(pcps_merge, planetscope_tree_pheno_sub, by.x = "TNC_Poly_ID", by.y = "PlanetScope_Object_ID")

ggplot(pcps_merge) +
  geom_point(aes(x = PlanetScope_SOS_50, y = PhenoCam_SOS_50, col = Species), size = 2) +
  geom_smooth(aes(x = PlanetScope_SOS_50, y = PhenoCam_SOS_50), method = "lm") +
  geom_abline(slope = 1, intercept = 0) +
  geom_text(aes(x = PlanetScope_SOS_50, y = PhenoCam_SOS_50, label = as.character(PC_Poly_ID))) +
  coord_equal()

summary(lm(pcps_merge$PhenoCam_SOS_50 ~ pcps_merge$PlanetScope_SOS_50))
mean(pcps_merge$PlanetScope_SOS_50 - pcps_merge$PhenoCam_SOS_50)
sd(pcps_merge$PlanetScope_SOS_50 - pcps_merge$PhenoCam_SOS_50)


ggplot(pcps_merge) +
  geom_point(aes(x = PlanetScope_EOS_50, y = PhenoCam_EOS_50, col = Species), size = 2) +
  geom_smooth(aes(x = PlanetScope_EOS_50, y = PhenoCam_EOS_50), method = "lm") +
  geom_abline(slope = 1, intercept = 0) +
  geom_text(aes(x = PlanetScope_EOS_50, y = PhenoCam_EOS_50, label = as.character(PC_Poly_ID))) +
  coord_equal()

summary(lm(pcps_merge$PhenoCam_EOS_50 ~ pcps_merge$PlanetScope_EOS_50))

mean(pcps_merge$PlanetScope_SOS_50 - pcps_merge$PhenoCam_SOS_50)
mean(pcps_merge$PlanetScope_EOS_50 - pcps_merge$PhenoCam_EOS_50)