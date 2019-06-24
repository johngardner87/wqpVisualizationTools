library(tidyverse)
library(feather)
library(sf)
library(nhdplusTools)
library(tmap)
setwd("~/Documents/Large Datasets/")

# network_trimmed <- readRDS("~/Documents/Large Datasets/Flowlines_trimmed.rds")
# network_ordered <- readRDS("~/Documents/Large Datasets/Flowlines_ordered.rds")
# bounds_HUC8 <- st_read("~/Documents/Large Datasets/Simplified WBDHUC Data/WBDHU8Counts.gpkg", quiet=TRUE)
# 
# network_trimmed <- mutate(network_trimmed, HUC8_code = substr(REACHCODE, 0, 8))
# 
# HUC8 <- mutate(bounds_HUC8, new_HUC8 = as.character(HUC8))
# HUC8 <- select(HUC8, -HUC8)
# HUC8 <- rename(HUC8, HUC8 = new_HUC8)

# look_up <- data.frame("network_obs" = 1 , "HUC8_code"="1")
# look_up <- filter(look_up, "bounds_obs" == 0)

# network_ordered <- network_trimmed[order(network_trimmed$HUC8_code),] 

# '''
# TOO SLOW
# network <- network_subset_1
# 
# for (i in 1:nrow(network)) {
#   for (j in 1:nrow(HUC8)) {
#     if (network[i,]$HUC8_code == HUC8[j,]$HUC8) {
#       frame_name <- paste("HUC8_", HUC8[j,]$HUC8, sep = "")
#       if (exists(frame_name) == FALSE) {
#         assign(frame_name, filter(network, HUC8_code == "0"))
#         assign(frame_name, network[i,])
#       }
#       if (exists(frame_name) == TRUE) {
#         assign(frame_name, rbind(get(frame_name), network[i,]))
#       }
#       break
#     }
#   }
#   print(i)
# }
# '''

# '''
# for (i in 1:(length(unique(network_ordered$HUC8_code))-1)) {
#   this_HUC8_code <- unique(network_ordered$HUC8_code)[i]
#   next_HUC8_code <- unique(network_ordered$HUC8_code)[i+1]
#   start <- match(this_HUC8_code, network_ordered$HUC8_code)
#   end <- match(next_HUC8_code, network_ordered$HUC8_code) - 1
#   frame_name <- paste("HUC8_", this_HUC8_code, sep = "")
#   assign(frame_name, network_ordered[start, end])
# }
# 
# #deal with the last occurence (i == 2117)
# '''

k <- length(unique(network_trimmed$HUC8_code))

for (i in 1:k) {
  this_HUC8_code <- unique(network_trimmed$HUC8_code)[i]
  network_trimmed %>% 
    filter(HUC8_code == this_HUC8_code) %>%
    saveRDS(file = paste("~/Documents/Large Datasets/Flowline split by HUC8/HUC8_", this_HUC8_code, ".rds", sep = ""))
  print(i)
}
