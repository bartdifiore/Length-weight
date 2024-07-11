library(gmRi)
library(tidyverse)
library(sf)


lw <- readRDS("Data/bottomTrawlSurvey_indLengthWeight.rds")

# Paths to Box folder
res_box_path<- cs_path(box_group = "Res Data")
proj_box_path<- cs_path(box_group = "Mills Lab", subfolder = "Projects/sdmTMB_sandbox")

df <- readRDS(paste0(res_box_path, "NMFS_trawl/SURVDAT_current/survdat_lw.rds")) # From https://github.com/NOAA-EDAB/data-requests/tree/main/EwE-menhaden-AndreBuchheister


df <- df$survdat

unique_tows <- df %>%
  dplyr::select(CRUISE6, STATION, STRATUM, TOW, LAT, LON) %>% 
  distinct() %>%
  as_tibble()


ecodata::epu_sf
library(ecodata)
plot(epu_sf)

st_crs(epu_sf)

epu <- epu_sf %>% 
  select(EPU) %>% 
  st_make_valid()


lw <- lw %>%
  mutate(CRUISE6 = as.numeric(CRUISE6), 
         STRATUM = as.numeric(STRATUM), 
         TOW = as.numeric(TOW), 
         STATION = as.numeric(STATION)) %>%
  left_join(unique_tows)

lw[is.na(lw$LAT), ]

lw_spatial <- lw %>% 
  drop_na(LAT, LON) %>%
  st_as_sf(coords = c("LON", "LAT"), crs = st_crs(4269)) %>%
  st_join(epu)

lw_plot <- lw_spatial %>% 
  select(CRUISE6, STATION, STRATUM, TOW, geometry, EPU) %>%
  distinct()


ggplot()+
  geom_sf(data = epu_sf)+
  geom_sf(data = lw_plot, aes(color = EPU), size = 0.01)

ggsave("map_of_points.png")


lw_out <- lw_spatial %>% 
  st_drop_geometry()
write_rds(lw_out, file = "Data/length_weight_epu.rds", compress = "gz")  
  
  
  
  
