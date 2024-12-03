
suppressWarnings({
  suppressPackageStartupMessages({
    library(amt)
    library(arcgisbinding)
    library(beepr)
    library(mapview)
    library(misty)
    library(move)
    library(sf)
    library(s2)
    library(spatialEco)
    library(spatstat)
    library(tidyverse)
    library(units)
  })})

#Allows an experted mapviews to be self-contained
mapviewOptions(fgb = FALSE) # so we can save map as html

#Keep this the same for replicating random samples
set.seed(608)

# Specify which packages functions should be used by default
select <- dplyr::select
mutate <- dplyr::mutate
sumarise <- dplyr::summarise

#---- Set Relevant Survey Data -------------------------------------------------
surveytimestart <- as.POSIXct("2022-01-01 12:00:01", tz = "America/Chicago") # Format is YYYY-MM-DD HH:MM:SS
surveytimeend <- as.POSIXct("2024-11-30 11:59:59", tz = "America/Chicago") # Format is YYYY-MM-DD HH:MM:SS

# Import Data from MoveBank -----------------------------------------------------------------
{ #Starts this chunk of code
  
# store movebank login credentials
login_storedAG <- movebankLogin(username = "aiden_gehrke",
                                password = "Camo1100awg5734351*")
# Read in 2022 Deployments
nopi_df <- getMovebankData(
  study = "Northern Pintail Migration Strategies (Louisiana Deployments)", # Project name as listed in MoveBank
  login = login_storedAG, # Reference the login object above
  timestamp_start = surveytimestart, # Specify range of dates to read data in from
  timestamp_end = surveytimeend,# Specify range of dates to read data in from
  removeDuplicatedTimestamps = TRUE) # Prevents errors when pulling in some (mostly earlier) data - Will give warning

ducks_df <- getMovebankData( #Read in 2023 & 2024 Deployments
  study = "NOPI Habitat Use/Disturbance", # Project name as listed in MoveBank
  login = login_storedAG,# Reference the login object above
  timestamp_start = surveytimestart, # Specify range of dates to read data in from
  timestamp_end = surveytimeend, # Specify range of dates to read data in from
  removeDuplicatedTimestamps = TRUE) # Prevents errors when pulling in some (mostly earlier) data - Will give warning

# Initial Dataframe Cleaning ------------------------------------------------------------
# Clean up Georgina's NOPI Points ----
la_nopi_df <- nopi_df %>%
  as.data.frame() %>% # convert from movestack to data frame
  select(location_long, location_lat, timestamp, local_identifier, tag_local_identifier, ground_speed, gps_hdop) %>%
  mutate(
    local_identifier = as.factor(local_identifier),
    serial_no = as.factor(tag_local_identifier),
    timestamp_cst = as.POSIXct(format(timestamp, tz = "America/Chicago")),# change time zone to CST
    day_of_year = lubridate::yday(timestamp_cst),# group data by day later to use last point of the day
    time_of_day = format(timestamp_cst, format="%H%M"),
    species = "pintail") %>%
  select(local_identifier, serial_no, timestamp_cst, location_long, location_lat, day_of_year, time_of_day, ground_speed, species, gps_hdop)%>%
  as_tibble()%>%  
  filter(!(local_identifier %in% c("LA-22-36-D2", #redeployed as "NOPI23-21"
                                   "LA-22-53", # redeployed as "NOPI23-24"
                                   "LA-22-49", # redeployed as "NOPI23-23"
                                   "LA-22-45"))) # redeployed as "NOPI23-22"

# Clean Up Data from Paul's Pilot Season Project ----
la_23_df <- ducks_df %>%
  as.data.frame() %>% # convert from movestack to data frame
  select(location_long, location_lat, timestamp, tag_local_identifier, local_identifier, ground_speed, gps_hdop) %>%
  mutate( 
    local_identifier = as.factor(local_identifier),
    serial_no = as.factor(tag_local_identifier),
    timestamp_cst = as.POSIXct(format(timestamp,tz = "America/Chicago")),# change time zone to CST
    day_of_year = lubridate::yday(timestamp_cst),# group data by day later to use last point of the day
    time_of_day = format(timestamp_cst, format="%H%M"),
    species = if_else(str_detect(local_identifier, "MALL"),"mallard","pintail"))%>%
  select(local_identifier, serial_no, timestamp_cst, location_long, location_lat, day_of_year, time_of_day, ground_speed, gps_hdop, species)%>%
  as_tibble()%>%
  filter(species != "mallard") %>% 
  filter(!(local_identifier %in% c("RWR 1Feb22 NOPI18", #Same as LA-22-64
                                   "RWR 12Jan22 NOPI12", #Same as LA-22-46
                                   "RWR 1Feb22 NOPI1", #Same as LA-22-47
                                   "RWR 1Feb22 NOPI4", #Same as LA-22-50
                                   "RWR 1Feb22 NOPI5", #Same as LA-22-51
                                   "RWR 1Feb22 NOPI10"))) #Same as LA-22-56

# Reclassify local identifier names to better fit with our nomenclature
la_23_df$local_identifier <-recode(la_23_df$local_identifier, 
                                   # Start of 2023 Deployments
                                   "LacNWR 6Jan23 NOPI1"  = "NOPI23-1", "LacNWR 6Jan23 NOPI2"  = "NOPI23-2",
                                   "RWR 7Jan23 NOPI1" = "NOPI23-3", "RWR 7Jan23 NOPI2" = "NOPI23-4",
                                   "RWR 7Jan23 NOPI3" = "NOPI23-5", "RWR 7Jan23 NOPI4" = "NOPI23-6",
                                   "RWR 7Jan23 NOPI5" = "NOPI23-7", "RWR 7Jan23 NOPI6" = "NOPI23-8",
                                   "RWR 7Jan23 NOPI7" = "NOPI23-9", "RWR 7Jan23 NOPI8" = "NOPI23-10", 
                                   "RWR 7Jan23 NOPI9" = "NOPI23-11", "RWR 7Jan23 NOPI10" = "NOPI23-12",
                                   "RWR 7Jan23 NOPI11" = "NOPI23-13", "RWR 7Jan23 NOPI12" = "NOPI23-14",
                                   "RWR 7Jan23 NOPI13" = "NOPI23-15", "RWR 7Jan23 NOPI14" = "NOPI23-16", 
                                   "RWR 7Jan23 NOPI15" = "NOPI23-17", "RWR 7Jan23 NOPI16" = "NOPI23-18", 
                                   "RWR 7Jan23 NOPI17" = "NOPI23-19", "RWR 7Jan23 NOPI18" = "NOPI23-20",
                                   "LacNWR 14Jan23 NOPI1" = "NOPI23-21", "LacNWR 14Jan23 NOPI2" = "NOPI23-22", 
                                   "LacNWR 14Jan23 NOPI3" = "NOPI23-23", "LacNWR 13Jan23 NOPI4" =  "NOPI23-24",
                                   # Start of 2024 Deployments
                                   "29Dec23 RWR_1" = "NOPI24-1", "29Dec23 RWR_2" = "NOPI24-2", 
                                   "29Dec23 RWR_3" = "NOPI24-3", "29Dec23 RWR_4" = "NOPI24-4",
                                   "29Dec23 RWR_5" = "NOPI24-5", "29Dec23 RWR_6" = "NOPI24-6",
                                   "29Dec23 RWR_7" = "NOPI24-7", "29Dec23 RWR_8" = "NOPI24-8",
                                   "29Dec23 RWR_9" = "NOPI24-9", "29Dec23 RWR_10" = "NOPI24-10",
                                   "29Dec23_RWR_11" = "NOPI24-11", "29Dec23 RWR_12" = "NOPI24-12",
                                   "29Dec23 RWR_13" = "NOPI24-13", "29Dec23 RWR_14" = "NOPI24-14",
                                   "29Dec23 RWR_15" = "NOPI24-15", "29Dec23 RWR_16" = "NOPI24-16",
                                   "29Dec23 RWR_17" = "NOPI24-17", "29Dec23 RWR_18" = "NOPI24-18",
                                   "29Dec23 RWR_19" = "NOPI24-19", "29Dec23 RWR_20" = "NOPI24-20",
                                   "29Dec23 RWR_21" = "NOPI24-21", "29Dec23 RWR_22" = "NOPI24-22",
                                   "29Dec23 RWR_23" = "NOPI24-23", "29Dec23 RWR_24" = "NOPI24-24",
                                   "29Dec23 RWR_25" = "NOPI24-25", "29Dec23 RWR_26" = "NOPI24-26",
                                   "29Dec23 RWR_27" = "NOPI24-27", "29Dec23 RWR_28" = "NOPI24-28",
                                   "29Dec23 RWR_29" = "NOPI24-29", "29Dec23 RWR_30" = "NOPI24-30",
                                   "31Dec23 RWR_1" = "NOPI24-31", "31Dec23 RWR_2" = "NOPI24-32",
                                   "31Dec23 RWR_3" = "NOPI24-33", "31Dec23 RWR_4" = "NOPI24-34",
                                   "31Dec23 RWR_5" = "NOPI24-35", "31Dec23 RWR_7" = "NOPI24-36",
                                   "31Dec23 RWR_8" = "NOPI24-37", "31Dec23 RWR_9" = "NOPI24-38",
                                   "31Dec23 RWR_10" = "NOPI24-39", "21Jan2024_1RWR" = "NOPI24-40",
                                   "21Jan2024_2RWR" = "NOPI24-41", "21Jan24_3RWR" = "NOPI24-42",
                                   "22Jan24_4RWR" = "NOPI24-43", "21Jan24_5RWR" = "NOPI24-44",
                                   "21Jan24_6RWR" = "NOPI24-45", "21Jan24_7RWR" = "NOPI24-46",
                                   "21Jan24_8RWR" = "NOPI24-47",
                                   # Start of the 2025 Deployments
                                   "246151" = "NOPI25-1", "246152" = "NOPI25-2", 
                                   "246153" = "NOPI25-3", "246154" = "NOPI25-4",
                                   "246155" = "NOPI25-5", "246156" = "NOPI25-6", 
                                   "246157" = "NOPI25-7", "246158" = "NOPI25-8",
                                   "246159" = "NOPI25-9", "246160" = "NOPI25-10", 
                                   "246161" = "NOPI25-11", "246162" = "NOPI25-12",
                                   "246163" = "NOPI25-13", "246164" = "NOPI25-14", 
                                   "246165" = "NOPI25-15", "246166" = "NOPI25-16",
                                   "246167" = "NOPI25-17", "246168" = "NOPI25-18", 
                                   "246169" = "NOPI25-19", "246170" = "NOPI25-20",
                                   "246171" = "NOPI25-21", "246172" = "NOPI25-22", 
                                   "246173" = "NOPI25-23", "246174" = "NOPI25-24",
                                   "246175" = "NOPI25-25", "246176" = "NOPI25-26",
                                   "246177" = "NOPI25-27", "246178" = "NOPI25-28",
                                   "246179" = "NOPI25-29", "246180" = "NOPI25-30", 
                                   "246181" = "NOPI25-31", "246182" = "NOPI25-32",
                                   "246183" = "NOPI25-33", "246184" = "NOPI25-34", 
                                   "246185" = "NOPI25-35", "246186" = "NOPI25-36",
                                   "246187" = "NOPI25-37", "246188" = "NOPI25-38", 
                                   "246189" = "NOPI25-39", "246190" = "NOPI25-40"
)


# Combine the two datasets 
la_ducks_df<-rbind(la_nopi_df,la_23_df)

# Turn the dataframe into a tibble for use cleaning data with amt
la_ducks_df <- as_tibble(la_ducks_df)%>%
  arrange(., timestamp_cst) # Ordered so that rows are in the correct order when transferring additional columns to track_xyt

# AMT Data Cleaning Workflow ------------------------------------------------------------
#1) Create the track_xyt feature  
la_ducks_trk <- make_track(la_ducks_df, location_long, location_lat, timestamp_cst, crs = 4326, all_cols = TRUE) %>% # Requires a projected coordinate system
  time_of_day() %>%
  group_by(local_identifier, serial_no) %>% 
  mutate(location_long = x_, location_lat = y_, timestamp = t_, daynight = tod_, hdop = as.numeric(gps_hdop), ground_speed = as.numeric(ground_speed)) %>%  
  arrange(local_identifier) %>% 
  select(local_identifier,serial_no,location_long,location_lat,timestamp,day_of_year,time_of_day,daynight,ground_speed,hdop, x_, y_, t_)


la_ducks_trk_rsmpl <- c()

for (i in unique(la_ducks_trk$local_identifier)){
  track_rsmpl <- la_ducks_trk %>% 
    filter(local_identifier == i) %>% 
    track_resample(rate = hours(1), tolerance = minutes(10))
  
  la_ducks_trk_rsmpl[[i]] <- track_rsmpl}

la_track_rsmpl <- bind_rows(la_ducks_trk_rsmpl) %>% 
  select(-burst_)

#2) Filter points based on specified criteria      
la_ducks_trk <- la_track_rsmpl %>%
  filter(hdop < 10)%>% # Removes imprecise fixes
  filter(ground_speed < 8) %>% # Removes fixes taken when in flight
  arrange(local_identifier, timestamp)

# Convert back into tibble
la_ducks_cln <- as_tibble(la_ducks_trk)

# Convert into an sf spatial object for workflow below
la_ducks_trk_cln <- la_ducks_cln %>% 
  st_as_sf( 
    coords = c("location_long","location_lat"), 
    crs = 4326,
    remove = FALSE)

} #Ends this chunk of code

saveRDS(la_ducks_trk_cln, file = "2025_Bird_Points.rds")


### DELETE EVERYTHING ABOVE THIS LINE FOLLOWING THE SEASON ###


#la_ducks_trk_cln <- readRDS(file = "2025_Bird_Points.rds")


la_ducks_tbl <- as_tibble(la_ducks_trk_cln) # Converts to a table for record keeping purposes


current_year_TS <- as.POSIXct("2023-12-10 00:00:01", tz = "America/Chicago") 
all_years_TS <- as.POSIXct("2022-01-31 00:00:01", tz = "America/Chicago")

post_season_track <- la_ducks_tbl %>%
  bind_rows() %>% 
  as_tibble() %>% 
  mutate(deployment_year = as.factor(case_when(
    grepl("-22-", local_identifier) == TRUE ~ "2022",
    grepl("23-", local_identifier) == TRUE ~ "2023",
    grepl("24-", local_identifier) == TRUE ~ "2024",
    grepl("25-", local_identifier) == TRUE ~ "2025")), 
    serial_no = if_else(local_identifier == "NOPI24-6" & serial_no == "235235", "235235(1)", serial_no),
    serial_no = if_else(local_identifier == "NOPI24-45" & serial_no == "235235", "235235(2)", serial_no),
    week = week(timestamp),
    day = day_of_year) %>% 
  filter(timestamp > all_years_TS)


gps_fixes_week <- post_season_track%>%
  filter(timestamp > current_year_TS) %>% 
  dplyr::summarise(fixes = n(), .by = c(local_identifier, serial_no, week, deployment_year))%>%
  dplyr::summarise(med = median(fixes), ave = mean(fixes), .by = c(deployment_year, local_identifier, serial_no))%>%
  arrange(med, ave)#, .by = local_identifier)

gps_fixes_day <- post_season_track%>%
  filter(timestamp > current_year_TS) %>% 
  dplyr::summarise(fixes = n(), .by = c(local_identifier, serial_no, day_of_year, deployment_year))%>%
  dplyr::summarise(med = median(fixes), ave = mean(fixes), .by = c(local_identifier, serial_no, deployment_year))%>%
  arrange(deployment_year)#, .by = local_identifier)

ggplot(gps_fixes_day, aes(x = serial_no, y = med, fill = deployment_year)) +
  geom_col(width = 0.75, colour = "black", linewidth = 0.25) +
  # scale_x_discrete(limits = c()) +
  scale_fill_manual(values = c("#1b3b71","#3D5169", "#7598C0", "#D1E3F7"))+
  labs(x = "Serial Number", 
       y = "Median GPS Fixes Per Day",
       title = "Median GPS Fixes per Day by Marked Bird")+
  theme_bw()+
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 10),
        axis.text.y = element_text(size = 12),
        text = element_text(size = 16, family = "serif"),
        plot.title = element_text(size=15, hjust = 0.45))+
  scale_y_continuous(breaks = seq(0, 24, by = 2))+
  guides(fill = guide_legend(title = "Deployment Year:"))
#ggsave(here::here('output/figures/Median_GPS_Fixes.png'), height = 5, width = 8, units = 'in')


la_duck_sf <- post_season_track %>% 
  st_as_sf( 
    coords = c("location_long","location_lat"), 
    crs = 4326,
    remove = FALSE)

la_ducks_studyarea <- st_read("C:/Users/AGehrke/OneDrive - LSU AgCenter/LSU_NocturnalForagingMS/R_RStudio_Data/data/la_ducks_studyarea/la_studyarea.shp") #la_ducks_studyarea.shp was generated by merging the 5-parish study region, merging the coastal basin polygons, and erasing the intersection between the two, along with clipping out all land south of the intercoastal waterway
#focal_blocks <- st_read("C:/Users/AGehrke/OneDrive - LSU AgCenter/LSU_NocturnalForagingMS/R_RStudio_Data/data/focal_blocks.shp")
post_season_track_SA <- st_intersection(la_duck_sf, la_ducks_studyarea)%>% # Selects only those fixes in the study area
  select(timestamp,local_identifier, deployment_year, serial_no,day_of_year,time_of_day,daynight,ground_speed,hdop,location_lat, location_long,geometry) %>% 
  as_tibble() %>% 
  suppressWarnings()

study_area_days <- post_season_track_SA %>%
  summarise(DISA = n_distinct(day_of_year), .by = c(local_identifier, serial_no, deployment_year)) #DISA = Days In Study Area
  

all_study_area_days <- study_area_days%>%
  summarise(Maximum = max(DISA), Minimum = min(DISA), Median = floor(median(DISA)), Mean = floor(mean(DISA)))%>% #DISA = Days In Study Area
  pivot_longer(cols = 1:4, names_to = "Statistic", values_to = "Days")

#Total days in the study area each day
ggplot(study_area_days, aes(x = serial_no, y = DISA, fill = deployment_year), show.legend = FALSE) +
  geom_col(width = 0.75, colour = "black", linewidth = 0.2) +
  scale_x_discrete(limits = c("214487", "214488", "214505", "214494", "226874", 
                              "227160", "235230", "235232", "235233", "235234", 
                              "235235(1)", "235236", "235237", "235238", "235239", 
                              "235240","235241", "235242", "235243", "235244", 
                              "235245", "235247", "235248", "235249", "235250", 
                              "235251", "235252", "235255", "235256", "235257",
                              "235258", "235259", "235260", "235261", "235262", 
                              "235263", "235265", "235266", "235267", "235268", 
                              "235269")) +
  scale_fill_manual(values = c("#45ADA8", "#9DE0AD", "#E5FCC2"))+
  labs(x = "Serial Number", 
       y = "Total Days in Study Area",
       title = "Days in Study Area By Marked Bird
       (11/01/23 - 3/12/24)")+
  theme_bw()+
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 10),
        axis.text.y = element_text(size = 12),
        text = element_text(size = 16, family = "serif"),
        plot.title = element_text(size=15 , hjust = 0.45)) +
  scale_y_continuous(breaks = seq(0, 150, by = 10))+
  guides(fill = guide_legend(title = "Deployment Year:"))
#ggsave(here::here('output/figures/Days_In_Study_Area_By_Bird.png'), height = 5, width = 8, units = 'in')

ggplot(all_study_area_days, aes(x = Statistic, y = Days, fill = Statistic), show.legend = FALSE) +
  geom_col(width = 0.75, colour = "black", linewidth = 0.75) +
  geom_text(aes(label = Days), vjust = -0.25, colour = "black", size = 4.5, family = "serif")+
  labs(y = "Days in Study Area", 
       x = "",
       title = "Days in Study Area - Summarized Across All Marked Birds") +
  scale_x_discrete(limits = c("Maximum", "Minimum", "Mean", "Median")) +
  scale_fill_manual(values = c("#555555","white","#0598CE","#113768")) +
  theme_bw()+
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 0, vjust = 0.75, hjust = 0.5, size = 14),
        axis.text.y = element_text(size = 12),
        text = element_text(size = 16, family = "serif"),
        plot.title = element_text(size = 15, hjust = 0.45))+
  scale_y_continuous(breaks = seq(0, 150, by = 10))
#ggsave(here::here('output/figures/Days_In_Study_Area_Aggregate.png'), height = 5, width = 8, units = 'in')
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Make sure to constrain to the study area for this one
{loss_contact <- as_tibble(post_season_track_SA)%>%
  select(timestamp, local_identifier, serial_no) %>% 
  filter(timestamp > current_year_TS)
loss_contact_head <- loss_contact%>%
  group_by(local_identifier, serial_no)%>%
  slice_head(n = 1)%>%
  mutate(fixes = "first_fix")%>%
  ungroup()
loss_contact_tail <- loss_contact%>%
  group_by(local_identifier, serial_no)%>%
  slice_tail(n = 1)%>%
  mutate(fixes = "last_fix")%>%
  ungroup()
loss_contact_start2end <- rbind(loss_contact_head, loss_contact_tail) %>%
  group_by(local_identifier, serial_no)%>%
  pivot_wider(names_from = fixes, values_from = timestamp )
loss_contact_start2end$first_fix <- as_date(loss_contact_start2end$first_fix)
loss_contact_start2end$last_fix <- as_date(loss_contact_start2end$last_fix)}


ggplot(loss_contact_start2end, aes(x = first_fix, y = serial_no)) +
  geom_segment(aes(xend = last_fix, yend = serial_no), colour = "black", linewidth = 1) +
  geom_point(aes(x = first_fix, colour = "First Fix"), 
             size = 2.5, stroke = 1, fill = "#a0ea8c", shape = 21, color = "black") +
  geom_point(aes(x = last_fix, colour = "Last Fix"), 
             size = 2, stroke = 1, fill = "#df5949", shape = 22, color = "black") +
  labs(x = "", y = "Serial Number", title = "Days Spent in the Study Area") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 0.5, size = 14),
        axis.text.y = element_text(size = 12),
        text = element_text(size = 16, family = "serif"),
        plot.title = element_text(size = 15, hjust = 0.45),
        legend.position = "inside", 
        legend.position.inside = c(0.85, 0.25),
        legend.key.size = unit(8, 'cm'),
        legend.key.height = unit(8, 'mm'),
        legend.key.width = unit(6, 'mm'),
        legend.text = element_text(size = 12, hjust = 0, vjust = 0.5),
        legend.background = element_rect(fill = "white", colour = "black"),
        legend.justification = "center",
        legend.spacing.x = unit(0.1, "mm"),
        legend.spacing.y = unit(-0.25, "mm")) +
  scale_x_date(date_breaks = "1 week", date_minor_breaks = "1 week", date_labels = "%m-%d-%Y") +
  scale_colour_manual(values = c("#a0ea8c", "#df5949"), 
                      labels = c("Deployment", "Last Known Fix")) +
  guides(colour = guide_legend(title = NULL))
#ggsave(here::here('output/figures/Birds_in_the_study_area.png'), height = 10, width = 20, units = 'in')


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Run the points starting from January 1st 2022 to current
{loss_contact_22 <- as_tibble(post_season_track)%>%
  filter(grepl('LA-22-', local_identifier)) %>%
  filter(timestamp > as.POSIXct("2022-01-31 00:00:01", tz = "America/Chicago")) %>% 
  select(timestamp, local_identifier, serial_no)
loss_contact_head_22 <- loss_contact_22%>%
  group_by(local_identifier, serial_no)%>%
  slice_head(n = 1)%>%
  mutate(fixes = "first_fix")%>%
  ungroup()
loss_contact_tail_22 <- loss_contact_22%>%
  group_by(local_identifier, serial_no)%>%
  slice_tail(n = 1)%>%
  mutate(fixes = "last_fix")%>%
  ungroup()
loss_contact_start2end_22 <- rbind(loss_contact_head_22, loss_contact_tail_22) %>%
  group_by(local_identifier, serial_no)%>%
  pivot_wider(names_from = fixes, values_from = timestamp )
loss_contact_start2end_22$first_fix <- as_date(loss_contact_start2end_22$first_fix)
loss_contact_start2end_22$last_fix <- as_date(loss_contact_start2end_22$last_fix)}


ggplot(loss_contact_start2end_22, aes(x = first_fix, y = serial_no)) +
  geom_segment(aes(xend = last_fix, yend = serial_no), colour = "black", linewidth = 1) +
  geom_point(aes(x = first_fix, colour = "First Fix"), 
             size = 2.5, stroke = 1, fill = "#a0ea8c", shape = 21, color = "black") +
  geom_point(aes(x = last_fix, colour = "Last Fix"), 
             size = 2, stroke = 1, fill = "#df5949", shape = 22, color = "black") +
  labs(x = "", y = "Serial Number", title = "February 2022 Deployments") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 0.5, size = 14),
        axis.text.y = element_text(size = 12),
        text = element_text(size = 16, family = "serif"),
        plot.title = element_text(size = 15, hjust = 0.45),
        legend.position = "inside", 
        legend.position.inside = c(0.85, 0.25),
        legend.key.size = unit(8, 'cm'),
        legend.key.height = unit(8, 'mm'),
        legend.key.width = unit(6, 'mm'),
        legend.text = element_text(size = 12, hjust = 0, vjust = 0.5),
        legend.background = element_rect(fill = "white", colour = "black"),
        legend.justification = "center",
        legend.spacing.x = unit(0.1, "mm"),
        legend.spacing.y = unit(-0.25, "mm")) +
  scale_x_date(date_breaks = "2 month", date_minor_breaks = "2 month", date_labels = "%b. %Y") +
  scale_colour_manual(values = c("#a0ea8c", "#df5949"), 
                      labels = c("Deployment", "Last Known Fix")) +
  guides(colour = guide_legend(title = NULL))
#ggsave(here::here('output/figures/2022_Deployments_Nov072024.png'), height = 10, width = 20, units = 'in')
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Run the points starting from January 1st 2023 to current
{loss_contact_23 <- as_tibble(post_season_track)%>%
  filter(grepl('23-', local_identifier)) %>%
  filter(timestamp > as.POSIXct("2023-01-01 00:00:01", tz = "America/Chicago")) %>% 
  select(timestamp, local_identifier, serial_no)
loss_contact_head_23 <- loss_contact_23%>%
  group_by(local_identifier, serial_no)%>%
  slice_head(n = 1)%>%
  mutate(fixes = "first_fix")%>%
  ungroup()
loss_contact_tail_23 <- loss_contact_23%>%
  group_by(local_identifier, serial_no)%>%
  slice_tail(n = 1)%>%
  mutate(fixes = "last_fix")%>%
  ungroup()
loss_contact_start2end_23 <- rbind(loss_contact_head_23, loss_contact_tail_23) %>%
  group_by(local_identifier, serial_no)%>%
  pivot_wider(names_from = fixes, values_from = timestamp)
loss_contact_start2end_23$first_fix <- as_date(loss_contact_start2end_23$first_fix)
loss_contact_start2end_23$last_fix <- as_date(loss_contact_start2end_23$last_fix)}


ggplot(loss_contact_start2end_23, aes(x = first_fix, y = serial_no)) +
  geom_segment(aes(xend = last_fix, yend = serial_no), colour = "black", linewidth = 1) +
  geom_point(aes(x = first_fix, colour = "First Fix"), 
             size = 2.5, stroke = 1, fill = "#a0ea8c", shape = 21, color = "black") +
  geom_point(aes(x = last_fix, colour = "Last Fix"), 
             size = 2, stroke = 1, fill = "#df5949", shape = 22, color = "black") +
  labs(x = "", y = "Serial Number", title = "January 2023 Deployments") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 0.5, size = 14),
        axis.text.y = element_text(size = 12),
        text = element_text(size = 16, family = "serif"),
        plot.title = element_text(size = 15, hjust = 0.45),
        legend.position = "inside", 
        legend.position.inside = c(0.85, 0.25),
        legend.key.size = unit(8, 'cm'),
        legend.key.height = unit(8, 'mm'),
        legend.key.width = unit(6, 'mm'),
        legend.text = element_text(size = 12, hjust = 0, vjust = 0.5),
        legend.background = element_rect(fill = "white", colour = "black"),
        legend.justification = "center",
        legend.spacing.x = unit(0.1, "mm"),
        legend.spacing.y = unit(-0.25, "mm")) +
  scale_x_date(date_breaks = "1 month", date_minor_breaks = "1 month", date_labels = "%b. %Y") +
  scale_colour_manual(values = c("#a0ea8c", "#df5949"), 
                      labels = c("Deployment", "Last Known Fix")) +
  guides(colour = guide_legend(title = NULL))
#ggsave(here::here('output/figures/2023_Deployments_Nov072024.png'), height = 10, width = 20, units = 'in')

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Run the points starting from December 19th 2023 to current
{loss_contact_24 <- as_tibble(post_season_track)%>%
  filter(grepl('NOPI24', local_identifier)) %>%
  filter(timestamp > as.POSIXct("2023-12-15 00:00:01", tz = "America/Chicago")) %>% 
  select(timestamp, local_identifier, serial_no)
loss_contact_head_24 <- loss_contact_24%>%
  group_by(local_identifier, serial_no)%>%
  slice_head(n = 1)%>%
  mutate(fixes = "first_fix")%>%
  ungroup()
loss_contact_tail_24 <- loss_contact_24%>%
  group_by(local_identifier, serial_no)%>%
  slice_tail(n = 1)%>%
  mutate(fixes = "last_fix")%>%
  ungroup()
loss_contact_start2end_24 <- rbind(loss_contact_head_24, loss_contact_tail_24) %>%
  group_by(local_identifier, serial_no)%>%
  pivot_wider(names_from = fixes, values_from = timestamp )
loss_contact_start2end_24$first_fix <- as_date(loss_contact_start2end_24$first_fix)
loss_contact_start2end_24$last_fix <- as_date(loss_contact_start2end_24$last_fix)}


ggplot(loss_contact_start2end_24, aes(x = first_fix, y = serial_no)) +
  geom_segment(aes(xend = last_fix, yend = serial_no), colour = "black", linewidth = 1) +
  geom_point(aes(x = first_fix, colour = "First Fix"), 
             size = 2.5, stroke = 1, fill = "#a0ea8c", shape = 21, color = "black") +
  geom_point(aes(x = last_fix, colour = "Last Fix"), 
             size = 2, stroke = 1, fill = "#df5949", shape = 22, color = "black") +
  labs(x = "", y = "Serial Number", title = "January 2024 Deployments") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 0.5, size = 14),
        axis.text.y = element_text(size = 12),
        text = element_text(size = 16, family = "serif"),
        plot.title = element_text(size = 15, hjust = 0.45),
        legend.position = "inside", 
        legend.position.inside = c(0.85, 0.25),
        legend.key.size = unit(8, 'cm'),
        legend.key.height = unit(8, 'mm'),
        legend.key.width = unit(6, 'mm'),
        legend.text = element_text(size = 12, hjust = 0, vjust = 0.5),
        legend.background = element_rect(fill = "white", colour = "black"),
        legend.justification = "center",
        legend.spacing.x = unit(0.1, "mm"),
        legend.spacing.y = unit(-0.25, "mm")) +
  scale_x_date(date_breaks = "2 week", date_minor_breaks = "2 week", date_labels = "%b. %d %Y") +
  scale_colour_manual(values = c("#a0ea8c", "#df5949"), 
                      labels = c("Deployment", "Last Known Fix")) +
  guides(colour = guide_legend(title = NULL))
#ggsave(here::here('output/figures/2024_Deployments_Nov072024.png'), height = 10, width = 20, units = 'in')

# ------ 2025 Deployments --------------------------------

{loss_contact_25 <- as_tibble(post_season_track)%>%
  filter(grepl('NOPI25', local_identifier)) %>%
  #filter(timestamp > as.POSIXct("2024-12-15 00:00:01", tz = "America/Chicago")) %>% 
  select(timestamp, local_identifier, serial_no)
loss_contact_head_25 <- loss_contact_25%>%
  group_by(local_identifier, serial_no)%>%
  slice_head(n = 1)%>%
  mutate(fixes = "first_fix")%>%
  ungroup()
loss_contact_tail_25 <- loss_contact_25%>%
  group_by(local_identifier, serial_no)%>%
  slice_tail(n = 1)%>%
  mutate(fixes = "last_fix")%>%
  ungroup()
loss_contact_start2end_25 <- rbind(loss_contact_head_25, loss_contact_tail_25) %>%
  group_by(local_identifier, serial_no)%>%
  pivot_wider(names_from = fixes, values_from = timestamp )
loss_contact_start2end_25$first_fix <- as_date(loss_contact_start2end_25$first_fix)
loss_contact_start2end_25$last_fix <- as_date(loss_contact_start2end_25$last_fix)}


ggplot(loss_contact_start2end_25, aes(x = first_fix, y = serial_no)) +
  geom_segment(aes(xend = last_fix, yend = serial_no), colour = "black", linewidth = 1) +
  geom_point(aes(x = first_fix, colour = "First Fix"), 
             size = 2.5, stroke = 1, fill = "#a0ea8c", shape = 21, color = "black") +
  geom_point(aes(x = last_fix, colour = "Last Fix"), 
             size = 2, stroke = 1, fill = "#df5949", shape = 22, color = "black") +
  labs(x = "", y = "Serial Number", title = "January 2025 Deployments") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 0.5, size = 14),
        axis.text.y = element_text(size = 12),
        text = element_text(size = 16, family = "serif"),
        plot.title = element_text(size = 15, hjust = 0.45),
        legend.position = "inside", 
        legend.position.inside = c(0.85, 0.25),
        legend.key.size = unit(8, 'cm'),
        legend.key.height = unit(8, 'mm'),
        legend.key.width = unit(6, 'mm'),
        legend.text = element_text(size = 12, hjust = 0, vjust = 0.5),
        legend.background = element_rect(fill = "white", colour = "black"),
        legend.justification = "center",
        legend.spacing.x = unit(0.1, "mm"),
        legend.spacing.y = unit(-0.25, "mm")) +
  scale_x_date(date_breaks = "1 week", date_minor_breaks = "1 week", date_labels = "%b. %d %Y") +
  scale_colour_manual(values = c("#a0ea8c", "#df5949"), 
                      labels = c("Deployment", "Last Known Fix")) +
  guides(colour = guide_legend(title = NULL))
#ggsave(here::here('output/figures/2024_Deployments_Nov072024.png'), height = 10, width = 20, units = 'in')

