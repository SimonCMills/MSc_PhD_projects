# Clean and format data for passing to flocker
# Notes:
#   - there's a 21:xx time needs fixing
#   - A couple of visits are missing times
#   - there are some point counts within the same hour.... worth checking on this
#   - There are some points with only two visits

# housekeeping ----
library(flocker); library(dplyr)

## function to convert time to numeric (hours past 6) and make wide-format
## note: this is probably now redundant
# time_to_hps <- function(time) {
#     time <- substr(time, 1, 5)
#     minutes <- gsub("^.*:(.*)", "\\1", time)
#     hours <- gsub("^(.*):.*$", "\\1", time)
#     (as.numeric(hours) + as.numeric(minutes)/60) - 6
# }

# data ---- 
df <- data.table::fread("inputs/Master2Birds_allPCs_Lidar_Traits_UndetectedSpp.csv") %>%
    as_tibble() %>%
    # convert date column to date format
    mutate(Date = as.Date(Date, format = "%d/%m/%Y")) 

df_habitat <- data.table::fread("inputs/Plantation_Habitat_Structure.csv") %>%
    as_tibble() %>%
    mutate(point_id = interaction(Site, Point))

# fix dataframe ----
## (1) fix timing formatting
df2 <- df %>%
    mutate(hours = gsub("^([0-9][0-9]).*", "\\1", Time), 
           minutes = gsub("^[0-9][0-9](.*)", "\\1", Time), 
           minutes = gsub("([0-9][0-9]):00", "\\1", minutes),
           minutes = gsub("\\.|\\:", "", minutes), 
           # as numeric
           hours = as.numeric(hours), 
           # note that a couple of visits are missing times: this needs fixing. When 
           # fixed remove this line 
           hours = ifelse(is.na(hours), 6, hours),
           minutes = as.numeric(minutes),
           hps = (hours + minutes/60) - 6, 
           # format date_time in order to sort out visit ID 
           minutes_char = as.character(minutes), 
           minutes_char = ifelse(nchar(minutes_char) == 1, 
                                 paste0("0", minutes_char), 
                                 minutes_char),
           date_time = as.POSIXct(paste0(Date, "-", hours, "-", minutes_char), 
                                  format = "%Y-%m-%d-%H-%M"))

## Note: worth visual checking that these split out hours and minutes properly
# df2 %>%
#     select(Time, hours, minutes, date_time) %>%
#     unique %>%
#     View

## (2) fix duplicated rows in Simon Mitchell's bit of the dataset (corresponding 
## to observations from different bands)
df_SM <- df2 %>%
    filter(Sampler == "SM") %>%
    group_by(Site, point, spp) %>%
    mutate(abundance = sum(abundance)) %>%
    slice(1)

## df3 is a dataframe with duplicated rows removed and time formatting sorted
df3 <- df2 %>%
    filter(Sampler != "SM") %>%
    bind_rows(., df_SM) %>%
    mutate(point_id = interaction(Site, point))


# Create 0-filled dataset ----
## (some discrepancies previously so have redone here)
## first need to recalculate Day column (as some sites have missing Day info)
date_info <- df3 %>%
    select(Site, point_id, Date, date_time, Time, Day, hps) %>%
    unique %>%
    group_by(Site, point_id) %>%
    # there are some point counts within the same hour.... worth checking on this
    mutate(Day = rank(date_time))

## select just non-0 rows
df_non0 <- df3 %>%
    filter(abundance > 0) %>%
    # remove Day columb and join recalculated Day column into dataframe
    select(-Day) %>%
    left_join(., date_info)

## generate backbone dataframe with all point:visit:species combinations
df_backbone <- with(df3, expand.grid(point_id = unique(point_id), 
                                     Day = 1:4,
                                     spp = unique(spp)))

## join to create full dataframe where abundance is 0 if point visit was made 
## but species not detected, 1 if species detected, and NA if point visit 
## doesn't exist
df_full <- date_info %>%
    mutate(point_visited = 1) %>%
    left_join(df_backbone, .) %>%
    left_join(., 
              df_non0[c("point_id", "Day", "spp", "abundance")]) %>%
    mutate(abundance = ifelse(is.na(abundance) & point_visited==1, 0, abundance))

df_full2 <- df3 %>%
    select(Date, point_id, Time) %>%
    unique %>%
    left_join(df_full, .) 


# Format dataframe for passing to flocker ----
# note: df_full2 is the cleaned dataframe to use hereafter

## make detection data wide-format
# note: if you get warnings about aggregating by length, then there is a 
# problem upstream. This warning indicates that there are at least some
# point:visit:species combinations that are not unique (which they should be) 
df_det_wf <- df_full2 %>%
    mutate(det = ifelse(abundance == 0, 0, 1)) %>%
    reshape2::dcast(formula = point_id + spp ~ Day, value.var = "det") %>%
    rename(species = spp, d1 = `1`, d2 = `2`, d3 = `3`, d4 = `4`)

## make visit time numeric and make wide-format
df_time_wf <- left_join(df_full2, date_info) %>%
    select(point_id, Day, hps) %>%
    mutate(hps_sc = scale(hps)) %>%
    unique %>% 
    reshape2::dcast(formula = point_id ~ Day, value.var = c("hps_sc"))  %>%
    rename(hps_sc1 = `1`, hps_sc2 = `2`, hps_sc3 = `3`, hps_sc4 = `4`) 

## sort out covariates
df_cov <- df3 %>%
    select(point_id, 
           site = Site,
           observer = Sampler,
           year = Year, 
           time_since_logging = Time_Since_Logging, 
           habitat = Habitat, 
           ABC50) %>%
    unique %>%
    # join in plantation age info
    left_join(., df_habitat[c("point_id", "Age")]) %>%
    rename(plantation_age = Age) 
    
## full analysis dataframe 
df_af <- left_join(df_det_wf, df_time_wf) %>%
    left_join(., df_cov) %>%
    mutate(time_since_logging_sc = scale(time_since_logging), 
           ABC50_sc = scale(ABC50), 
           plantation_age_sc = scale(plantation_age), 
           time_since_logging_sc = ifelse(is.na(time_since_logging_sc), 
                                          -99, 
                                          time_since_logging_sc),
           ABC50_sc = ifelse(is.na(ABC50_sc), 
                             -99, 
                             ABC50_sc),
           plantation_age_sc = ifelse(is.na(plantation_age_sc), 
                                      -99, 
                                      plantation_age_sc)) %>%
    mutate(site_sp = interaction(site, species), 
           observer_sp = interaction(observer, species), 
           year_sp = interaction(year, species), 
           # habitat vars 
           primary = ifelse(habitat == "Primary", 1, -1),
           twice_logged = ifelse(habitat == "Twice_logged", 1, -1), 
           once_logged = ifelse(habitat == "Once_logged", 1, -1), 
           logged_restored = ifelse(habitat == "Restored", 1, -1),
           eucalyptus = ifelse(habitat == "Eucalyptus_pellita", 1, -1), 
           albizia = ifelse(habitat == "Albizia_falcataria", 1, -1),
           forestdep_high = ifelse(habitat == "high", 1, -1), 
           forestdep_med = ifelse(habitat == "medium", 1, -1), 
           forestdep_low = ifelse(habitat %in% c("low", "none"), 1, -1)
           )
  
fd <- flocker::make_flocker_data(
    obs = as.matrix(select(df_af, d1:d4)), 
    unit_covs = select(df_af, 
                       primary:albizia,
                       year, year_sp, 
                       species, 
                       forestdep_high:forestdep_low,
                       observer, observer_sp), 
    list(time_of_day = select(df_af, hps_sc1:hps_sc4)))
