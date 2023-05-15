# Clean and format data for passing to flocker
# Notes:

#   - I think we need to set the mean of LiDAR for plantation points (but I 
#   can't remember exactly how or why to do this)
#   - One point (gc1l-2.9 wasn't visited on day 3 due to logistical constraints - so NA)
#   - only a few points were collected for 4 days, so most are NA on day 4 
#   - I filter data from Daniel to <100 m distance 

# housekeeping ----
#install.packages("remotes")
#remotes::install_github("jsocolar/flocker")
library(flocker);
library(dplyr)


## function to convert time to numeric (hours past 6) and make wide-format
## note: this is probably now redundant
# time_to_hps <- function(time) {
#     time <- substr(time, 1, 5)
#     minutes <- gsub("^.*:(.*)", "\\1", time)
#     hours <- gsub("^(.*):.*$", "\\1", time)
#     (as.numeric(hours) + as.numeric(minutes)/60) - 6
# }

# full PC data ---- 
df_bird <- data.table::fread("inputs/MasterBirds_allPCs_Lidar_Traits_UndetectedSpp.csv") %>%
    as_tibble() %>%
    # convert date column to date format
    mutate(Date = as.Date(Date, format = "%d/%m/%Y")) 

#plantation age information
df_habitat <- data.table::fread("inputs/Plantation_Habitat_Structure.csv") %>%
    as_tibble() %>%
    mutate(point_id = interaction(Site, Point))

#iucn status and forest dependency:low,medium,high (none reclassified to low)  (non-resident = 1; we get rid of these 6 spp)
df_IUCN_FD <- data.table::fread("inputs/SppForForIntegrityIUCN.csv") %>% 
    filter(nonResident == 0) %>%
    as_tibble()

# DOUBLECHECK THIS; filter out any of Daniel's data that is >100m away to enable matching across datesets 
df <- df_IUCN_FD %>%  
    left_join(., df_bird, by = "spp") %>%
    filter(!(Sampler == "Daniel_Kong" & distance > 100))

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
## to observations of the same species and point in  different bands (e.g. diff birds))
df_SM <- df2 %>%
    filter(Sampler == "SM") %>%
    group_by(Site, point, spp,Date) %>%
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
              df_non0[on =c("point_id", "Day", "spp","abundance")]) %>%
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

## sort out point covariates
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

df_cov_sp <- df3 %>%
    select(species = spp, dependency = forestDependency) %>%
    unique

    
## full analysis dataframe 
df_af <- left_join(df_det_wf, df_time_wf) %>%
    left_join(., df_cov) %>%
    left_join(., df_cov_sp) %>%
    mutate(time_since_logging_sc = scale(time_since_logging), 
           ABC50_sc = scale(ABC50), 
           plantation_age_sc = scale(plantation_age), 
           plantation_age_sc = scale(plantation_age), 
           time_since_logging_sc = ifelse(is.na(time_since_logging_sc), 
                                          0, 
                                          time_since_logging_sc),
           ABC50_sc = ifelse(is.na(ABC50_sc), 
                             0, 
                             ABC50_sc),
           plantation_age_sc = ifelse(is.na(plantation_age_sc), 
                                      0, 
                                      plantation_age_sc)) %>%
    mutate(site_sp = interaction(site, species), 
           observer_sp = interaction(observer, species), 
           year_sp = interaction(year, species), 
           # habitat vars 
           primary = ifelse(habitat == "Primary", 1, -1),
           twice_logged = ifelse(habitat == "Twice_logged", 1, 0), 
           once_logged = ifelse(habitat == "Once_logged", 1, 0), 
           logged_restored = ifelse(habitat == "Restored", 1, 0),
           eucalyptus = ifelse(habitat == "Eucalyptus_pellita", 1, 0), 
           albizia = ifelse(habitat == "Albizia_falcataria", 1, 0),
           forestdep_high = ifelse(dependency == "high", 1, -1), 
           forestdep_med = ifelse(dependency == "medium", 1, -1), 
           forestdep_low = ifelse(dependency %in% c("low", "none"), 1, -1)
    )


#Note that GC1L-2.9 on day 3 wasn't sampled due to logistical constraints,
# so put NA for abundance in d3
# Note: this is redundant: it is already treated as NA in the above code
# df_af <- df_af %>%
#     mutate(d3 = ifelse(point_id =="GC1L-2.9" , NA, d3))

#Last checks 
#--------------------------------
# #NOTE
# colnames(df_af)
# # calculate the number of NAs in each column
# na_count <- colSums(is.na(df_af))
# # get the names of columns with NAs
# cols_with_na <- names(na_count[na_count > 0])
# # print the names of columns with NAs
# cols_with_na
# #which columns don't have d3 info? 
# d3NA <- df_af %>% filter(is.na(d3)) #only GC1L-2.9 - good	 
# d4NA <- df_af %>% filter(is.na(d4)) #	Lots of points dont have d4 data, as 
# expected (most points were only sampled 3 times) 
# hps_sc3NA <- df_af%>% filter(is.na(hps_sc3)) %>% select(point_id) %>% unique() 
# Day 3; GC1L-2.9 is missing time data, good
# hps_sc4NA <- df_af%>% filter(is.na(hps_sc4)) %>% select(point_id) %>% unique() 
# Lots of points dont have d4 time; as expected
#--------------------------------

#EXPORT spp names to get forest integrity data for 
#spp <- as.data.frame(df_af$species %>% unique) %>% rename("Species" = 1)
#write.csv(spp, "SppForForIntegrity.csv")

fd <- flocker::make_flocker_data(obs = as.matrix(select(df_af, d1:d4)), 
                                 unit_covs = select(df_af, 
                                                    point_id,
                                                    habitat,
                                                    primary:albizia,
                                                    time_since_logging_sc,
                                                    ABC50_sc, 
                                                    plantation_age_sc,
                                                    year, year_sp, 
                                                    site, site_sp,
                                                    species, 
                                                    dependency,
                                                    forestdep_high:forestdep_low,
                                                    observer, observer_sp), 
                                 list(time_of_day = select(df_af, hps_sc1:hps_sc4)))


saveRDS(fd, "fd_15-05-23.rds")


