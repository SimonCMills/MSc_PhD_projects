# data checking and exploration

# housekeeping ----
library(dplyr); library(ggplot2)

# read data & format ----
fd <- readRDS("fd_22-05-23.rds")

# format data
fd_data <- fd$data %>%
    slice(1:fd$data$n_unit[1]) %>%
    mutate(dependency = factor(dependency, 
                               levels = c("high", "medium", "low", "none")), 
           habitat = case_when(grepl("Albizia", habitat) ~ "albizia", 
                               grepl("Eucalyptus", habitat) ~ "eucalyptus", 
                               TRUE ~ tolower(habitat)),
           habitat = factor(habitat, 
                            levels = c("primary", "restored", 
                                       "once_logged", "twice_logged", 
                                       "albizia", "eucalyptus")))
    

# number of species ----
(n_species <- length(unique(fd_data$species)))

# distribution of number of visits ----
fd_data %>%
    select(site, point_id, n_rep) %>%
    unique %>% 
    filter(n_rep != -99) %>%
    group_by(n_rep) %>%
    summarise(n_point = n())

# distribution of Q ----
fd_data %>%
    group_by(species) %>%
    summarise(sumQ = sum(Q)) %>%
    group_by(sumQ = cut(sumQ, 
                 c(-.5, .5, 1.5, 2.5, 5.5, 10.5, 20.5, 40.5, 80.5, 1000), 
                 labels = c(0, 1, 2, "3-5", "6-10", "11-20", "21-40", "40-80", "81+"))) %>%
    summarise(n_species = n()) 


# plot sumQ by dependency & habitat ----
fd_data %>%
    group_by(species, habitat, dependency) %>%
    summarise(sumQ = sum(Q)) %>%
    arrange(sumQ) %>%
    ggplot(aes(habitat, sumQ)) + 
    geom_boxplot() + 
    facet_wrap(~dependency) +
    theme(axis.text.x = element_text(angle=45, hjust=1))

ggsave("figures/species_sumQ_by_dependency&habitat.png", 
       units = "mm", height=120, width=166)

# alternative version
fd_data %>%
    group_by(species, habitat, dependency) %>%
    summarise(sumQ = sum(Q)) %>%
    arrange(sumQ) %>%
    ggplot(aes(dependency, sumQ, col = dependency)) + 
    geom_boxplot() + 
    facet_wrap(~habitat)


# sumQ covariation across habitat ----
df_sumQ <- fd_data %>%
    group_by(species, habitat, dependency) %>%
    summarise(sumQ = sum(Q)) 

df_sumQ_wf <- reshape2::dcast(df_sumQ, species ~ habitat, value.var = "sumQ") %>%
    left_join(., df_sumQ %>% select(species, dependency) %>% unique)

df_sumQ_wf %>%
    ggplot(aes(primary, restored, col=dependency)) +
    geom_point() +
    facet_wrap(~dependency)

df_sumQ_wf %>%
    ggplot(aes(primary, eucalyptus, col=dependency)) +
    geom_point() +
    facet_wrap(~dependency)

df_sumQ_wf %>%
    ggplot(aes(primary, albizia, col=dependency)) +
    geom_point() +
    facet_wrap(~dependency)

df_sumQ_wf %>%
    ggplot(aes(primary, once_logged, col=dependency)) +
    geom_point() +
    facet_wrap(~dependency)


# plot ABC50, age, and time since logging ----
fd_data %>%
    select(point_id, habitat, ABC50) %>%
    unique %>%
    ggplot(aes(habitat, ABC50)) + 
    geom_jitter(height = 0, width = .3)

fd_data %>%
    select(point_id, habitat, plantation_age) %>%
    unique %>%
    ggplot(aes(habitat, plantation_age)) + 
    geom_jitter(height = 0, width = .3)

fd_data %>%
    select(point_id, habitat, time_since_logging) %>%
    unique %>%
    ggplot(aes(habitat, time_since_logging)) + 
    geom_jitter(height = 0, width = .3)


# ABC50 values against covariates ----
fd_data %>%
    select(point_id, habitat, time_since_logging, ABC50) %>%
    unique %>%
    filter(time_since_logging != -99) %>%
    ggplot(aes(time_since_logging, ABC50)) + 
    geom_point()

fd_data %>%
    select(point_id, habitat, plantation_age, ABC50) %>%
    unique %>%
    filter(plantation_age != -99) %>%
    ggplot(aes(plantation_age, ABC50)) + 
    geom_point()


# check data ----
fd_summ <- fd_data %>%
    group_by(species, habitat, dependency) %>%
    summarise(sumQ = sum(Q)) %>%
    ungroup %>%
    arrange(sumQ)

## each species only has a single forest dependency ----
fd_data %>%
    select(species, dependency) %>%
    unique %>%
    nrow(.) == n_species
    
## each species has the same number of points ----
fd_data %>% 
    group_by(species) %>%
    summarise(N = n()) %>%
    pull(N) %>%
    unique(.) %>%
    length(.) == 1

## check: scaled variables look correct----
fd_data %>%
    filter(species == species[1]) %>%
    pull(time_of_day) %>%
    hist

fd_data %>%
    select(point_id, habitat, ABC50_sc) %>%
    unique %>%
    ggplot(aes(habitat, ABC50_sc)) + 
    geom_jitter(height = 0, width = .3)

fd_data %>%
    select(point_id, habitat, plantation_age_sc) %>%
    unique %>%
    ggplot(aes(habitat, plantation_age_sc)) + 
    geom_jitter(height = 0, width = .3)

fd_data %>%
    select(point_id, habitat, time_since_logging_sc) %>%
    unique %>%
    ggplot(aes(habitat, time_since_logging_sc)) + 
    geom_jitter(height = 0, width = .3)

###
df_full2 %>%
    mutate(det = ifelse(abundance == 0, 0, 1)) %>%
    group_by(Site, spp) %>%
    summarise(abundance = (mean(abundance)), det = mean(det)) %>%
    ggplot(aes(det, abundance)) + 
    geom_point() + scale_y_continuous(trans="log")
