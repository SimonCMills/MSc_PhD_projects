library(dplyr); library(ggplot2)

fd <- readRDS("fd_15-05-23.rds")


fd_data <- fd$data %>%
    slice(1:fd$data$n_unit[1]) %>%
    mutate(dependency = factor(dependency, levels = c("high", "medium", "low", "none")))
    
# distribution of number of visits ----
fd_data %>%
    select(site, point_id, n_rep) %>%
    unique %>% 
    filter(n_rep != -99) %>%
    group_by(n_rep) %>%
    summarise(n())

(n_species <- length(unique(fd_data$species)))
# 387 points:
# A tibble: 3 × 2
#   n_rep `n()`
#   <dbl> <int>
#1     2     2
#2     3   356
#3     4    39

# distribution of Q ----
fd_data %>%
    group_by(species) %>%
    summarise(sumQ = sum(Q)) %>%
    group_by(sumQ = cut(sumQ, 
                 c(-.5, .5, 1.5, 2.5, 5.5, 10.5, 20.5, 40.5, 80.5, 1000), 
                 labels = c(0, 1, 2, "3-5", "6-10", "11-20", "21-40", "40-80", "81+"))) %>%
    summarise(N = n()) 


##
fd_data %>%
    group_by(species, habitat, dependency) %>%
    summarise(sumQ = sum(Q)) %>%
    arrange(sumQ) %>%
    ggplot(aes(habitat, sumQ, col = dependency)) + 
    geom_boxplot() + 
    facet_wrap(~dependency)

# 
fd_data %>%
    group_by(species, habitat, dependency) %>%
    summarise(sumQ = sum(Q)) %>%
    arrange(sumQ) %>%
    ggplot(aes(dependency, sumQ, col = dependency)) + 
    geom_boxplot() + 
    facet_wrap(~habitat)


fd_summ <- fd_data %>%
    group_by(species, habitat, dependency) %>%
    summarise(sumQ = sum(Q)) %>%
    ungroup %>%
    arrange(sumQ)

#  check: each species only has a single forest dependency ----
fd_data %>%
    select(species, dependency) %>%
    unique %>%
    nrow(.) == n_species
    
# check: each species has the same number of points ----
fd_data %>% 
    group_by(species) %>%
    summarise(N = n()) %>%
    pull(N) %>%
    unique(.) %>%
    length(.) == 1

# check: scaled variables ----
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




