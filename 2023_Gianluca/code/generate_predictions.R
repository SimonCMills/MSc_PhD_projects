# generate predicted occupancy
# note that the model was fitted under outdated version of flocker, so have 
# needed to reformat a bit (and supply some additional columns to 
# fitted_flocker) to get processing functions to work. 

# packages
library(flocker); library(brms); library(dplyr); library(ggplot2)

fit <- readRDS("../../Rainforest Builder Dropbox/Simon Mills/Gian/fit.rds")
fd <- readRDS("fd_01-10-23.rds")

# fix attributes to work with updated flocker version
# attr(fit, "data_type") <- "single"
# attr(fit, "fp") <- FALSE
# attr(fit, "lik_type") <- NULL

# get fd data
fd_data <- fd$data[1:fd$data$ff_n_unit[1],]

# check scaling coefs are correct ----
# plantation age
plantation_age <- fd_data$plantation_age
plantation_age[plantation_age == -99] <- NA
plantation_age_sc <- scale(plantation_age)
plantation_age_sc[is.na(plantation_age_sc)] <- 0
all(plantation_age_sc - fd_data$plantation_age_sc == 0)

# time since logging
time_since_logging <- fd_data$time_since_logging
time_since_logging[time_since_logging == -99] <- NA
time_since_logging_sc <- scale(time_since_logging)
time_since_logging_sc[is.na(time_since_logging_sc)] <- 0
all(time_since_logging_sc - fd_data$time_since_logging_sc == 0)


# all good so extract scaling info
# plantation age
plant_age_cent <- attr(plantation_age_sc, "scaled:center")
plant_age_scale <- attr(plantation_age_sc, "scaled:scale")

# time since logging
time_since_logging_cent <- attr(time_since_logging_sc, "scaled:center")
time_since_logging_scale <- attr(time_since_logging_sc, "scaled:scale")

# range(time_since_logging, na.rm = T)
# range(plantation_age, na.rm = T)

plantation_age_seq <- tibble(plantation_age = 0:13) %>%
    mutate(plantation_age_sc = (plantation_age - plant_age_cent)/plant_age_scale)

time_since_logging_seq <- tibble(time_since_logging = 19:62) %>%
    mutate(time_since_logging_sc = 
               (time_since_logging - time_since_logging_cent)/time_since_logging_scale)


# okay, now into the full expand.grid bit. 
pred_data <- fd_data %>%
    select(habitat, species, 
           dependency, forestdep_high, forestdep_med, forestdep_low, 
           primary, once_logged, twice_logged, logged_restored, eucalyptus, albizia) %>%
    unique %>%
    mutate(#time_since_logging_sc = 0, plantation_age_sc = 0, 
           time_of_day = 0, ABC50_sc = 0, 
           observer = "SM", 
           observer_sp = fd$data$observer_sp[1], 
           year = fd$data$year[1], 
           year_sp = fd$data$year_sp[1], 
           site = fd$data$site[1],
           site_sp = fd$data$site_sp[1])
           #ff_y = 0, ff_n_unit = fd$data$n_unit[1], ff_n_rep = 4, ff_Q = 1, 
           #ff_rep_index1 = 1, ff_rep_index2 = 1, ff_rep_index3 = 1, ff_rep_index4 = 1,
           y = 0,
           #n_unit = fd$data$n_unit[1], n_rep = 4,  Q = 0, 
           rep_index1 = 1, rep_index2 = 1, rep_index3 = 1,
           rep_index4 = 1)


pred_time_since_logging <- pred_data %>%
    filter(habitat %in% c("Once_logged", "Restored")) %>%
    replicate(nrow(time_since_logging_seq), ., FALSE) %>%
    bind_rows(., .id = "id") %>%
    mutate(time_since_logging = (19:62)[as.integer(id)]) %>%
    left_join(., time_since_logging_seq) %>%
    mutate(plantation_age = NA, plantation_age_sc = 0)

pred_plantation_age <- pred_data %>%
    filter(habitat %in% c("Eucalyptus_pellita", "Albizia_falcataria")) %>%
    replicate(nrow(plantation_age_seq), ., FALSE) %>%
    bind_rows(., .id = "id") %>%
    mutate(plantation_age = (0:13)[as.integer(id)]) %>%
    left_join(., plantation_age_seq) %>%
    mutate(time_since_logging = NA, time_since_logging_sc = 0)


pred_data_full <- pred_data %>%
    filter(!(habitat %in% c("Eucalyptus_pellita", "Albizia_falcataria", 
                            "Once_logged", "Restored"))) %>%
    mutate(time_since_logging = NA, time_since_logging_sc = 0, 
           plantation_age = NA, plantation_age_sc = 0) %>%
    bind_rows(., pred_time_since_logging) %>%
    bind_rows(., pred_plantation_age) %>%
    select(-id)

preds <- fitted_flocker(fit, components = "occ", new_data = pred_data_full, 
                        draw_ids = seq(1, 4000, 8))

# pdat_red <- pred_data_full %>%
#     select(-(ff_y:rep_index4))


out <- as_tibble(preds$linpred_occ) %>%
    setNames(paste0("draw_", 1:500)) %>%
    bind_cols(pred_data_full, .)

out_summ <- tibble(mid = matrixStats::rowMeans2(preds$linpred_occ), 
                   lwr = matrixStats::rowQuantiles(preds$linpred_occ, probs = .1),
                   upr = matrixStats::rowQuantiles(preds$linpred_occ, probs = .9)) %>%
    bind_cols(pred_data_full, .)
    
library(ggplot2)
out_summ %>%
    mutate(dependency_label = case_when(dependency == "none" ~ "low", 
                                        TRUE ~ dependency), 
           dependency_label = factor(dependency_label, levels = c("high", "medium", "low"))) %>%
    filter(habitat %in% c("Eucalyptus_pellita", "Albizia_falcataria")) %>%
    ggplot(aes(plantation_age, mid, group=species)) +
    geom_line() +
    facet_grid(habitat~dependency_label) +
    theme(strip.text = element_text(hjust=0, face="bold"), 
          strip.background = element_blank(), 
          axis.text = element_text(colour="black")) +
    labs(y = "P(occupancy)", x = "Plantation age")
ggsave("figures/plantation_age_estimates.png", units="mm", height=150, width=230)

out_summ %>%
    mutate(dependency_label = case_when(dependency == "none" ~ "low", 
                                        TRUE ~ dependency), 
           dependency_label = factor(dependency_label, levels = c("high", "medium", "low"))) %>%
    filter(habitat %in% c("Once_logged", "Restored")) %>% 
    ggplot(aes(time_since_logging, mid, group=species)) +
    geom_line() +
    facet_grid(habitat~dependency_label) +
    theme(strip.text = element_text(hjust=0, face="bold"), 
          strip.background = element_blank(), 
          axis.text = element_text(colour="black")) +
    labs(y = "P(occupancy)", x = "Plantation age")
ggsave("figures/time_since_logging_estimates.png", units="mm", height=150, width=230)

saveRDS(out, "outputs/predicted_occupancy_500_draws.rds")
saveRDS(out_summ, "outputs/predicted_occupancy_500_draws_summarised.rds")
