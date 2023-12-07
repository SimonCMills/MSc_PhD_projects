# Unpacking outputs
library(flocker); library(dplyr); library(brms)

fit <- readRDS("outputs/fit_v3.rds")
fd <- readRDS("fd_22-05-23.rds")

# plot chains
plot(fit)

# the combined effect is: 
pred_data <- fd$data %>%
    select(habitat, species, 
           dependency, forestdep_high, forestdep_med, forestdep_low, 
           primary, once_logged, twice_logged, logged_restored, eucalyptus, albizia) %>%
    unique %>%
    mutate(time_since_logging_sc = 0, plantation_age_sc = 0, 
           time_of_day = 0, ABC50_sc = 0, observer = "SM", 
           observer_sp = fd$data$observer_sp[1], 
           year = fd$data$year[1], 
           year_sp = fd$data$year_sp[1], 
           site = fd$data$site[1],
           site_sp = fd$data$site_sp[1], 
           n_unit = fd$data$n_unit[1], n_rep = 4, Q = 1, 
           rep_index1 = 1, rep_index2 =1, rep_index3 = 1, rep_index4 = 1)

fits <- fitted_flocker(fit, "occupancy", new_data = pred_data, 
                       re_formula = NA, ndraws = 200)


fits_sp <- fitted_flocker(fit, "occupancy", new_data = pred_data, #summarise = F,
                       re_formula = ~(0 + primary + twice_logged + once_logged + logged_restored + 
                                          eucalyptus + albizia|g1|species), ndraws = 200)

bind_cols(pred_data, fits) %>%
    mutate(dependency = ifelse(dependency == "none", "low", dependency), 
           dependency = factor(dependency, 
                               levels = c("high", "medium", "low")), 
           labels = case_when(dependency == "high" ~ "(a) High dependency", 
                              dependency == "medium" ~ "(b) Medium dependency",
                              dependency == "low" ~ "(c) Low dependency"),
           habitat = case_when(grepl("Albizia", habitat) ~ "albizia", 
                               grepl("Eucalyptus", habitat) ~ "eucalyptus", 
                               TRUE ~ tolower(habitat)),
           habitat = factor(habitat, 
                            levels = c("primary", "restored", 
                                       "once_logged", "twice_logged", 
                                       "albizia", "eucalyptus"))) %>%
    ggplot(aes(habitat, estimate, ymin=Q5, ymax=Q95)) +
    geom_point() +
    geom_linerange() + 
    geom_line(aes(y = fits_sp$estimate, group=species), lty="longdash", alpha=.2) +
    facet_wrap(~labels, ncol=1) +
    theme_bw() + 
    theme(strip.background = element_blank(), 
          strip.text = element_text(hjust=0, face="bold"), 
          panel.grid = element_blank(), 
          axis.text = element_text(colour = "black"),
          axis.text.x = element_text(angle=45, hjust=1.05, colour="black")) +
    labs(y = "Occupancy", x="")

fig_scale <- 1.5
ggsave("figures/occupancy_estimates.png", units="mm", 
       height=120*fig_scale, width=115*fig_scale)

# 
df_save <- bind_cols(pred_data[,c("habitat", "species", "dependency")], fits_sp) %>%
    as_tibble

saveRDS(df_save, "outputs/df_save.rds")
