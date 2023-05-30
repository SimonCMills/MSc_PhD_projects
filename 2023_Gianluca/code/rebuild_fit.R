# rebuild fit object after two chains stopped writing to csv (due to running 
# out of memory)


fit <- flock(f_occ = ~ 0 + # don't fit intercept (as -1:1 coded)
                 #primary + 
                 #twice_logged + # limited variation in time since second logging
                 #once_logged + once_logged:time_since_logging_sc +
                 #logged_restored + logged_restored:time_since_logging_sc +
                 #eucalyptus + eucalyptus:plantation_age_sc + 
                 #albizia + albizia:plantation_age_sc +
                 # year plus year x species intercepts
                 (1|year) + (1|year_sp) + 
                 # site plus site x species intercepts
                 (1|site) + (1|site_sp) + 
                 # species random intercepts for each habitat type
                 (0 + primary + twice_logged + once_logged + logged_restored + 
                      eucalyptus + albizia|g1|species) + 
                 # bird-life forest dependency 
                 #forestdep_high + 
                 #forestdep_med +
                 #forestdep_low +
                 # dependency:habitat interactions
                 forestdep_high:primary + 
                 forestdep_high:twice_logged + forestdep_high:twice_logged:time_since_logging_sc +
                 forestdep_high:once_logged + forestdep_high:once_logged:time_since_logging_sc +
                 forestdep_high:logged_restored + 
                 forestdep_high:eucalyptus + forestdep_high:eucalyptus:plantation_age_sc +  
                 forestdep_high:albizia + forestdep_high:albizia:plantation_age_sc + 
                 #
                 forestdep_med:primary + 
                 forestdep_med:twice_logged + forestdep_med:twice_logged:time_since_logging_sc +
                 forestdep_med:once_logged + forestdep_med:once_logged:time_since_logging_sc +
                 forestdep_med:logged_restored + 
                 forestdep_med:eucalyptus + forestdep_med:eucalyptus:plantation_age_sc +  
                 forestdep_med:albizia + forestdep_med:albizia:plantation_age_sc + 
                 #
                 forestdep_low:primary + 
                 forestdep_low:twice_logged + forestdep_low:twice_logged:time_since_logging_sc +
                 forestdep_low:once_logged + forestdep_low:once_logged:time_since_logging_sc +
                 forestdep_low:logged_restored + 
                 forestdep_low:eucalyptus + forestdep_low:eucalyptus:plantation_age_sc +  
                 forestdep_low:albizia + forestdep_low:albizia:plantation_age_sc,
             # additional components that might be worth including
             # occupancy in each habitat type can vary according to body mass
             #log_body_mass + log_body_mass:habitat_type +
             # dietary guild
             # random intercept can vary by family
             # (1 + twice_logged + once_logged + logged_restored + eucalyptus + 
             #      albizia|family)
             f_det = ~ 0 + 
                 primary + ABC50_sc + 
                 twice_logged + 
                 once_logged + 
                 logged_restored + 
                 eucalyptus + eucalyptus:plantation_age_sc + 
                 albizia + albizia:plantation_age_sc +
                 time_of_day + 
                 observer + 
                 (1|observer_sp) + 
                 (1 + time_of_day|g1|species),
             prior = prior_specification,
             flocker_data = fd, 
             sample_prior = "yes",
             save_warmup = TRUE,
             chains = 2, cores = 2,
             #output_dir = "C:/Users/smills2/Desktop/stan_out",
             output_basename = "Borneo_v2",
             backend = "cmdstanr", 
             empty = TRUE
)

fnames <- list.files("C:/Users/smills2/Desktop/stan_out", full.names = TRUE)
file.info(fnames)

fit_csv <- brms:::read_csv_as_stanfit(fnames[c(2,4)])

fit$fit <- fit_csv
fit <- rename_pars(fit)

saveRDS(fit, "outputs/Borneo_v2_2chain.rds")
file.copy(fnames, "stan_out", overwrite = TRUE)

# plot...
plot(hypothesis(fit, "occ_forestdep_high:primary  > 0"))
plot(hypothesis(fit, "occ_forestdep_high:logged_restored  > 0"))
plot(hypothesis(fit, "occ_forestdep_high:once_logged  > 0"))
plot(hypothesis(fit, "occ_forestdep_high:twice_logged  > 0"))
plot(hypothesis(fit, "occ_forestdep_high:albizia  = 0"))



