library(brms); library(flocker); library(dplyr)

fd <- readRDS("fd_15-05-23.rds")

prior_specification <- c(
    # intercept terms (i.e. average species )
    #set_prior("normal(-3,2)", dpar = "occ", coef = "albizia"),
    #set_prior("normal(-3,2)", dpar = "occ", coef = "eucalyptus"),
    #set_prior("normal(-3,2)", dpar = "occ", coef = "logged_restored"),
    #set_prior("normal(-3,2)", dpar = "occ", coef = "once_logged"),
    #set_prior("normal(-3,2)", dpar = "occ", coef = "twice_logged"),
    #set_prior("normal(-3,2)", dpar = "occ", coef = "primary"),
    # forest dependency interaction terms
    #set_prior("normal(0,2)", dpar = "occ", coef = "albizia:forestdep_high"),
    set_prior("normal(0,2)", dpar = "occ", coef = "albizia:forestdep_med"),
    set_prior("normal(0,2)", dpar = "occ", coef = "albizia:forestdep_low"),
    #set_prior("normal(0,2)", dpar = "occ", coef = "eucalyptus:forestdep_high"),
    set_prior("normal(0,2)", dpar = "occ", coef = "eucalyptus:forestdep_med"),
    set_prior("normal(0,2)", dpar = "occ", coef = "eucalyptus:forestdep_low"),
    #set_prior("normal(0,2)", dpar = "occ", coef = "logged_restored:forestdep_high"),
    set_prior("normal(0,2)", dpar = "occ", coef = "logged_restored:forestdep_med"),
    set_prior("normal(0,2)", dpar = "occ", coef = "logged_restored:forestdep_low"),
    #set_prior("normal(0,2)", dpar = "occ", coef = "once_logged:forestdep_high"),
    set_prior("normal(0,2)", dpar = "occ", coef = "once_logged:forestdep_med"),
    set_prior("normal(0,2)", dpar = "occ", coef = "once_logged:forestdep_low"),
    #set_prior("normal(0,2)", dpar = "occ", coef = "primary:forestdep_high"),
    set_prior("normal(0,2)", dpar = "occ", coef = "primary:forestdep_med"),
    set_prior("normal(0,2)", dpar = "occ", coef = "primary:forestdep_low"),
    #set_prior("normal(0,2)", dpar = "occ", coef = "twice_logged:forestdep_high"),
    set_prior("normal(0,2)", dpar = "occ", coef = "twice_logged:forestdep_med"),
    set_prior("normal(0,2)", dpar = "occ", coef = "twice_logged:forestdep_low"),
    # year
    set_prior("normal(0,1)", dpar = "occ", coef = "factoryear2008"),
    set_prior("normal(0,1)", dpar = "occ", coef = "factoryear2009"),
    set_prior("normal(0,1)", dpar = "occ", coef = "factoryear2011"),
    set_prior("normal(0,1)", dpar = "occ", coef = "factoryear2015"),
    set_prior("normal(0,1)", dpar = "occ", coef = "factoryear2017"),
    set_prior("normal(0,1)", dpar = "occ", coef = "factoryear2022"),
    # forest dep
    #set_prior("normal(0,.5)", dpar = "occ", coef = "forestdep_high"),
    #set_prior("normal(0,.5)", dpar = "occ", coef = "forestdep_low"),
    #set_prior("normal(0,.5)", dpar = "occ", coef = "forestdep_med"),
    # interactions
    set_prior("normal(0,2)", dpar = "occ", class = "b"), 
    set_prior("normal(0,3)", dpar = "occ", class = "sd"), 
    ## detection
    set_prior("normal(0,2)", class = "b", dpar = ""),
    set_prior("normal(0,3)", class = "sd", dpar = "")
)

fit <- flock(f_occ = ~ 0 + # don't fit intercept (as -1:1 coded)
                 #primary + 
                 #twice_logged + # limited variation in time since second logging
                 #once_logged + once_logged:time_since_logging_sc +
                 #logged_restored + logged_restored:time_since_logging_sc +
                 #eucalyptus + eucalyptus:plantation_age_sc + 
                 #albizia + albizia:plantation_age_sc +
                 # year plus year x species intercepts
                 factor(year) + (1|year_sp) + 
                 # site plus site x species intercepts
                 (1|site) + (1|site_sp) + 
                 # species random intercepts for each habitat type
                 (1 + primary + twice_logged + once_logged + logged_restored + 
                      eucalyptus + albizia|species) + 
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
                 primary + primary:ABC50_sc + 
                 twice_logged + twice_logged:ABC50_sc + 
                 once_logged + once_logged:ABC50_sc +
                 logged_restored + logged_restored:ABC50_sc +
                 eucalyptus + eucalyptus:plantation_age_sc + 
                 albizia + albizia:plantation_age_sc +
                 time_of_day + 
                 observer + 
                 (1|observer_sp) + 
                 (1 + time_of_day|species),
             prior = prior_specification,
             flocker_data = fd, 
             chains = 4, cores = 4,
             output_dir = "stan_out",
             output_basename = "Borneo_v1",
             backend = "cmdstanr"
)

saveRDS(fit, "outputs/fit_v1.rds")
## v2
# prior_specification <- c(
#     # intercept terms (i.e. average species )
#     set_prior("normal(-3,2)", dpar = "occ", coef = "albizia"),
#     set_prior("normal(-3,2)", dpar = "occ", coef = "eucalyptus"),
#     set_prior("normal(-3,2)", dpar = "occ", coef = "logged_restored"),
#     set_prior("normal(-3,2)", dpar = "occ", coef = "once_logged"),
#     # forest dependency interaction terms
# 
#     set_prior("normal(0,3)", dpar = "occ", coef = "albizia:forestdep_med"),
#     set_prior("normal(0,3)", dpar = "occ", coef = "albizia:forestdep_low"),
# 
#     set_prior("normal(0,3)", dpar = "occ", coef = "eucalyptus:forestdep_med"),
#     set_prior("normal(0,3)", dpar = "occ", coef = "eucalyptus:forestdep_low"),
# 
#     set_prior("normal(0,3)", dpar = "occ", coef = "logged_restored:forestdep_med"),
#     set_prior("normal(0,3)", dpar = "occ", coef = "logged_restored:forestdep_low"),
# 
#     set_prior("normal(0,3)", dpar = "occ", coef = "once_logged:forestdep_med"),
#     set_prior("normal(0,3)", dpar = "occ", coef = "once_logged:forestdep_low"),
# 
#     set_prior("normal(0,3)", dpar = "occ", coef = "twice_logged:forestdep_med"),
#     set_prior("normal(0,3)", dpar = "occ", coef = "twice_logged:forestdep_low"),
#     # year
#     #set_prior("normal(0,1)", dpar = "occ", coef = "factoryear2008"),
#     set_prior("normal(0,1)", dpar = "occ", coef = "factoryear2009"),
#     set_prior("normal(0,1)", dpar = "occ", coef = "factoryear2011"),
#     set_prior("normal(0,1)", dpar = "occ", coef = "factoryear2015"),
#     set_prior("normal(0,1)", dpar = "occ", coef = "factoryear2017"),
#     set_prior("normal(0,1)", dpar = "occ", coef = "factoryear2022"),
#     # forest dep
#     set_prior("normal(0,.5)", dpar = "occ", coef = "forestdep_low"),
#     set_prior("normal(0,.5)", dpar = "occ", coef = "forestdep_med"),
#     # interactions
#     set_prior("normal(0,2)", dpar = "occ", class = "b"), 
#     set_prior("normal(0,3)", dpar = "occ", class = "sd"), 
#     ## detection
#     set_prior("normal(0,2)", class = "b", dpar = ""),
#     set_prior("normal(0,3)", class = "sd", dpar = "")
# )
# 
# fit <- flock(f_occ = ~ 1 + # primary & high fdep is intercept
#                  twice_logged + # limited variation in time since second logging
#                  once_logged + once_logged:time_since_logging_sc +
#                  logged_restored + logged_restored:time_since_logging_sc +
#                  eucalyptus + eucalyptus:plantation_age_sc + 
#                  albizia + albizia:plantation_age_sc +
#                  # year plus year x species intercepts
#                  factor(year) + (1|year_sp) + 
#                  # site plus site x species intercepts
#                  (1|site) + (1|site_sp) + 
#                  # species random intercepts for each habitat type
#                  (1 + twice_logged + once_logged + logged_restored + 
#                       eucalyptus + albizia|species) + 
#                  # bird-life forest dependency 
#                  forestdep_med +
#                  forestdep_low +
#                  # dependency:habitat interactions
#                  forestdep_med:twice_logged + #forestdep_med:twice_logged:time_since_logging_sc +
#                  forestdep_med:once_logged + #forestdep_med:once_logged:time_since_logging_sc +
#                  forestdep_med:logged_restored + 
#                  forestdep_med:eucalyptus + forestdep_med:eucalyptus:plantation_age_sc +  
#                  forestdep_med:albizia + forestdep_med:albizia:plantation_age_sc + 
#                  #
#                  forestdep_low:twice_logged + #forestdep_low:twice_logged:time_since_logging_sc +
#                  forestdep_low:once_logged + #forestdep_low:once_logged:time_since_logging_sc +
#                  forestdep_low:logged_restored + 
#                  forestdep_low:eucalyptus + forestdep_low:eucalyptus:plantation_age_sc +  
#                  forestdep_low:albizia + forestdep_low:albizia:plantation_age_sc,
#              # additional components that might be worth including
#              # occupancy in each habitat type can vary according to body mass
#              #log_body_mass + log_body_mass:habitat_type +
#              # dietary guild
#              # random intercept can vary by family
#              # (1 + twice_logged + once_logged + logged_restored + eucalyptus + 
#              #      albizia|family)
#              f_det = ~ 1 + 
#                  ABC50_sc + 
#                  twice_logged + twice_logged:ABC50_sc + 
#                  once_logged + once_logged:ABC50_sc +
#                  logged_restored + logged_restored:ABC50_sc +
#                  eucalyptus + eucalyptus:plantation_age_sc + 
#                  albizia + albizia:plantation_age_sc +
#                  time_of_day + 
#                  observer + 
#                  (1|observer_sp) + 
#                  (1 + time_of_day|species),
#              #prior = prior_specification,
#              flocker_data = fd, 
#              chains = 4, cores = 4,
#              output_dir = "stan_out",
#              output_basename = "Borneo_v2",
#              backend = "cmdstanr"
# )
# 
# df <- data.frame(y = rnorm(10), f = sample(LETTERS[1:3], 10, T))
# model.matrix(y ~ f, df)
# 
# 
# 
# prior_specification <- c(
#     # intercept terms (i.e. average species )
#     set_prior("normal(-3,2)", dpar = "occ", coef = "albizia"),
#     set_prior("normal(-3,2)", dpar = "occ", coef = "eucalyptus"),
#     set_prior("normal(-3,2)", dpar = "occ", coef = "logged_restored"),
#     set_prior("normal(-3,2)", dpar = "occ", coef = "once_logged"),
#     # forest dependency interaction terms
#     
#     set_prior("normal(0,3)", dpar = "occ", coef = "albizia:forestdep_med"),
#     set_prior("normal(0,3)", dpar = "occ", coef = "albizia:forestdep_low"),
#     
#     set_prior("normal(0,3)", dpar = "occ", coef = "eucalyptus:forestdep_med"),
#     set_prior("normal(0,3)", dpar = "occ", coef = "eucalyptus:forestdep_low"),
#     
#     set_prior("normal(0,3)", dpar = "occ", coef = "logged_restored:forestdep_med"),
#     set_prior("normal(0,3)", dpar = "occ", coef = "logged_restored:forestdep_low"),
#     
#     set_prior("normal(0,3)", dpar = "occ", coef = "once_logged:forestdep_med"),
#     set_prior("normal(0,3)", dpar = "occ", coef = "once_logged:forestdep_low"),
#     
#     set_prior("normal(0,3)", dpar = "occ", coef = "twice_logged:forestdep_med"),
#     set_prior("normal(0,3)", dpar = "occ", coef = "twice_logged:forestdep_low"),
#     # year
#     #set_prior("normal(0,1)", dpar = "occ", coef = "factoryear2008"),
#     #set_prior("normal(0,1)", dpar = "occ", coef = "factoryear2009"),
#     #set_prior("normal(0,1)", dpar = "occ", coef = "factoryear2011"),
#     #set_prior("normal(0,1)", dpar = "occ", coef = "factoryear2015"),
#     #set_prior("normal(0,1)", dpar = "occ", coef = "factoryear2017"),
#     #set_prior("normal(0,1)", dpar = "occ", coef = "factoryear2022"),
#     # forest dep
#     set_prior("normal(0,.5)", dpar = "occ", coef = "forestdep_low"),
#     set_prior("normal(0,.5)", dpar = "occ", coef = "forestdep_med"),
#     # interactions
#     set_prior("normal(0,2)", dpar = "occ", class = "b"), 
#     set_prior("normal(0,3)", dpar = "occ", class = "sd"), 
#     ## detection
#     set_prior("normal(0,2)", class = "b", dpar = ""),
#     set_prior("normal(0,3)", class = "sd", dpar = "")
# )
# fit <- flock(f_occ = ~ 1 + # primary & high fdep is intercept
#                  twice_logged + # limited variation in time since second logging
#                  once_logged + #once_logged:time_since_logging_sc +
#                  logged_restored + #logged_restored:time_since_logging_sc +
#                  eucalyptus + #eucalyptus:plantation_age_sc + 
#                  albizia + #albizia:plantation_age_sc +
#                  # year plus year x species intercepts
#                  # factor(year) + (1|year_sp) + 
#                  # site plus site x species intercepts
#                  (1|site) + (1|site_sp) + 
#                  # species random intercepts for each habitat type
#                  (1 + twice_logged + once_logged + logged_restored + 
#                       eucalyptus + albizia||species) + 
#                  # bird-life forest dependency 
#                  forestdep_med +
#                  forestdep_low +
#                  # dependency:habitat interactions
#                  forestdep_med:twice_logged + #forestdep_med:twice_logged:time_since_logging_sc +
#                  forestdep_med:once_logged + #forestdep_med:once_logged:time_since_logging_sc +
#                  forestdep_med:logged_restored + 
#                  forestdep_med:eucalyptus + #forestdep_med:eucalyptus:plantation_age_sc +  
#                  forestdep_med:albizia + #forestdep_med:albizia:plantation_age_sc + 
#                  #
#                  forestdep_low:twice_logged + #forestdep_low:twice_logged:time_since_logging_sc +
#                  forestdep_low:once_logged + #forestdep_low:once_logged:time_since_logging_sc +
#                  forestdep_low:logged_restored + 
#                  forestdep_low:eucalyptus + #forestdep_low:eucalyptus:plantation_age_sc +  
#                  forestdep_low:albizia, #+ forestdep_low:albizia:plantation_age_sc,
#              # additional components that might be worth including
#              # occupancy in each habitat type can vary according to body mass
#              #log_body_mass + log_body_mass:habitat_type +
#              # dietary guild
#              # random intercept can vary by family
#              # (1 + twice_logged + once_logged + logged_restored + eucalyptus + 
#              #      albizia|family)
#              f_det = ~ 1 + 
#                  #ABC50_sc + 
#                  #twice_logged + #twice_logged:ABC50_sc + 
#                  #once_logged + #once_logged:ABC50_sc +
#                  #logged_restored + #logged_restored:ABC50_sc +
#                  #eucalyptus + #eucalyptus:plantation_age_sc + 
#                  #albizia + #albizia:plantation_age_sc +
#                  time_of_day + 
#                  observer + 
#                  (1|observer_sp) + 
#                  (1 + time_of_day|species),
#              prior = prior_specification,
#              flocker_data = fd, 
#              chains = 4, cores = 4,
#              output_dir = "stan_out",
#              output_basename = "Borneo_v2",
#              backend = "cmdstanr", 
#              save_warmup = T, 
#              refresh = 1
# )
# 
# 
# ####
# prior_specification <- c(
#     # intercept terms (i.e. average species )
#     set_prior("normal(-3,2)", dpar = "occ", class = "Intercept"),
#     set_prior("normal(-1,3)", dpar = "", class = "Intercept"),
#     #set_prior("normal(0,3)", dpar = "occ", coef = "habitat"),
#     # forest dependency interaction terms
#     set_prior("normal(0,3)", dpar = "occ", class = "b")
# )
# 
# fit <- flock(f_occ = ~ 1 + # primary & high fdep is intercept
#                  habitat +
#                  # year plus year x species intercepts
#                  # factor(year) + (1|year_sp) + 
#                  # site plus site x species intercepts
#                  (1|site) + (1|site_sp) + 
#                  # species random intercepts for each habitat type
#                  (1 + habitat||species) + 
#                  # bird-life forest dependency 
#                  dependency +
#                  # dependency:habitat interactions
#                  dependency*habitat,
#              # additional components that might be worth including
#              # occupancy in each habitat type can vary according to body mass
#              #log_body_mass + log_body_mass:habitat_type +
#              # dietary guild
#              # random intercept can vary by family
#              # (1 + twice_logged + once_logged + logged_restored + eucalyptus + 
#              #      albizia|family)
#              f_det = ~ 1 + 
#                  #ABC50_sc + 
#                  #twice_logged + #twice_logged:ABC50_sc + 
#                  #once_logged + #once_logged:ABC50_sc +
#                  #logged_restored + #logged_restored:ABC50_sc +
#                  #eucalyptus + #eucalyptus:plantation_age_sc + 
#                  #albizia + #albizia:plantation_age_sc +
#                  time_of_day + 
#                  observer + 
#                  (1|observer_sp) + 
#                  (1 + time_of_day|species),
#              prior = prior_specification,
#              # empty = T,
#              flocker_data = fd, 
#              chains = 4, cores = 4,
#              output_dir = "stan_out",
#              output_basename = "Borneo_v3",
#              backend = "cmdstanr", 
#              save_warmup = T,
# )
# 
# saveRDS(fit, "outputs/initial_fit.rds")