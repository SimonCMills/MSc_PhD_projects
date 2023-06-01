# posterior predictive checks
library(flocker); library(brms); library(dplyr)

fit <- readRDS("outputs/Borneo_v2_2chain.rds")
fd <- readRDS("fd_22-05-23.rds")

## extract linear predictor for occupancy and detection components of the model
occ_linpred <- posterior_linpred(fit, dpar="occ", ndraws=100)
det_linpred <- posterior_linpred(fit, dpar="mu", ndraws=100)

# format the linear predictor for occupancy (only want the rows corresponding to 
# the detection units)
n_row <- fd$data$n_unit[1]
occ_linpred2 <- t(boot::inv.logit(occ_linpred[,1:n_row]))

# generated predicted occupancy per species x point combination
occ_sim <- occ_linpred2
occ_sim[] <- rbinom(100 * n_row, 1, occ_linpred2)

# format simulated occupancy
post_preds_occ <- fd$data[1:n_row,] %>%
    select(species, point_id) %>%
    bind_cols(., as_tibble(occ_sim)) %>%
    reshape2::melt(id.vars=c("species", "point_id"), 
                   variable.name = "draw", value.name = "occ") %>%
    as_tibble

# format predicted detection
post_preds_det <- fd$data 
    select(species, dependency, habitat, point_id) %>%
    group_by(species, point_id) %>%
    mutate(rep_id = 1:n()) %>%
    bind_cols(., as_tibble(t(boot::inv.logit(det_linpred)))) %>%
    reshape2::melt(id.vars=c("species", "point_id", "dependency", "habitat", "rep_id"),
                   variable.name = "draw", value.name = "p_det") %>%
    as_tibble

# combine to form posterior predictions ----
post_preds <- left_join(post_preds_occ, post_preds_det, 
                        by=c("species", "point_id", "draw")) %>%
    mutate(det_sim = rbinom(n(), occ, p_det))

# summarise point-level predictions
post_preds_pt <- post_preds %>%
    group_by(species, point_id, draw) %>%
    summarise(dependency = dependency[1],
              Q = sum(det_sim > 0), 
              n_rep = n(),
              det_hist = paste0(det_sim, collapse=""))

# ..and summarise these
post_summ_pt <- post_preds_pt %>%
    group_by(species, draw, dependency, habitat) %>%
    summarise(sumQ = sum(Q))

# plot: sumQ per species ----
obs <- fd$data[1:fd$data$n_unit[1],]
Q_summ_obs <- obs %>%
    group_by(species, dependency) %>%
    summarise(sumQ = sum(Q)) 

ggplot(Q_summ_obs, aes(dependency, sumQ)) + 
    geom_density() +
    geom_line(stat="density", data=post_summ_pt, aes(group=draw), col="red", alpha=.1)

full_join(post_summ_pt, Q_summ_obs, by="species", suffix = c("_pred", "_obs")) %>%
    group_by(species) %>%
    mutate(diff = mean(sumQ_obs - sumQ_pred)) %>%
    ungroup(species) %>%
    arrange(diff) %>%
    mutate(species = factor(species, levels=unique(species))) %>%
    ggplot(aes(sumQ_obs - sumQ_pred, species, col=dependency)) + 
    geom_point(alpha=.1) + 
    geom_vline(xintercept = 0, lty="longdash") 

# detection history frequency ----
obs <- obs %>%
    mutate_at(vars(rep_index1:rep_index4), function(x) ifelse(x == -99, NA, x)) %>%
    mutate(d_index1 = y_vec[rep_index1], 
           d_index2 = y_vec[rep_index2], 
           d_index3 = y_vec[rep_index3], 
           d_index4 = y_vec[rep_index4]) 

obs_det <- obs %>%
    group_by(species, point_id) %>%
    summarise(det_hist = case_when(n_rep == 2 ~ paste0(d_index1, d_index2, collapse=""),
                               n_rep == 3 ~ paste0(d_index1, d_index2, d_index3, collapse=""), 
                               n_rep == 4 ~ paste0(d_index1, d_index2, d_index3, d_index4, collapse="")))


## plot ----
post_preds_pt %>%
    group_by(draw, det_hist, n_rep) %>%
    summarise(N = n()) %>%
    filter(!(det_hist %in% c("00", "000", "0000"))) %>%
    ggplot(aes(det_hist, N)) + 
    geom_boxplot() +
    facet_wrap(~n_rep) +
    geom_point(data=obs_det, col = "red") +
    theme(axis.text.x = element_text(angle=45, hjust=1))