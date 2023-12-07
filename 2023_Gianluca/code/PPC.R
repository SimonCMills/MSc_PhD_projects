# posterior predictive checks
# note: currently can't guarantee this runs 

library(flocker); library(brms); library(dplyr); library(ggplot2)

fit <- readRDS("outputs/fit_v3.rds")
fd <- readRDS("fd_22-05-23.rds")


## extract linear predictor for occupancy and detection components of the model
occ_linpred <- posterior_linpred(fit, dpar="occ", ndraws=50)
det_linpred <- posterior_linpred(fit, dpar="mu", ndraws=100)

# format the linear predictor for occupancy (only want the rows corresponding to 
# the detection units)
n_row <- fd$data$n_unit[1]
obs <- fd$data[1:n_row,]
occ_linpred2 <- t(boot::inv.logit(occ_linpred[,1:n_row]))
rm(occ_linpred)

# generated predicted occupancy per species x point combination
# format simulated occupancy
post_preds_occ <- obs %>%
    select(species, point_id) %>%
    bind_cols(., as_tibble(occ_linpred2)) %>%
    reshape2::melt(id.vars=c("species", "point_id"), 
                   variable.name = "draw", value.name = "occ") %>%
    as_tibble

# format predicted detection
post_preds_det <- fd$data %>%
    select(species, dependency, habitat, point_id) %>%
    group_by(species, point_id) %>%
    mutate(rep_id = 1:n()) %>%
    bind_cols(., as_tibble(t(boot::inv.logit(det_linpred)))) %>%
    reshape2::melt(id.vars=c("species", "point_id", "dependency", "habitat", "rep_id"),
                   variable.name = "draw", value.name = "p_det") %>%
    as_tibble

# combine to form posterior predictions ----
post_preds <- post_preds_det %>%
    group_by(species, point_id, draw) %>%
    summarise(p_not_det = prod(1-p_det), 
              habitat = habitat[1], dependency=dependency[1]) %>%
    left_join(post_preds_occ, ., 
                        by=c("species", "point_id", "draw"))

post_preds <- post_preds %>%
    mutate(det_sim = rbinom(n(), 1, occ * (1 - p_not_det)))
# summarise point-level predictions
# post_preds_pt <- post_preds %>%
#     group_by(species, point_id, draw) %>%
#     summarise(dependency = dependency[1],
#               habitat = habitat[1],
#               Q = sum(det_sim > 0), 
#               n_rep = n(),
#               det_hist = paste0(det_sim, collapse=""))

# switch to data.table as dplyr too slow
library(data.table)
post_preds <- as.data.table(post_preds)

setkey(post_preds, species, point_id, draw)
post_preds_pt <- post_preds[,list(dependency = dependency[1],
                                  habitat = habitat[1],
                                  Q = sum(det_sim) > 0,
                                  n_rep = .N),
                                  #det_hist = paste0(det_sim, collapse="")), 
                            by = .(species, point_id, draw)]


# plot: sumQ ----
post_preds_pt[,list(sumQ = sum(Q)), by=draw] %>%
    ggplot(aes(sumQ)) +
    geom_histogram(binwidth=50, boundary=0, col="black", fill="white") +
    geom_vline(xintercept = sum(obs$Q), lty = "longdash") +
    labs() +
    scale_y_continuous(breaks=seq(0, 30, 2)) +
    theme(plot.caption = element_text(hjust=0))

ggsave("figures/PPC_sumQ_total.png", units="mm", width = 140, height=150)


# ..and summarise these
post_summ_pt <- post_preds_pt %>%
    group_by(species, dependency, draw) %>%
    mutate(dependency = ifelse(dependency == "none", "low", dependency)) %>%
    summarise(sumQ = sum(Q))

# plot: sumQ per species ----
# obs <- fd$data[1:fd$data$n_unit[1],]
Q_summ_obs <- obs %>%
    group_by(species, dependency) %>%
    mutate(dependency = ifelse(dependency == "none", "low", dependency)) %>%
    summarise(sumQ = sum(Q)) 

t1 <- Q_summ_obs %>% 
    group_by(Q_cut = cut(sumQ, c(0, 1, seq(6, 100, 5), Inf), include.lowest=TRUE, right=F)) %>%
    summarise(n = n()) %>%
    ungroup %>%
    mutate(p = n/sum(n)) #%>%
    # ggplot(aes(sumQ, p)) + geom_line()

t2 <- post_summ_pt %>% 
    group_by(draw, 
             Q_cut = cut(sumQ, c(0, 1, seq(6, 100, 5), Inf), include.lowest=TRUE, right=F)) %>%
    summarise(n = n()) %>%
    group_by(draw) %>%
    mutate(p = n/sum(n))

t2_summ <- t2 %>%
    group_by(Q_cut) %>%
    summarise(mean = mean(p), 
              lwr1 = quantile(p, .1), 
              upr1 = quantile(p, .9), 
              lwr2 = quantile(p, .05), 
              upr2 = quantile(p, .95))

t1_t2 <- full_join(t2, t1, by="Q_cut", suffix = c("_pred", "_obs"))

plot_sumQ_diff <- left_join(t2_summ, t1) %>%
    ggplot(aes(Q_cut, mean - p, ymin=lwr2-p, ymax = upr2-p)) +
    geom_violin(data=t1_t2, aes(Q_cut, p_pred - p_obs), inherit.aes = FALSE,
                colour="grey70", fill="grey90", scale = "width") + 
    geom_point() +
    geom_linerange() +
    geom_hline(yintercept = 0, lty="longdash") +
    theme(axis.text.x = element_text(hjust=1, angle=45), 
          axis.text = element_text(colour="black")) +
    labs(y="Proportion(predicted - observed)")

plot_sumQ <- ggplot(t2, aes(Q_cut, p)) +
    geom_violin(colour="grey70", fill="grey90", scale = "width") + 
    geom_point(data=t2_summ, aes(y = mean), col="black") +
    geom_linerange(data = t2_summ, aes(ymin=lwr2, ymax=upr2, y=mean)) +
    geom_point(data = t1, col="red", alpha=.5) +
    theme(axis.text.x = element_text(hjust=1, angle=45), 
          axis.text = element_text(colour="black")) +
    labs(y="Proportion")


plot_both <- egg::ggarrange(plot_sumQ + theme(axis.text.x = element_blank(), 
                                 axis.title.x = element_blank()), 
               plot_sumQ_diff, ncol=1)

ggsave("figures/PPC_sumQ_total.png", plot_both, units="mm", width = 140, height=150)

## which species are overpredicted?
full_join(post_summ_pt, Q_summ_obs, by=c("species", "dependency"), 
          suffix = c("_pred", "_obs")) %>%
    group_by(draw, species, sumQ_obs) %>% 
    ggplot(aes(sumQ_pred, sumQ_obs)) +
    geom_point(alpha = .1) +
    coord_equal() +
    geom_abline(col="red")


full_join(post_summ_pt, Q_summ_obs, by=c("species", "dependency"), 
          suffix = c("_pred", "_obs")) %>%
    group_by(draw, species, sumQ_obs) %>% 
    ggplot(aes(sumQ_obs, log(sumQ_pred/sumQ_obs))) +
    geom_point(alpha = .1) +
    # coord_equal() +
    geom_abline(col="red")


ggplot(Q_summ_obs, aes(sumQ)) + 
    geom_line(stat="density", data=post_summ_pt, aes(group=draw), col="red", alpha=.1) +
    theme(plot.caption = element_text(hjust=0)) +
    geom_density() + 
    facet_wrap(~dependency, scales="free", ncol=1) +
    scale_x_continuous(breaks = 2^(0:10)) +
    # scale_x_sqrt(breaks=2^(1:10)) +
    theme(panel.grid.minor = element_blank())
ggsave("figures/PPC_sumQ_by_species_dep.png", units="mm", width = 140, height=150)

ggplot(Q_summ_obs, aes(sumQ)) + 
    geom_line(stat="density", data=post_summ_pt, aes(group=draw), col="red", alpha=.1) +
    theme(plot.caption = element_text(hjust=0)) +
    geom_density() + 
    #scale_x_sqrt(breaks=2^(1:10)) +
    theme(panel.grid.minor = element_blank())

ggsave("figures/PPC_sumQ_by_species.png", units="mm", width = 140, height=150)


# plot: sumQ by family ----
# sumQ_fam <- left_join(post_summ_pt, temp) %>%
#     group_by(family, draw) %>%
#     summarise(sumQ = sum(sumQ)) 
# 
# sumQ_fam_obs <- left_join(Q_summ_obs, temp) %>%
#     group_by(family) %>%
#     summarise(sumQ = sum(sumQ), n_species = length(unique(species)))  
# 
# fam_comps <- full_join(sumQ_fam, sumQ_fam_obs, by="family", suffix = c("_pred", "_obs")) %>%
#     mutate(family = paste0(family, " (", n_species, ")")) %>%
#     ungroup %>%
#     arrange(sumQ_obs) %>%
#     mutate(family= factor(family, levels=unique(family)))
# 
# mean_d <- fam_comps %>%
#     group_by(family) %>%
#     summarise(diff = sumQ_pred - sumQ_obs, 
#               meanD = mean(diff), 
#               lwr = quantile(diff, .1), 
#               upr = quantile(diff, .9))
#     
# ggplot(fam_comps, aes(sumQ_pred - sumQ_obs, family)) +
#     geom_jitter(width=0, height=.2, alpha=.1) +
#     geom_point(data=mean_d, aes(x = meanD), col="red") +
#     geom_linerange(data=mean_d, aes(xmin=lwr, xmax=upr, x=meanD), col="red") +
#     scale_x_continuous(breaks = seq(-100, 100, 20)) +
#     geom_vline(xintercept = 0, lty="longdash")
# 

# plot: sumQ by habitat ----
post_summ_pt <- post_preds_pt %>%
    group_by(species, dependency, habitat, draw) %>%
    summarise(sumQ = sum(Q))

Q_summ_obs <- obs %>%
    group_by(species, habitat) %>%
    summarise(sumQ = sum(Q)) 


full_join(post_summ_pt, Q_summ_obs, by=c("species", "habitat"), suffix = c("_pred", "_obs")) %>%
    group_by(draw, dependency, habitat, species) %>%
    summarise(sumQ_pred = sum(sumQ_pred), 
              sumQ_obs = sum(sumQ_obs)) %>%
    ggplot(aes(sumQ_obs, sumQ_pred, group=draw)) +
    geom_point(alpha=.05) +
    # geom_density(aes(sumQ_obs)) +
    facet_grid(dependency~habitat, scales="free") +
    geom_abline()

ggsave("figures/PPC_sumQ_by_habitat_dependency.png", units="mm")


# detection history frequency ----
y_vec <- fd$data$y
obs <- obs %>%
    mutate_at(vars(rep_index1:rep_index4), function(x) ifelse(x == -99, NA, x)) %>%
    mutate(d_index1 = y_vec[rep_index1], 
           d_index2 = y_vec[rep_index2], 
           d_index3 = y_vec[rep_index3], 
           d_index4 = y_vec[rep_index4]) 

obs_det <- obs %>%
    group_by(species, point_id) %>%
    mutate(det_hist = case_when(n_rep == 2 ~ paste0(d_index1, d_index2, collapse=""),
                                n_rep == 3 ~ paste0(d_index1, d_index2, d_index3, collapse=""), 
                                n_rep == 4 ~ paste0(d_index1, d_index2, d_index3, d_index4, collapse=""))) %>%
    group_by(det_hist, n_rep) %>%
    summarise(N = n())


## plot ----
post_preds_pt %>%
    group_by(draw, det_hist, n_rep) %>%
    summarise(N = n()) %>%
    # filter(!(det_hist %in% c("00", "000", "0000"))) %>%
    ggplot(aes(det_hist, N)) + 
    geom_boxplot() +
    facet_wrap((det_hist %in% c("00", "000", "0000"))~n_rep, scales="free") +
    geom_point(data=obs_det, col = "red") +
    theme(axis.text.x = element_text(angle=45, hjust=1))
