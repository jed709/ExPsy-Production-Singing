# Load data and calculate effects ---------------------------------------------

# read in sheet

meta_dat = read.xlsx('data/meta/singing_meta_v5.xlsx', sheet=1) %>%
  filter(include==1) %>%
  mutate(ri = d_cor, 
         sing_sens = sing_dm,
         sing_sens_sd = sing_dsd,
         aloud_sens = aloud_dm,
         aloud_sens_sd = aloud_dsd,
         silent_sens = silent_dm,
         silent_sens_sd = silent_dsd)

# impute missing correlations

meta_dat[is.na(meta_dat$ri),]$ri = mean(meta_dat$ri, na.rm = T)

# calculate effects

meta_dat = escalc('MC', m1i=sing_sens, m2i=aloud_sens, sd1i=sing_sens_sd, sd2i=aloud_sens_sd, ni=n_par, ri=ri, data=meta_dat)

# Models of the SSE -----------------------------------------------------------

# add in IDs for effects

meta_dat$effid = 1:nrow(meta_dat)

# calculate sampling var

meta_dat$sei = meta_dat$vi**.5

# fit model

m1_brms = brm(yi|se(sei) ~ 1 + (1|effid), data=meta_dat, 
              chains = 4, iter=80000, cores=4,
              threads = threading(2),
              control=list(adapt_delta=.999),
              prior = c(prior(normal(0, .3), class='Intercept'),
                        prior(normal(0, .3), class='sd')))

# estimated SSE

m1_brms %>%
  as_draws_df() %>%
  median_hdi(b_Intercept)

# calculate prediction intervals for the above

posterior_summary(posterior_epred(m1_brms, 
                                  newdata=data.frame(sei=0, effid=9999), 
                                  re_formula=~(1|effid), allow_new_levels=TRUE))

# fit moderator analysis for color matching

m1.2_brms = brm(yi|se(sei) ~ sep_fa-1 + (1|effid), data=meta_dat,
               chains = 4, iter=80000, cores=4,
               control=list(adapt_delta=.999),
               prior = c(prior(normal(0, .3), class='b'),
                         prior(normal(0, .3), class='sd')))

# estimated SSE in each condition

m1.2_brms %>%
  as_draws_df() %>%
  median_hdi(b_sep_fay)

m1.2_brms %>%
  as_draws_df() %>%
  median_hdi(b_sep_fan)

# calculate prediction intervals for the above
# effid = 9999 used to predict effect for a new study

posterior_summary(posterior_epred(m1.2_brms, 
                                  newdata=data.frame(sei=0, effid=9999,
                                                     sep_fa = c('y', 'n')), 
                                  re_formula=~(1|effid), 
                                  allow_new_levels=TRUE))

# Forest plot for SSE model -----------------------------------------------

# extract by-study draws to get model estimate for each study

m1_r <- spread_draws(m1_brms, r_effid[effid,term], b_Intercept) %>% 
  mutate(b_Intercept = r_effid + b_Intercept) 

# extract intercept draws for aggregate estimate

m1_b <- spread_draws(m1_brms, b_Intercept) %>% 
  mutate(effid = "Average") 

# calculate PIs for plot

median_hdi(posterior_epred(m1_brms, 
                           newdata=data.frame(sei=0, effid = '999'), 
                           re_formula=~(1|effid), allow_new_levels=TRUE), .width = c(0.5, 0.95))

# wrangling for binding data

m1_r %>%
  mutate(effid = as.character(effid)) -> m1_r

# bind study and aggregate draws together

m1_all <- bind_rows(m1_r, m1_b) %>% 
  ungroup() %>%
  mutate(effid = fct_relevel(effid, "Average")) %>%
  mutate(effid = str_replace_all(effid, "\\.", " "))

# wrangling for the plot labels

m1_all$effid = factor(m1_all$effid)

m1_all$effid = recode_factor(m1_all$effid, Average = 'Average',
                             '14' = 'Current Study (E3 Unmatched)',
                             '13' = 'Current Study (E3 Matched)',
                             '12' = 'Current Study (E2 Unmatched)',
                             '11' = 'Current Study (E2 Matched)',
                             '10' = 'Current Study (E1b)',
                             '9' = 'Current Study (E1a)',
                             '8' = 'Zhang (2024)',
                             '7' = "Quinlan & Taylor (2019; E3)",
                             '6' = "Quinlan & Taylor (2019; E2)",
                             '5' = "Quinlan & Taylor (2019; E1)",
                             '4' = "Hassall et al (2016)",
                             '3' = "Quinlan & Taylor (2013; E3)",
                             '2' = "Quinlan & Taylor (2013; E2)",
                             '1' = 'Quinlan & Taylor (2013; Pilot)')

m1_all_sum <- group_by(m1_all, effid, term) %>% 
  median_hdi(b_Intercept)

meta_dat$effid = factor(c('Quinlan & Taylor (2013; Pilot)',
                          'Quinlan & Taylor (2013; E2)',
                          'Quinlan & Taylor (2013; E3)',
                          'Hassall et al (2016)',
                          'Quinlan & Taylor (2019; E1)',
                          'Quinlan & Taylor (2019; E2)',
                          'Quinlan & Taylor (2019; E3)',
                          'Zhang (2024)',
                          'Current Study (E1a)',
                          'Current Study (E1b)',
                          'Current Study (E2 Matched)',
                          'Current Study (E2 Unmatched)',
                          'Current Study (E3 Matched)',
                          'Current Study (E3 Unmatched)'),
                        levels = c('Quinlan & Taylor (2013; Pilot)',
                                   'Quinlan & Taylor (2013; E2)',
                                   'Quinlan & Taylor (2013; E3)',
                                   'Hassall et al (2016)',
                                   'Quinlan & Taylor (2019; E1)',
                                   'Quinlan & Taylor (2019; E2)',
                                   'Quinlan & Taylor (2019; E3)',
                                   'Zhang (2024)',
                                   'Current Study (E1a)',
                                   'Current Study (E1b)',
                                   'Current Study (E2 Matched)',
                                   'Current Study (E2 Unmatched)',
                                   'Current Study (E3 Matched)',
                                   'Current Study (E3 Unmatched)'
                        ))

# create plot

m1_all %>%   
  ggplot(aes(b_Intercept, effid)) +
  geom_vline(xintercept = 0, color = 'black', lty = 2) +
  stat_halfeye(.width = c(.5, .95), fill = "#b5b5b5", point_interval = 'median_hdi',
               slab_alpha = 0.5, scale = 0.8) +
  geom_text(
    data = m1_all_sum%>% mutate_if(is.numeric, round, digits=2) %>%
      mutate_if(is.numeric, formatC, format='f', digits=2),
    aes(label = str_glue("{b_Intercept} [{.lower}, {.upper}]"), x = 1),
    hjust = "inward"
  ) +
  geom_point(
    data = meta_dat %>% mutate(effid = str_replace_all(effid, "\\.", " ")), 
    aes(x=yi), shape = 4, size = 3
  ) + 
  labs(x = 'Raw Mean Difference', y = NULL) +
  annotate("segment", x=-0.15402302, xend = 0.4608277, y=0.7, yend = 0.7, linetype = 3)+
  theme_classic()

# Models of the production effect -----------------------------------------

# prepare data frame

meta_dat_2 = read.xlsx('data/meta/pe_meta_v1.xlsx', sheet = 1)

# impute missing correlations

meta_dat_2[is.na(meta_dat_2$pe_cor) & meta_dat_2$condition=='aloud',]$pe_cor = 
  mean(meta_dat_2[meta_dat_2$condition == 'aloud',]$pe_cor, na.rm = T)

meta_dat_2[is.na(meta_dat_2$pe_cor) & meta_dat_2$condition=='sing',]$pe_cor = 
  mean(meta_dat_2[meta_dat_2$condition == 'sing',]$pe_cor, na.rm = T)

# calculate PEs

meta_dat_2 = escalc(measure = 'MC', m1i=prod_mean, m2i=silent_sens, 
       sd1i=prod_sd, sd2i=silent_sens_sd, 
       ni=n_par, ri=pe_cor, data = meta_dat_2)

# se

meta_dat_2$sei = meta_dat_2$vi**.5

# model the PE by condition (sing or aloud)

m2_brms = brm(yi|se(sei) ~ condition-1 + (condition-1|plot_label), data=meta_dat_2, 
                        chains = 4, iter=80000, cores=4, threads = threading(2),
                        control=list(adapt_delta=.99),
                        prior = c(prior(normal(0, .5), class='b'),
                                  prior(normal(0, .5), class='sd'),
                                  prior(lkj(3), class = 'cor'))
)

# estimated PE in each condition

m2_brms %>%
  as_draws_df() %>%
  median_hdi(b_conditionaloud)

m2_brms %>%
  as_draws_df() %>%
  median_hdi(b_conditionsing)

# PIs for each condition

posterior_summary(posterior_epred(m2_brms, 
                newdata=data.frame(sei=0, 
                                   plot_label = 'new', 
                                   condition = c('aloud', 'sing')), 
                re_formula=~(condition-1|plot_label), allow_new_levels=TRUE))

# difference between conditions

m2_brms %>%
  as_draws_df() %>%
  median_hdi(b_conditionsing - b_conditionaloud)

# moderator analysis for color matching

m2.1_brms = brm(yi|se(sei) ~ condition:sep_fa-1 + (condition-1|plot_label), data=meta_dat_2, 
              chains = 4, iter=80000, cores=4,
              threads = threading(2),
              control=list(adapt_delta=.999),
              prior = c(prior(normal(0, .5), class='b'),
                        prior(normal(0, .5), class='sd'),
                        prior(lkj(3), class = 'cor')))

# estimated differences across color matching

m2.1_brms %>%
  as_draws_df() %>%
  median_hdi(`b_conditionsing:sep_fan` - `b_conditionaloud:sep_fan`)

m2.1_brms %>%
  as_draws_df() %>%
  median_hdi(`b_conditionsing:sep_fay` - `b_conditionaloud:sep_fay`)

m2.1_brms %>%
  as_draws_df() %>%
  median_hdi(`b_conditionaloud:sep_fan` - `b_conditionaloud:sep_fay`)

m2.1_brms %>%
  as_draws_df() %>%
  median_hdi(`b_conditionsing:sep_fan` - `b_conditionsing:sep_fay`)

# Forest plot for PE model ------------------------------------------------

# first, refit model with numeric sample id variable for use with tidy functions;
# not sure why this doesn't work otherwise. model is identical, though.

meta_dat_2$samp_id = c(rep(1:14, each = 2))

m2.2_brms = brm(yi|se(sei) ~ condition-1 + (condition-1|samp_id), data=meta_dat_2, 
                chains = 4, iter=80000, cores=4,
                threads = threading(2),
                control=list(adapt_delta=.99),
                prior = c(prior(normal(0, .5), class='b'),
                          prior(normal(0, .5), class='sd'),
                          prior(lkj(3), class = 'cor')))

# extract by-study draws to calculate estimate for each study

m2_r_sing <- spread_draws(m2.2_brms, r_samp_id[samp_id,term], b_conditionsing) %>%
  filter(term == 'conditionsing') %>%
  mutate(vals = r_samp_id + b_conditionsing) %>%
  select(-b_conditionsing)

m2_r_aloud <- spread_draws(m2.2_brms, r_samp_id[samp_id,term], b_conditionaloud) %>%
  filter(term == 'conditionaloud') %>%
  mutate(vals = r_samp_id + b_conditionaloud) %>%
  select(-b_conditionaloud)

# extract aggregate draws

m2_b_sing <- spread_draws(m2.2_brms, b_conditionsing) %>%
  mutate(samp_id = "Average",
         vals = b_conditionsing,
         term = 'conditionsing') %>%
  select(-b_conditionsing)

m2_b_aloud <- spread_draws(m2.2_brms, b_conditionaloud) %>%
  mutate(samp_id = "Average",
         vals = b_conditionaloud,
         term = 'conditionaloud') %>%
  select(-b_conditionaloud)

# wrangling for binding data

m2_r_sing %>%
  mutate(samp_id = as.character(samp_id)) -> m2_r_sing

m2_r_aloud %>%
  mutate(samp_id = as.character(samp_id)) -> m2_r_aloud

# bind dataframes together for sing and aloud

m2_all_sing <- bind_rows(m2_r_sing, m2_b_sing) %>%
  ungroup() %>%
  mutate(samp_id = fct_relevel(samp_id, "Average")) %>% 
  mutate(samp_id = str_replace_all(samp_id, "\\.", " "))

m2_all_aloud <- bind_rows(m2_r_aloud, m2_b_aloud) %>%
  ungroup() %>%
  mutate(samp_id = fct_relevel(samp_id, "Average")) %>% 
  mutate(samp_id = str_replace_all(samp_id, "\\.", " "))

# bind aloud and sing together

m2_all <- bind_rows(m2_all_sing, m2_all_aloud)

# wrangling for the plot labels

m2_all$samp_id = factor(m2_all$samp_id)

m2_all$samp_id = recode_factor(m2_all$samp_id, Average = 'Average',
                                '14' = 'Current Study (E3 Unmatched)',
                                '13' = 'Current Study (E3 Matched)',
                                '12' = 'Current Study (E2 Unmatched)',
                                '11' = 'Current Study (E2 Matched)',
                                '10' = 'Current Study (E1b)',
                                '9' = 'Current Study (E1a)',
                                '8' = 'Zhang (2024)',
                                '7' = "Quinlan & Taylor (2019; E3)",
                                '6' = "Quinlan & Taylor (2019; E2)",
                                '5' = "Quinlan & Taylor (2019; E1)",
                                '4' = "Hassall et al. (2016)",
                                '3' = "Quinlan & Taylor (2013; E3)",
                                '2' = "Quinlan & Taylor (2013; E2)",
                                '1' = 'Quinlan & Taylor (2013; Pilot)')

m2_all_sum <- group_by(m2_all, samp_id, term) %>% 
  median_hdi(vals)

meta_dat_2$plot_label = factor(rep(c('Quinlan & Taylor (2013; Pilot)',
                          'Quinlan & Taylor (2013; E2)',
                          'Quinlan & Taylor (2013; E3)',
                          'Hassall et al. (2016)',
                          'Quinlan & Taylor (2019; E1)',
                          'Quinlan & Taylor (2019; E2)',
                          'Quinlan & Taylor (2019; E3)',
                          'Zhang (2024)',
                          'Current Study (E1a)',
                          'Current Study (E1b)',
                          'Current Study (E2 Matched)',
                          'Current Study (E2 Unmatched)',
                          'Current Study (E3 Matched)',
                          'Current Study (E3 Unmatched)'), each = 2),
                          levels = c('Quinlan & Taylor (2013; Pilot)',
           'Quinlan & Taylor (2013; E2)',
           'Quinlan & Taylor (2013; E3)',
           'Hassall et al. (2016)',
           'Quinlan & Taylor (2019; E1)',
           'Quinlan & Taylor (2019; E2)',
           'Quinlan & Taylor (2019; E3)',
           'Zhang (2024)',
           'Current Study (E1a)',
           'Current Study (E1b)',
           'Current Study (E2 Matched)',
           'Current Study (E2 Unmatched)',
           'Current Study (E3 Matched)',
           'Current Study (E3 Unmatched)'
))

# create plot
                    
m2_all %>%   
  ggplot(aes(x = vals, y = samp_id, fill = term)) +
  geom_vline(xintercept = 0, color = 'black', lty = 2) +
  stat_halfeye(aes(shape = term), .width = c(.5, .95), point_interval = 'median_hdi', slab_linewidth = 0.5, 
               slab_alpha = 0.7, scale = 0.8,
               position = position_dodge(width = 0.9)) + 
  scale_shape_manual(values=c(15, 16)) +
  scale_fill_manual(values = c('#b5b5b5', '#333333'), name = c("Contrast"), 
                    labels = c("Aloud-Silent", "Sing-Silent")) +
  geom_text(
  data = m2_all_sum%>% filter(term == 'conditionsing') %>% mutate_if(is.numeric, round, digits=2) %>%
    mutate_if(is.numeric, formatC, format='f', digits=2),
  aes(label = str_glue("{vals} [{.lower}, {.upper}]"), x = 1.5),
  hjust = "inward", vjust = 0.1, position = position_nudge(y=0.25), size = 3) +
  geom_text(
    data = m2_all_sum%>% filter(term == 'conditionaloud') %>% mutate_if(is.numeric, round, digits=2) %>%
      mutate_if(is.numeric, formatC, format='f', digits=2),
    aes(label = str_glue("{vals} [{.lower}, {.upper}]"), x = 1.5),
    hjust = "inward", vjust = 0.1, position = position_nudge(y=-0.25), size = 3) +
  geom_point(
    data = meta_dat_2 %>% mutate(term = rep(c('conditionaloud', 'conditionsing'), 14),
      samp_id = plot_label), 
    aes(x=yi), shape = 4, size = 3, position = position_dodge(width = 0.9), show.legend = F) +
  labs(x = 'Raw Mean Difference', y = NULL) +
  theme_classic() +
  guides(shape = 'none') +
  theme(legend.position = 'bottom')

# Analyses of Publication Bias --------------------------------------------

# scale sample size and SE

meta_dat = meta_dat %>%
  mutate(se_std = (sei - mean(sei))/sd(sei),
         size_std = (n_par - mean(n_par))/sd(n_par))

# fit models analgous to egger's regression test

regtest_se = brm(yi|se(vi**.5) ~ se_std + (1|plot_label),
              data=meta_dat, cores=4, chains = 4, 
              threads = threading(2), 
              control = list(adapt_delta=.9),
              prior = c(prior(normal(0, .3), class='Intercept'),
                        prior(normal(0, 1), class = 'b'),
                        prior(normal(0, .3), class='sd')),
              iter = 80000)

regtest_n = brm(yi|se(vi**.5) ~ size_std + (1|plot_label),
                 data=meta_dat, cores=4, chains = 4, 
                 threads = threading(2), 
                 control = list(adapt_delta=.9),
                 prior = c(prior(normal(0, .3), class='Intercept'),
                           prior(normal(0, 1), class = 'b'),
                           prior(normal(0, .3), class='sd')),
                 iter = 80000)

# extract estimates for the above

regtest_se %>%
  as_draws_df() %>%
  median_hdi(b_Intercept)

regtest_se %>%
  as_draws_df() %>%
  median_hdi(b_se_std)

regtest_n %>%
  as_draws_df() %>%
  median_hdi(b_Intercept)

regtest_n %>%
  as_draws_df() %>%
  median_hdi(b_size_std)

# cumulative meta-analysis of the SSE

# set up study order from smallest to largest samples

meta_dat$order = c(13,11,10,8,12,7,6,14,9,5,4,3,2,1)

new = meta_dat

# create list to hold models

metalist <- vector(mode="list")

# fit model iteratively, adding one study at a time

for (i in 1:nrow(new)) {
  nd = new[new$order <= i,]
  
  temp = brm(yi|se(sei) ~ 1 + (1|plot_label), data=nd, 
             chains = 4, iter=80000, cores=4,
             threads = threading(2),
             control=list(adapt_delta=.999),
             prior = c(prior(normal(0, .3), class='Intercept'),
                       prior(normal(0, .3), class='sd')))
  
  metalist[[i]] <- temp
}

# Forest plot for cumulative meta -----------------------------------------

# wrangling for plot labels

metalist[[14]] %>% 
  as_draws_df() %>% 
  mutate('+ Zhang (2024)' = b_Intercept) %>% 
  select('+ Zhang (2024)') %>%
  pivot_longer(everything(), names_to = 'plot_label', values_to = 'intercept') -> d.z

metalist[[13]] %>% 
  as_draws_df() %>% 
  mutate('+ Quinlan & Taylor (2013; Pilot)' = b_Intercept) %>% 
  select('+ Quinlan & Taylor (2013; Pilot)') %>%
  pivot_longer(everything(), names_to = 'plot_label', values_to = 'intercept') -> d.a

metalist[[12]] %>% 
  as_draws_df() %>% 
  mutate('+ Quinlan & Taylor (2019; E1)' = b_Intercept) %>% 
  select('+ Quinlan & Taylor (2019; E1)') %>%
  pivot_longer(everything(), names_to = 'plot_label', values_to = 'intercept') -> d.b

metalist[[11]] %>% 
  as_draws_df() %>% 
  mutate('+ Quinlan & Taylor (2013; E2)' = b_Intercept) %>% 
  select('+ Quinlan & Taylor (2013; E2)') %>%
  pivot_longer(everything(), names_to = 'plot_label', values_to = 'intercept') -> d.c

metalist[[10]] %>% 
  as_draws_df() %>% 
  mutate('+ Quinlan & Taylor (2013; E3)' = b_Intercept) %>% 
  select('+ Quinlan & Taylor (2013; E3)') %>%
  pivot_longer(everything(), names_to = 'plot_label', values_to = 'intercept') -> d.d

metalist[[9]] %>% 
  as_draws_df() %>% 
  mutate('+ Current study (E1a)' = b_Intercept) %>% 
  select('+ Current study (E1a)') %>%
  pivot_longer(everything(), names_to = 'plot_label', values_to = 'intercept') -> d.e

metalist[[8]] %>% 
  as_draws_df() %>% 
  mutate('+ Hassall et al. (2016)' = b_Intercept) %>% 
  select('+ Hassall et al. (2016)') %>%
  pivot_longer(everything(), names_to = 'plot_label', values_to = 'intercept') -> d.f

metalist[[7]] %>% 
  as_draws_df() %>% 
  mutate('+ Quinlan & Taylor (2019; E2)' = b_Intercept) %>% 
  select('+ Quinlan & Taylor (2019; E2)') %>%
  pivot_longer(everything(), names_to = 'plot_label', values_to = 'intercept') -> d.g

metalist[[6]] %>% 
  as_draws_df() %>% 
  mutate('+ Quinlan & Taylor (2019; E3)' = b_Intercept) %>% 
  select('+ Quinlan & Taylor (2019; E3)') %>%
  pivot_longer(everything(), names_to = 'plot_label', values_to = 'intercept') -> d.h

metalist[[5]] %>% 
  as_draws_df() %>% 
  mutate('+ Current study (E1b)' = b_Intercept) %>% 
  select('+ Current study (E1b)') %>%
  pivot_longer(everything(), names_to = 'plot_label', values_to = 'intercept') -> d.i

metalist[[4]] %>% 
  as_draws_df() %>% 
  mutate('+ Current study (E2 Unmatched)' = b_Intercept) %>% 
  select('+ Current study (E2 Unmatched)') %>%
  pivot_longer(everything(), names_to = 'plot_label', values_to = 'intercept') -> d.j

metalist[[3]] %>% 
  as_draws_df() %>% 
  mutate('+ Current study (E2 Matched)' = b_Intercept) %>% 
  select('+ Current study (E2 Matched)') %>%
  pivot_longer(everything(), names_to = 'plot_label', values_to = 'intercept') -> d.k

metalist[[2]] %>% 
  as_draws_df() %>% 
  mutate('+ Current Study (E3 Matched)' = b_Intercept) %>% 
  select('+ Current Study (E3 Matched)') %>%
  pivot_longer(everything(), names_to = 'plot_label', values_to = 'intercept') -> d.l

metalist[[1]] %>% 
  as_draws_df() %>% 
  mutate('Current Study (E3 Unmatched)' = b_Intercept) %>% 
  select('Current Study (E3 Unmatched)') %>%
  pivot_longer(everything(), names_to = 'plot_label', values_to = 'intercept') -> d.m



v <- rbind(d.z,d.a,d.b,d.c,d.d,d.e,d.f,d.g,d.h,d.i,d.j,d.k,d.l,d.m)

v$plot_label= factor(v$plot_label, 
         levels = c("+ Zhang (2024)",
                    "+ Quinlan & Taylor (2013; Pilot)",
                    "+ Quinlan & Taylor (2019; E1)",
                    "+ Quinlan & Taylor (2013; E2)",
                    "+ Quinlan & Taylor (2013; E3)",
                    "+ Current study (E1a)",
                    "+ Hassall et al. (2016)",
                    "+ Quinlan & Taylor (2019; E2)",
                    "+ Quinlan & Taylor (2019; E3)",
                    "+ Current study (E1b)",
                    "+ Current study (E2 Unmatched)",                  
                    "+ Current study (E2 Matched)",
                    "+ Current Study (E3 Matched)",                   
                    "Current Study (E3 Unmatched)"))

# calculate model estimates

cumul_sum <- group_by(v, plot_label) %>%
  median_hdi(intercept)

# create cumulative plot

v %>%
  ggplot(aes(x = intercept, y = plot_label)) +
  geom_vline(xintercept = 0, color = 'black', lty = 2) +
  stat_halfeye(.width = c(.5, .95), fill = "#b5b5b5", point_interval = 'median_hdi',
               slab_alpha = 0.5,
               scale = 0.8) +
  geom_text(
    data = cumul_sum%>% mutate_if(is.numeric, round, digits=2) %>%
      mutate_if(is.numeric, formatC, format='f', digits=2),
    aes(label = str_glue("{intercept} [{.lower}, {.upper}]"), x = 1.5),
    hjust = "inward"
  ) +
  labs(x = 'Raw Mean Difference', y = NULL) +
  scale_x_continuous(limits = c(-1,1.5), breaks = seq(-1, 1.5, 0.5)) +
  theme_classic()
