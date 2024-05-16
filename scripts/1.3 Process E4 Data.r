
# Note: This is the between-subject experiment conducted by Huff. Experiment 4
# in manuscript
# exclusion information can be found in the manuscript
#

# Load Data ---------------------------------------------------------------

# Reading both phases at once
e4_test_dat = NULL
for(l in list.files('data/e4', pattern='.csv'))
{
  print(l)
  dat = read.table(paste('data/e4', l, sep='/'), sep=',', header=TRUE, fill=TRUE)
  dat = select(dat, test_word:frameRate)
  dat = dat %>% 
    select(participant, test_order, words = test_word, condition=test_condition, old_new_test, conf=keyConf.keys, conf_rt=keyConf.rt, 
           rkn=keyRKN.keys, rkn_rt=keyRKN.rt)
  
  e4_test_dat = rbind(e4_test_dat, dat)
}

# Create matched foils
e4_test_dat = e4_test_dat %>%
  filter(!is.na(as.numeric(as.character(test_order)))) %>%
  mutate(instruction = str_replace(condition, 'new_', ''), is_old = old_new_test == 'old', is_old_cent = is_old - .5) %>%
  mutate(conf=as.numeric(as.character(conf))) %>%
  mutate(said_old = as.numeric(conf > 3), instruction = recode(instruction, a = 'aloud', b = 'silent', c = 'sing'))

# summary stats & related -------------------------------------------------

e4_test_dat %>%
  group_by(participant, instruction, is_old) %>%
  summarize(n=n(), m=mean(conf > 3), r=mean(rkn=='r'), f_raw=mean(rkn=='f'), f=mean(rkn[rkn!='r']=='f'), new=mean(conf < 4)) -> e4_test_sum

# fix irk values

e4_test_sum[is.na(e4_test_sum)]<- 0

# d' calculations
e4_test_sum %>%
  filter(!(participant %in% c('June27Sing1'))) %>%
  filter(!(participant %in% c('87 Sing'))) %>%
  filter(!(participant %in% c('85 Silent'))) %>%
  filter(!(participant %in% c('84 Sing'))) %>%
  filter(!(participant %in% c('82  Silent'))) %>%
  filter(!(participant %in% c('81 Sing'))) %>%
  filter(!(participant %in% c('72 Sing'))) %>%
  filter(!(participant %in% c('71 Aloud'))) %>%
  filter(!(participant %in% c('66 Sing'))) %>%
  filter(!(participant %in% c('118 Silent'))) %>%
  filter(!(participant %in% c('112 Silent'))) %>%
  filter(!(participant %in% c('110 Aloud'))) %>%
  filter(!(participant %in% c('105 Sing'))) %>%
  filter(!(participant %in% c('63 Sing'))) %>%
  filter(!(participant %in% c('sing45'))) %>%
  select(participant, instruction, is_old, m) %>%
  pivot_wider(id_cols = c('participant', 'instruction'), names_from = 'is_old', values_from = 'm') %>%
  mutate(d = calc_d(`TRUE`, `FALSE`, cor = .005)) %>%
  select(instruction, participant, d) -> e4_d_sum
 
# d' calculations for recollection

e4_test_sum %>%
  filter(!(participant %in% c('June27Sing1'))) %>%
  filter(!(participant %in% c('87 Sing'))) %>%
  filter(!(participant %in% c('85 Silent'))) %>%
  filter(!(participant %in% c('84 Sing'))) %>%
  filter(!(participant %in% c('82  Silent'))) %>%
  filter(!(participant %in% c('81 Sing'))) %>%
  filter(!(participant %in% c('72 Sing'))) %>%
  filter(!(participant %in% c('71 Aloud'))) %>%
  filter(!(participant %in% c('66 Sing'))) %>%
  filter(!(participant %in% c('118 Silent'))) %>%
  filter(!(participant %in% c('112 Silent'))) %>%
  filter(!(participant %in% c('110 Aloud'))) %>%
  filter(!(participant %in% c('105 Sing'))) %>%
  filter(!(participant %in% c('63 Sing'))) %>%
  filter(!(participant %in% c('sing45'))) %>%
  select(participant, instruction, is_old, r) %>%
  pivot_wider(id_cols = c('participant', 'instruction'), names_from = 'is_old', values_from = 'r') %>%
  mutate(d = calc_d(`TRUE`, `FALSE`, cor = .005)) %>%
  select(instruction, participant, d) -> e4_d_sum_rec

e4_test_sum %>%
  filter(!(participant %in% c('June27Sing1'))) %>%
  filter(!(participant %in% c('87 Sing'))) %>%
  filter(!(participant %in% c('85 Silent'))) %>%
  filter(!(participant %in% c('84 Sing'))) %>%
  filter(!(participant %in% c('82  Silent'))) %>%
  filter(!(participant %in% c('81 Sing'))) %>%
  filter(!(participant %in% c('72 Sing'))) %>%
  filter(!(participant %in% c('71 Aloud'))) %>%
  filter(!(participant %in% c('66 Sing'))) %>%
  filter(!(participant %in% c('118 Silent'))) %>%
  filter(!(participant %in% c('112 Silent'))) %>%
  filter(!(participant %in% c('110 Aloud'))) %>%
  filter(!(participant %in% c('105 Sing'))) %>%
  filter(!(participant %in% c('63 Sing'))) %>%
  filter(!(participant %in% c('sing45'))) %>%
  pivot_wider(id_cols = c('participant', 'instruction'), names_from = 'is_old', values_from = 'f') %>%
  mutate(d = calc_d(`TRUE`, `FALSE`, cor = .005)) %>%
  select(instruction, participant, d) -> e4_d_sum_fam

e4_test_sum %>%
  filter(!(participant %in% c('June27Sing1'))) %>%
  filter(!(participant %in% c('87 Sing'))) %>%
  filter(!(participant %in% c('85 Silent'))) %>%
  filter(!(participant %in% c('84 Sing'))) %>%
  filter(!(participant %in% c('82  Silent'))) %>%
  filter(!(participant %in% c('81 Sing'))) %>%
  filter(!(participant %in% c('72 Sing'))) %>%
  filter(!(participant %in% c('71 Aloud'))) %>%
  filter(!(participant %in% c('66 Sing'))) %>%
  filter(!(participant %in% c('118 Silent'))) %>%
  filter(!(participant %in% c('112 Silent'))) %>%
  filter(!(participant %in% c('110 Aloud'))) %>%
  filter(!(participant %in% c('105 Sing'))) %>%
  filter(!(participant %in% c('63 Sing'))) %>%
  filter(!(participant %in% c('sing45'))) %>%
  pivot_wider(id_cols = c('participant', 'instruction'), names_from = 'is_old', values_from = 'f_raw') %>%
  mutate(d = calc_d(`TRUE`, `FALSE`, cor = .005)) %>%
  select(instruction, participant, d) -> e4_d_sum_fam_raw


##### probit models --------

#model for conf judgements

# define formula

e4m1 <- bf(
  said_old ~ Phi(dprime * is_old_cent - c),
  dprime ~ instruction-1 + (1|participant) + (instruction-1|words),
  c ~ instruction-1 + (1|participant) + (instruction-1|words),
  nl = TRUE
)

# set prior

Priors <- c(
  prior(normal(1, 1), nlpar = "dprime"),
  prior(normal(0, 1), nlpar = "dprime", class = "sd"),
  prior(normal(0, 1), nlpar = "c"),
  prior(normal(0, 1), nlpar = "c", class = "sd"),
  prior(lkj(4), class = "cor")
)

# excluding bad participants

nd <- e4_test_dat%>%
  filter(!(participant %in% c('June27Sing1'))) %>%
  filter(!(participant %in% c('87 Sing'))) %>%
  filter(!(participant %in% c('85 Silent'))) %>%
  filter(!(participant %in% c('84 Sing'))) %>%
  filter(!(participant %in% c('82  Silent'))) %>%
  filter(!(participant %in% c('81 Sing'))) %>%
  filter(!(participant %in% c('72 Sing'))) %>%
  filter(!(participant %in% c('71 Aloud'))) %>%
  filter(!(participant %in% c('66 Sing'))) %>%
  filter(!(participant %in% c('118 Silent'))) %>%
  filter(!(participant %in% c('112 Silent'))) %>%
  filter(!(participant %in% c('110 Aloud'))) %>%
  filter(!(participant %in% c('105 Sing'))) %>%
  filter(!(participant %in% c('63 Sing'))) %>%
  filter(!(participant %in% c('sing45'))) %>%
  mutate(said_rec = as.numeric(rkn == 'r'),
         said_fam = as.numeric(rkn == 'f'))



# fit model

e4_conf_large <- brm(
  e4m1,
  family = bernoulli(link = "identity"),
  data = nd,
  prior = Priors,
  backend = 'cmdstanr',
  iter= 15000,
  warmup = 7500,
  control = list(adapt_delta = .90),
  chains= 8,
  cores = 8,
  init = 0,
  sample_prior = 'yes',
)

# plot conf

e4_conf_large %>%
  as_draws_df() %>%
  rename(Aloud = 'b_dprime_instructionaloud',
         Sing = 'b_dprime_instructionsing',
         Silent = 'b_dprime_instructionsilent') %>%
  select(Aloud:Sing) %>%
  pivot_longer(everything(), names_to = 'Condition', values_to = 'Post') %>%
  mutate(Condition = factor(Condition, levels = c('Silent', 'Aloud', 'Sing'))) %>%
  ggplot(aes(y = Condition, x = Post)) +
  stat_halfeye(fill = '#b5b5b5',
               point_interval = 'median_hdi', slab_linewidth = 0.5, slab_alpha = 0.5, scale = 0.8) +
  labs(title = 'E4 Confidence', y = NULL, x = expression(~italic("d'"))) +
  scale_x_continuous(limits = c(0,2.0), breaks = seq(0, 2, 0.5)) +
  theme_classic() -> e4_conf_plot

# plot conf contrasts

e4_conf_large %>%
  as_draws_df() %>%
  rename(Aloud = 'b_dprime_instructionaloud',
         Sing = 'b_dprime_instructionsing',
         Silent = 'b_dprime_instructionsilent') %>%
  mutate('Aloud-Silent' = Aloud - Silent,
         'Sing-Silent' = Sing - Silent,
         'Sing-Aloud' = Sing - Aloud) %>%
  select('Aloud-Silent':'Sing-Aloud') %>%
  pivot_longer(everything(), names_to = 'Contrast', values_to = 'Diff') %>%
  mutate(Contrast = factor(Contrast, levels = c('Sing-Aloud', 'Aloud-Silent', 'Sing-Silent'))) %>%
  ggplot(aes(y = Contrast, x = Diff)) +
  stat_halfeye(fill = '#b5b5b5',
               point_interval = 'median_hdi', slab_linewidth = 0.5, slab_alpha = 0.5, scale = 0.8) +
  geom_vline(xintercept = 0, color = "black", linetype = "dashed") +
  labs(title = '', y = NULL, x = expression(~italic("d'"))) +
  scale_x_continuous(limits = c(-1.0,1.5), breaks = seq(-1, 1.5, 0.5)) +
  theme_classic() -> e4_conf_con_plot

# plots for conf c

e4_conf_large %>%
  as_draws_df() %>%
  rename(Aloud = 'b_c_instructionaloud',
         Sing = 'b_c_instructionsing',
         Silent = 'b_c_instructionsilent') %>%
  select(Aloud:Sing) %>%
  pivot_longer(everything(), names_to = 'Condition', values_to = 'Post') %>%
  mutate(Condition = factor(Condition, levels = c('Silent', 'Aloud', 'Sing'))) %>%
  ggplot(aes(y = Condition, x = Post)) +
  stat_halfeye(fill = '#b5b5b5',
               point_interval = 'median_hdi', slab_linewidth = 0.5, slab_alpha = 0.5, scale = 0.8) +
  labs(title = 'E4 Confidence', y = NULL, x = expression(~italic("C"))) +
  scale_x_continuous(limits = c(-1,2.0), breaks = seq(-1, 2, 0.5)) +
  theme_classic() -> e4_conf_plot_c

# plot conf c contrasts

e4_conf_large %>%
  as_draws_df() %>%
  rename(Aloud = 'b_c_instructionaloud',
         Sing = 'b_c_instructionsing',
         Silent = 'b_c_instructionsilent') %>%
  mutate('Aloud-Silent' = Aloud - Silent,
         'Sing-Silent' = Sing - Silent,
         'Sing-Aloud' = Sing - Aloud) %>%
  select('Aloud-Silent':'Sing-Aloud') %>%
  pivot_longer(everything(), names_to = 'Contrast', values_to = 'Diff') %>%
  mutate(Contrast = factor(Contrast, levels = c('Sing-Aloud', 'Aloud-Silent', 'Sing-Silent'))) %>%
  ggplot(aes(y = Contrast, x = Diff)) +
  stat_halfeye(fill = '#b5b5b5',
               point_interval = 'median_hdi', slab_linewidth = 0.5, slab_alpha = 0.5, scale = 0.8) +
  geom_vline(xintercept = 0, color = "black", linetype = "dashed") +
  labs(title = '', y = NULL, x = expression(~italic("C"))) +
  scale_x_continuous(limits = c(-1,1.0), breaks = seq(-1, 1, 0.5)) +
  theme_classic() -> e4_conf_con_plot_c

# rec/fsm

# create IRK familiarity data

fd_e4 <- nd %>%
  filter(rkn != 'r')

# rec model formula

e4rm1 <- bf(
  said_rec ~ Phi(dprime * is_old_cent - c),
  dprime ~ instruction-1 + (1|participant) + (instruction-1|words), 
  c ~ instruction-1 + (1|participant) + (instruction-1|words),
  nl = TRUE
)

# def priors

Priors <- c(
  prior(normal(1, 1), nlpar = "dprime"),
  prior(normal(0, 1), nlpar = "dprime", class = "sd"),
  prior(normal(0, 1), nlpar = "c"),
  prior(normal(0, 1), nlpar = "c", class = "sd"),
  prior(lkj(4), class = "cor")
)

# fit rec model

e4_rec_large <- brm(
  e4rm1,
  family = bernoulli(link = "identity"),
  data = nd,
  prior = Priors,
  backend = 'cmdstanr',
  iter= 15000,
  warmup = 7500,
  control = list(adapt_delta = .90),
  chains= 8,
  cores = 8,
  init = 0,
  sample_prior = 'yes'
)

# plot rec

e4_rec_large %>%
  as_draws_df() %>%
  rename(Aloud = 'b_dprime_instructionaloud',
         Sing = 'b_dprime_instructionsing',
         Silent = 'b_dprime_instructionsilent') %>%
  select(Aloud:Sing) %>%
  pivot_longer(everything(), names_to = 'Condition', values_to = 'Post') %>%
  mutate(Condition = factor(Condition, levels = c('Silent', 'Aloud', 'Sing'))) %>%
  ggplot(aes(y = Condition, x = Post)) +
  stat_halfeye(fill = '#b5b5b5',
               point_interval = 'median_hdi', slab_linewidth = 0.5, slab_alpha = 0.5, scale = 0.8) +
  labs(title = 'E4 Recollection', y = NULL, x = expression(~italic("d'"))) +
  scale_x_continuous(limits = c(0,2.0), breaks = seq(0, 2, 0.5)) +
  theme_classic() -> e4_rec_plot

# plot rec contrasts

e4_rec_large %>%
  as_draws_df() %>%
  rename(Aloud = 'b_dprime_instructionaloud',
         Sing = 'b_dprime_instructionsing',
         Silent = 'b_dprime_instructionsilent') %>%
  mutate('Aloud-Silent' = Aloud - Silent,
         'Sing-Silent' = Sing - Silent,
         'Sing-Aloud' = Sing - Aloud) %>%
  select('Aloud-Silent':'Sing-Aloud') %>%
  pivot_longer(everything(), names_to = 'Contrast', values_to = 'Diff') %>%
  mutate(Contrast = factor(Contrast, levels = c('Sing-Aloud', 'Aloud-Silent', 'Sing-Silent'))) %>%
  ggplot(aes(y = Contrast, x = Diff)) +
  stat_halfeye(fill = '#b5b5b5',
               point_interval = 'median_hdi', slab_linewidth = 0.5, slab_alpha = 0.5, scale = 0.8) +
  geom_vline(xintercept = 0, color = "black", linetype = "dashed") +
  labs(title = '', y = NULL, x = expression(~italic("d'"))) +
  scale_x_continuous(limits = c(-1.0,1.5), breaks = seq(-1, 1.5, 0.5)) +
  theme_classic() -> e4_rec_con_plot

# plots for c rec

e4_rec_large %>%
  as_draws_df() %>%
  rename(Aloud = 'b_c_instructionaloud',
         Sing = 'b_c_instructionsing',
         Silent = 'b_c_instructionsilent') %>%
  select(Aloud:Sing) %>%
  pivot_longer(everything(), names_to = 'Condition', values_to = 'Post') %>%
  mutate(Condition = factor(Condition, levels = c('Silent', 'Aloud', 'Sing'))) %>%
  ggplot(aes(y = Condition, x = Post)) +
  stat_halfeye(fill = '#b5b5b5',
               point_interval = 'median_hdi', slab_linewidth = 0.5, slab_alpha = 0.5, scale = 0.8) +
  labs(title = 'E4 Recollection', y = NULL, x = expression(~italic("C"))) +
  scale_x_continuous(limits = c(-1,2.0), breaks = seq(-1, 2, 0.5)) +
  theme_classic() -> e4_rec_plot_c

# plot c rec contrasts

e4_rec_large %>%
  as_draws_df() %>%
  rename(Aloud = 'b_c_instructionaloud',
         Sing = 'b_c_instructionsing',
         Silent = 'b_c_instructionsilent') %>%
  mutate('Aloud-Silent' = Aloud - Silent,
         'Sing-Silent' = Sing - Silent,
         'Sing-Aloud' = Sing - Aloud) %>%
  select('Aloud-Silent':'Sing-Aloud') %>%
  pivot_longer(everything(), names_to = 'Contrast', values_to = 'Diff') %>%
  mutate(Contrast = factor(Contrast, levels = c('Sing-Aloud', 'Aloud-Silent', 'Sing-Silent'))) %>%
  ggplot(aes(y = Contrast, x = Diff)) +
  stat_halfeye(fill = '#b5b5b5',
               point_interval = 'median_hdi', slab_linewidth = 0.5, slab_alpha = 0.5, scale = 0.8) +
  geom_vline(xintercept = 0, color = "black", linetype = "dashed") +
  labs(title = '', y = NULL, x = expression(~italic("C"))) +
  scale_x_continuous(limits = c(-1,1.0), breaks = seq(-1, 1, 0.5)) +
  theme_classic() -> e4_rec_con_plot_c


# fam model formula

e4fm1 <- bf(
  said_fam ~ Phi(dprime * is_old_cent - c),
  dprime ~ instruction-1 + (1|participant) + (instruction-1|words), 
  c ~ instruction-1 + (1|participant) + (instruction-1|words),
  nl = TRUE
)

# fit fam model

e4_fam_large <- brm(
  e4fm1,
  family = bernoulli(link = "identity"),
  data = fd_e4,
  prior = Priors,
  backend = 'cmdstanr',
  iter= 15000,
  warmup = 7500,
  control = list(adapt_delta = .90),
  chains= 8,
  cores = 8,
  init = 0,
  sample_prior = 'yes'
)

# plot fam

e4_fam_large %>%
  as_draws_df() %>%
  rename(Aloud = 'b_dprime_instructionaloud',
         Sing = 'b_dprime_instructionsing',
         Silent = 'b_dprime_instructionsilent') %>%
  select(Aloud:Sing) %>%
  pivot_longer(everything(), names_to = 'Condition', values_to = 'Post') %>%
  mutate(Condition = factor(Condition, levels = c('Silent', 'Aloud', 'Sing'))) %>%
  ggplot(aes(y = Condition, x = Post)) +
  stat_halfeye(fill = '#b5b5b5',
               point_interval = 'median_hdi', slab_linewidth = 0.5, slab_alpha = 0.5, scale = 0.8) +
  labs(title = 'E4 Familiarity', y = NULL, x = expression(~italic("d'"))) +
  scale_x_continuous(limits = c(0,2.0), breaks = seq(0, 2, 0.5)) +
  theme_classic() -> e4_fam_plot

# plot fam contrasts

e4_fam_large %>%
  as_draws_df() %>%
  rename(Aloud = 'b_dprime_instructionaloud',
         Sing = 'b_dprime_instructionsing',
         Silent = 'b_dprime_instructionsilent') %>%
  mutate('Aloud-Silent' = Aloud - Silent,
         'Sing-Silent' = Sing - Silent,
         'Sing-Aloud' = Sing - Aloud) %>%
  select('Aloud-Silent':'Sing-Aloud') %>%
  pivot_longer(everything(), names_to = 'Contrast', values_to = 'Diff') %>%
  mutate(Contrast = factor(Contrast, levels = c('Sing-Aloud', 'Aloud-Silent', 'Sing-Silent'))) %>%
  ggplot(aes(y = Contrast, x = Diff)) +
  stat_halfeye(fill = '#b5b5b5',
               point_interval = 'median_hdi', slab_linewidth = 0.5, slab_alpha = 0.5, scale = 0.8) +
  geom_vline(xintercept = 0, color = "black", linetype = "dashed") +
  labs(title = '', y = NULL, x = expression(~italic("d'"))) +
  scale_x_continuous(limits = c(-1.0,1.5), breaks = seq(-1, 1.5, 0.5)) +
  theme_classic() -> e4_con_fam_plot

# plots for c fam

e4_fam_large %>%
  as_draws_df() %>%
  rename(Aloud = 'b_c_instructionaloud',
         Sing = 'b_c_instructionsing',
         Silent = 'b_c_instructionsilent') %>%
  select(Aloud:Sing) %>%
  pivot_longer(everything(), names_to = 'Condition', values_to = 'Post') %>%
  mutate(Condition = factor(Condition, levels = c('Silent', 'Aloud', 'Sing'))) %>%
  ggplot(aes(y = Condition, x = Post)) +
  stat_halfeye(fill = '#b5b5b5',
               point_interval = 'median_hdi', slab_linewidth = 0.5, slab_alpha = 0.5, scale = 0.8) +
  labs(title = 'E4 Familiarity', y = NULL, x = expression(~italic("C"))) +
  scale_x_continuous(limits = c(-1,2.0), breaks = seq(-1, 2, 0.5)) +
  theme_classic() -> e4_fam_plot_c

# plot c fam contrasts

e4_fam_large %>%
  as_draws_df() %>%
  rename(Aloud = 'b_c_instructionaloud',
         Sing = 'b_c_instructionsing',
         Silent = 'b_c_instructionsilent') %>%
  mutate('Aloud-Silent' = Aloud - Silent,
         'Sing-Silent' = Sing - Silent,
         'Sing-Aloud' = Sing - Aloud) %>%
  select('Aloud-Silent':'Sing-Aloud') %>%
  pivot_longer(everything(), names_to = 'Contrast', values_to = 'Diff') %>%
  mutate(Contrast = factor(Contrast, levels = c('Sing-Aloud', 'Aloud-Silent', 'Sing-Silent'))) %>%
  ggplot(aes(y = Contrast, x = Diff)) +
  stat_halfeye(fill = '#b5b5b5',
               point_interval = 'median_hdi', slab_linewidth = 0.5, slab_alpha = 0.5, scale = 0.8) +
  geom_vline(xintercept = 0, color = "black", linetype = "dashed") +
  labs(title = '', y = NULL, x = expression(~italic("C"))) +
  scale_x_continuous(limits = c(-1,1.0), breaks = seq(-1, 1, 0.5)) +
  theme_classic() -> e4_fam_con_plot_c

ggarrange(e4_conf_plot, e4_conf_con_plot, 
          e4_rec_plot, e4_rec_con_plot,
          e4_fam_plot, e4_con_fam_plot,
          nrow = 3, ncol = 2, widths = c(0.75,1))

ggarrange(e4_conf_plot_c, e4_conf_con_plot_c, 
          e4_rec_plot_c, e4_rec_con_plot_c,
          e4_fam_plot_c, e4_fam_con_plot_c,
          nrow = 3, ncol = 2, widths = c(0.75,1))