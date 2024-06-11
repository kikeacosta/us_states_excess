rm (list = ls())
source("code/00_setup.R")


# loading data about government political affiliation
polit <- read_csv("data_input/woolf_tabs3.csv")
codes <- read_csv("data_input/us_state_codes.csv")

pol2 <- 
  polit %>% 
  filter(phase %in% 1:3) %>% 
  group_by(state) %>% 
  summarise(pol = round(mean(polit))) %>%  
  bind_rows(tibble(state = "District of Columbia", pol = 1),
            tibble(state = "Nebraska", pol = 4),
            # tibble(state = "US", pol = NA)
            ) %>% 
  mutate(chm = case_when(pol == 1 ~ "Democrat",
                         pol == 4 ~ "Republican",
                         TRUE ~ "Mixed"),
         gov = ifelse(pol <= 2, "dem","rep"))


# phases definitions
# phases for adjusting rates denominators
ph1 <- ymd(c("2020-03-07", "2020-06-13"))
ph2 <- ymd(c("2020-06-20", "2021-03-13"))
ph3 <- ymd(c("2021-03-20", "2022-03-19"))
ph4 <- ymd(c("2022-03-26", "2023-02-04"))
ph5 <- ymd(c("2023-02-11", "2023-07-02"))

# only for phases 1-4, 
# excluding phase 5 (less than 500 excess deaths in the whole US!!)

# fractions for incomplete years 2020 and 2023
fr2020 <- interval(ymd("2020-03-07"), ymd("2020-12-31")) %>% as.numeric('years')
fr2023 <- interval(ymd("2023-01-01"), ymd("2023-02-04")) %>% as.numeric('years')

# US population by age
# will be used as the reference population for standardization 
pop_us <- read_tsv("data_input/pop_us_agex5_2015_2023.txt")

pop_us2 <- 
  pop_us %>% 
  select(year = `Year Code`,
         age = `Age Group Code`,
         pop = `Projected Populations`) %>% 
  mutate(age = parse_number(age),
         age = if_else(age < 15,0,age)) %>% 
  filter(year %in% 2020:2023) %>% 
  # adding the fraction for each year
  mutate(fr = case_when(year == 2020 ~ fr2020,
                        year == 2023 ~ fr2023,
                        TRUE ~ 1)) %>% 
  # calculating exposures for the whole period 2020-03-07 - 2023-07-02
  group_by(age) %>% 
  summarise(exposure = sum(pop * fr),
            .groups = "drop") %>% 
  mutate(cx = exposure/sum(exposure)) %>% 
  select(-exposure)


# Population by state by age
pop_sts <- read_csv("data_inter/pop_state_age_2013_2024.csv")

# calculating population exposures by state in person-years for all phases
pop_sts2 <- 
  pop_sts %>% 
  filter(year %in% 2020:2023) %>% 
  # adding the fraction for each year
  mutate(fr = case_when(year == 2020 ~ fr2020,
                        year == 2023 ~ fr2023,
                        TRUE ~ 1)) %>% 
  # calculating exposures for the whole period 2020-03-07 - 2023-07-02
  group_by(state, age) %>% 
  summarise(exposure = sum(pop * fr),
            .groups = "drop")


pop_sts_tot <- 
  pop_sts2 %>% 
  group_by(state) %>% 
  summarise(exp_tot = sum(exposure),.groups = "drop")


# excess estimates by state and age
exc <- read_csv("data_inter/excess_state_phase_age.csv")

# merging excess deaths and exposures (by age)
exc2 <- 
  exc %>% 
  filter(phase %in% 1:4) %>% 
  group_by(state, age) %>% 
  summarise(dx = sum(dx),
            bsn = sum(bsn),
            exc = sum(exc),
            .groups = "drop") %>% 
  left_join(pop_sts2, 
            by = join_by(state, age)) %>% 
  # this is an approximation, taking info from just 1 pop estimate,
  # can be refined
  mutate(# excess rates (exc_r) expressed as per 100k, by age
    exc_r = 1e5 * exc / exposure,
    # baseline rates (bsn_r) expressed as per 100k, by age
    bsn_r = 1e5 * bsn / exposure,
    # death rates (mx) expressed as per 100k, by age
    mx = 1e5 * dx / exposure
  )

write_csv(exc2, "data_inter/excess_rates_by_age_phases_1_4.csv")


# crude excess rates for the whole period 
tot_rates <- 
  exc2 %>% 
  group_by(state) %>% 
  summarise(exc = sum(exc),
            bsn = sum(bsn),
            exposure = sum(exposure)) %>% 
  mutate(exc_r = 1e5*exc/exposure,
         psc = exc/bsn) %>%
  arrange(exc_r)

# average p-score across states 
tot_rates %>% 
    summarise(psc_av = mean(psc))


# age-specific excess deaths according to standard population
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# using the age structure of the US
# and the total population in each state

std <- 
  exc2 %>% 
  select(state, age, exc_r, bsn_r, mx) %>% 
  left_join(pop_us2, 
            by = join_by(age)) %>% 
  left_join(pop_sts_tot, 
            by = join_by(state)) %>% 
  # remember to undo the 100k units of rates
  mutate(
    exp_std = exp_tot * cx,
    exc_std = exc_r * exp_std / 1e5,
    bsn_std = bsn_r * exp_std / 1e5,
    dx_std = mx * exp_std / 1e5,
  )

std %>% 
  ggplot()+
  geom_line(aes(age, exc_r, group = state),
            alpha = .5)+
  scale_y_log10()+
  theme_bw()



std2 <- 
  std %>% 
  # calculate age-standardized obs death, baseline, and excess rates
  group_by(state) %>% 
  summarise(exc_std = sum(exc_std),
            bsn_std = sum(bsn_std),
            dx_std = sum(dx_std),
            exposure = sum(exp_std), 
            .groups = "drop") %>% 
  # once again age-standardized excess rate scaled up to pre 100k
  mutate(exc_r_std = 1e5 * exc_std / exposure,
         bsn_r_std = 1e5 * bsn_std / exposure,
         mx_std = 1e5 * dx_std / exposure,
         psc = exc_std / bsn_std) %>% 
  arrange(-exc_r_std) %>% 
  mutate(rnk_exc_std = 1:n()) %>% 
  arrange(-psc) %>% 
  mutate(rnk_psc_std = 1:n()) %>% 
  arrange(rnk_exc_std)

std2 |> 
  ggplot(aes(x = psc, y = reorder(state,psc))) +
  geom_point() +
  theme_minimal() +
  labs(x = "P-score (excess / baseline)", y = "")


# checks, for quick stats
std2 %>% 
  summarize(psc_av = mean(psc))

std2 %>% 
  summarize(exc = sum(exc_std))

write_csv(std2, "data_inter/age_standardized_excess_rates_states.csv")

# looking at raw ranking
# ~~~~~~~~~~~~~~~~~~~~~~

# adding all phases together for accumulating Wolff's estimates
exc_wlf <- 
  exc %>% 
  left_join(pop_sts2, 
            by = join_by(state, age)) %>% 
  group_by(state) %>% 
  summarise(exc = sum(exc),
            bsn = sum(bsn),
            dts = sum(dx),
            exposure = sum(exposure),
            .groups = "drop") %>% 
  mutate(exc_r = 1e5*exc/exposure,
         psc = exc / bsn) %>% 
  arrange(-exc_r) %>% 
  mutate(rnk_exc_raw = 1:n()) %>% 
  arrange(-psc) %>% 
  mutate(rnk_psc_raw = 1:n()) %>% 
  arrange(rnk_exc_raw)



# comparing standardized and raw ranks
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
cmp <- 
  std2 %>% 
  select(state, rnk_exc_std, rnk_psc_std) %>% 
  left_join(exc_wlf %>% 
              select(state, rnk_exc_raw, rnk_psc_raw),
            by = join_by(state))


write_csv(cmp, "data_inter/preliminary_ranks_raw_std_cum.csv")


# some plots comparing crude and age-standardized state rankings over full cumulative period
cmp |>
  left_join(codes, by = join_by(state)) |> 
  ggplot(aes(x = rnk_exc_std, y = rnk_exc_raw, label = state_code)) +
  coord_equal() +
  theme_minimal() +
  geom_abline(slope = 1, intercept = 0, color = "red") +
  geom_text() +
  annotate("text",40,10,label = "lower rank when\nage standardized", size = 6) +
  annotate("text",10,40,label = "higher rank when\nage standardized", size = 6) +
  labs(x = "age standardized excess rank",
       y = "crude excess rank")

cmp |>
  left_join(codes, by = join_by(state)) |> 
  ggplot(aes(x = rnk_psc_std, y = rnk_psc_raw, label = state_code)) +
  coord_equal() +
  theme_minimal() +
  geom_abline(slope = 1, intercept = 0, color = "red") +
  geom_text() +
  annotate("text",40,10,label = "lower rank when\nage standardized", size = 6) +
  annotate("text",10,40,label = "higher rank when\nage standardized", size = 6) +
  labs(x = "age standardized p-score rank",
       y = "crude p-score rank")



# best practice mortality
# ~~~~~~~~~~~~~~~~~~~~~~~
# selecting the lowest mortality baseline in each age among all states
bst <- 
  exc2 %>% 
  group_by(age) %>% 
  filter(bsn_r == min(bsn_r)) %>% 
  arrange(age) 

# plot which states make up the best-practice schedule
bst |> 
  mutate(ageint = if_else(age==0,15,10) ) |> 
  ggplot(aes(y = mx/1e5,
             yend = mx/1e5, 
             color = state, 
             label = state)) +
  geom_segment(mapping = aes(x=age, 
                             xend = (ageint + age)),
               linewidth=2) +
  scale_y_log10() +
  scale_x_continuous(breaks = c(0, seq(15, 85, 10)))+
  theme_minimal() +
  geom_text(mapping = aes(x=age+ageint/2),nudge_y =.1) +
  guides(color = "none") +
  labs(x = "age",
       y = "best practice mortality rate", 
       title = "state composition of best practice mortality schedule")

ggsave("figures/best_practice_age_specific_death_rates.png",
       w = 6,
       h = 4)

age_strs <- 
  exc2 %>% 
  mutate(ageint = if_else(age==0,15,10) )

bst |> 
  mutate(ageint = if_else(age==0,15,10) ) |> 
  ggplot(aes(y = mx/1e5,
             yend = mx/1e5, 
             color = state, 
             label = state)) +
  geom_segment(mapping = aes(x=age, 
                             xend = (ageint + age)),
               linewidth=2) +
  geom_segment(data = age_strs,
                 mapping = aes(x=age, 
                             xend = (ageint + age)),
               linewidth=.2,
               alpha = .3,
               col = "grey"
               ) +
  scale_y_log10() +
  scale_x_continuous(breaks = c(0, seq(15, 85, 10)))+
  theme_minimal() +
  geom_text(mapping = aes(x=age+ageint/2),nudge_y =-.1, size = 2.5) +
  guides(color = "none") +
  labs(x = "age",
       y = "best practice mortality rate", 
       title = "state composition of best practice mortality schedule")

ggsave("figures/test.png",
       w = 6,
       h = 4)

bst2 <- 
  bst %>% 
  select(age, bsn_bst_r = bsn_r)


# pre-pandemic excess
# ~~~~~~~~~~~~~~~~~~~
exc_pre <- 
  std %>% 
  # joining each state with the best baseline practice for each age
  left_join(bst2, by = join_by(age)) %>% 
  mutate(bsn_bst = bsn_bst_r * exp_std / 1e5,
         exc_pre_r = bsn_r - bsn_bst_r,
         exc_pre = exc_pre_r * exp_std / 1e5)

# all ages together
exc_pre2 <- 
  exc_pre %>% 
  group_by(state) %>% 
  summarise(dx_std = sum(dx_std),
            bsn_std = sum(bsn_std),
            exc_std = sum(exc_std),
            bsn_bst = sum(bsn_bst),
            exc_pre = sum(exc_pre),
            exposure = sum(exp_std), 
            .groups = "drop") %>% 
  mutate(exc_r = 1e5*exc_std/exposure,
         exc_pre_r = 1e5*exc_pre/exposure) %>% 
  arrange(exc_r) %>% 
  mutate(rnk = 1:n())

# total age-standardized pandemic and inequality excess
exc_pre2 %>% 
  summarise(exc_std = sum(exc_std),
            exc_pre = sum(exc_pre)) %>% 
  mutate(ratio = exc_pre/exc_std) 


# all US together
exc_pre_us <- 
  exc_pre %>% 
  summarise(dx_std = sum(dx_std),
            bsn_std = sum(bsn_std),
            exc_std = sum(exc_std),
            bsn_bst = sum(bsn_bst),
            exc_pre = sum(exc_pre),
            exposure = sum(exp_std), 
            .groups = "drop") %>% 
  mutate(exc_r = 1e5*exc_std/exposure,
         exc_pre_r = 1e5*exc_pre/exposure) %>% 
  arrange(exc_std) %>% 
  mutate(rnk = 52) %>% 
  mutate(state = "US")

exc_pre3 <- 
  exc_pre2 %>% 
  bind_rows(exc_pre_us) %>% 
  arrange(rnk) %>% 
  select(state, rnk, exc_r, exc_pre_r) %>% 
  pivot_longer(c(exc_r, exc_pre_r), 
               names_to = "exc_typ", 
               values_to = "exc_r")

# ratio of pandemic to preexisting excess rates
exc_ratio <- 
  exc_pre2 %>% 
  bind_rows(exc_pre_us) %>% 
  arrange(rnk) %>% 
  # spread()
  select(state, rnk, exc_r, exc_pre_r) %>% 
  mutate(ratio = exc_pre_r / exc_r)

# average of ratios. Not same as ratio of the averages
exc_ratio %>% 
  filter(state != "US") %>% 
  summarise(ratio = mean(ratio))


pscs <- 
  exc_pre2 %>% 
  mutate(psc_pre = exc_pre / bsn_bst,
         psc_pan = exc_std / bsn_std) %>% 
  arrange(psc_pan) %>% 
  mutate(rnk = 1:n()) %>% 
  bind_rows(
    exc_pre_us %>% 
      mutate(psc_pre = exc_pre / bsn_bst,
             psc_pan = exc_std / bsn_std)
  ) %>% 
  arrange(rnk) %>% 
  select(state, rnk, psc_pan, psc_pre) %>% 
  pivot_longer(c(psc_pan, psc_pre), names_to = "exc_typ", values_to = "psc")

# AS excess death rates
exc_pre3 %>% 
  filter(exc_typ == "exc_r") %>% 
  ggplot(aes(y=exc_r, x=reorder(state, rnk))) + 
  geom_bar(position="dodge", stat="identity", fill = "black")+
  # scale_fill_manual(values = cols)+
  coord_flip()+
  labs(y = "Age standardized excess death rates")+
  theme_bw()+
  theme(legend.position = c(.75, .1),
        legend.background = element_blank(),
        legend.text = element_text(size = 11),
        legend.title = element_blank(),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_blank())

# ggsave("figures/excess_pandemic.png",
#        w = 4,
#        h = 7.5)


pscs %>% 
  filter(exc_typ == "psc_pan") %>% 
  ggplot() + 
  # geom_bar(position="dodge", stat="identity", fill = "black")+
  # scale_fill_manual(values = cols)+
  geom_point(aes(x=psc, y=reorder(state, rnk)))+
  coord_cartesian(xlim = c(0, .26))+
  labs(x = "P-scores")+
  theme_bw()+
  theme(axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_blank())

# ggsave("figures/pscores_pandemic.png",
#        w = 4,
#        h = 7.5)

# ranked by pandemic excess, stacked, fill to governor party
exc_pre3 %>% 
  left_join(pol2,by=join_by(state)) |> 
  mutate(exc_typ = if_else(exc_typ == "exc_pre_r", "baseline excess","pandemic excess")) |> 
  ggplot(aes(x=exc_r, y=reorder(state, rnk), fill = gov,alpha=exc_typ)) + 
  geom_bar(position="stack", stat="identity",
           col = "grey30",
           width = .8)+
<<<<<<< HEAD
  scale_fill_identity() +
  coord_flip()+
  labs(y = "Age-standardized excess death rates (/100K)")+
  theme_bw()+
  theme(legend.position = c(.75, .1),
=======
  scale_fill_manual(values = c("#bb1212","#1212bb")) +
  scale_alpha_manual(values = c(.5,1))+
  labs(y = "Age-standardized excess death rates (/100K)",
       fill = "governor party",
       alpha = "excess type")+
  theme_bw() +
  theme(legend.position = c(.75, .3),
>>>>>>> c7aca000da15a6151cdce71e92dd551c745bc1d6
        legend.background = element_blank(),
        legend.text = element_text(size = 11),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 11),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_blank())

# ranked to overall excess
exc_pre3 %>% 
<<<<<<< HEAD
  mutate(exc_typ = factor(exc_typ, 
                          levels = c("exc_pre_r", "exc_r"))) %>% 
  ggplot() + 
  geom_point(data = pol2,
             aes(x = state, y = -3, col = gov),
             size = 3.3)+
  geom_bar(aes(fill=exc_typ, y=exc_r, x=reorder(state, rnk)),
           position="stack", stat="identity",
           col = "grey30",
           width = .8)+
  scale_fill_manual(values = cols, 
                    # breaks = c("exc_r", "exc_pre_r"),
                    labels = c("Disparity excess",
                               "Pandemic excesss"))+
  scale_color_manual(values = c("#3157d4", "#db2a2a"),
                     guide = 'none')+
  coord_flip()+
  labs(y = "Age-standardized excess death rates (/100K)")+
  theme_bw()+
  theme(legend.position = c(.75, .1),
=======
  left_join(pol2,by=join_by(state)) |> 
  mutate(exc_typ = if_else(exc_typ == "exc_pre_r", "baseline excess","pandemic excess")) |> 
  ggplot(aes(x=exc_r, y=reorder(state, exc_r), fill = gov,alpha=exc_typ)) + 
  geom_bar(position="stack", stat="identity",
           col = "grey30",
           width = .8)+
  scale_fill_manual(values = c("#bb1212","#1212bb")) +
  scale_alpha_manual(values = c(.5,1))+
  labs(y = "Age-standardized excess death rates (/100K)",
       fill = "governor party",
       alpha = "excess type")+
  theme_bw() +
  theme(legend.position = c(.75, .3),
>>>>>>> c7aca000da15a6151cdce71e92dd551c745bc1d6
        legend.background = element_blank(),
        legend.text = element_text(size = 11),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 11),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_blank())
<<<<<<< HEAD

ggsave("figures/excess_prepandemic_pandemic_v4.png",
       w = 6,
       h = 7.5)
=======
# ggsave("figures/excess_prepandemic_pandemic_v4.png",
#        w = 6,
#        h = 7.5)
>>>>>>> c7aca000da15a6151cdce71e92dd551c745bc1d6


# ggsave("figures/excess_prepandemic_pandemic_v2.png",
#        w = 6,
#        h = 7.5)

# compare
exc_pre3 %>% 
  pivot_wider(names_from = exc_typ, values_from = exc_r) %>% 
  left_join(pol2, by = join_by(state)) |> 
  left_join(codes, by = join_by(state)) |> 
  ggplot()+
  geom_text(aes(exc_pre_r, exc_r, color = gov, label = state_code))+
  theme_bw() +
  labs(y = "Pandemic excess",
       x = "Pre-pandemic excess") +
  scale_color_manual(values = c("#bb1212","#1212bb"))


library("ggpubr")
exc_pre3 %>% 
  #filter(state != "District of Columbia") |> 
  pivot_wider(names_from = exc_typ, values_from = exc_r) %>% 
  ggscatter(x = "exc_pre_r", y = "exc_r", 
            add = "reg.line", conf.int = TRUE, 
            cor.coef = TRUE, cor.method = "pearson",
            xlab = "Inequality excess", ylab = "Pandemic excess") 

ggsave("figures/scatter_plot_prepand_pand_excess.png",
       w = 5,
       h = 5)

# without DC
exc_pre3 %>% 
  filter(state != "District of Columbia") |>
  pivot_wider(names_from = exc_typ, values_from = exc_r) %>% 
  ggscatter(x = "exc_pre_r", y = "exc_r", 
            add = "reg.line", conf.int = TRUE, 
            cor.coef = TRUE, cor.method = "pearson",
            xlab = "Inequality excess", ylab = "Pandemic excess") 

ggsave("figures/scatter_plot_prepand_pand_excess_noDC.png",
       w = 5,
       h = 5)

tt <- 
  exc_pre3 %>% 
  spread(exc_typ, exc_r) %>% 
  mutate(ratio = exc_pre_r/exc_r)

tt %>% 
  summarise(r_av = mean(ratio))


# P-scores before and during the pandemic
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
pscs %>% 
  # mutate(exc_typ = factor(exc_typ, levels = c("psc_pre", "psc_pan"))) %>% 
  ggplot() + 
  # geom_bar(position="stack", stat="identity")+
  geom_point(aes(x = psc, y = reorder(state, rnk), col = exc_typ))+
  # scale_color_manual(values = cols, labels = c("Pandemic-free excess",
  #                                             "Pandemic excesss"))+
  # coord_flip()+
  # labs(y = "Age-standardized excess death rates (/100K)")+
  theme_bw()+
  theme(legend.position = c(.75, .1),
        legend.background = element_blank(),
        legend.text = element_text(size = 11),
        legend.title = element_blank(),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_blank())



# relative difference with the average before and during the pandemic
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


r <- 
  exc_pre2 %>% 
  select(state, dx_std, bsn_std, exc_std, exposure) %>% 
  mutate(mx = 1e5*dx_std/exposure,
         bsn_r = 1e5*bsn_std/exposure,
         exc_r = 1e5*exc_std/exposure,
         mx_av = mean(mx),
         exc_av = mean(exc_r),
         bsn_r_av = mean(bsn_r),
         rr_pre = bsn_r/bsn_r_av,
         rr_pan = mx/mx_av,
         rr_exc = exc_r/exc_av) %>% 
  left_join(pol2) %>% 
  left_join(codes)

r %>% 
  filter(state != "District of Columbia") %>% 
  ggscatter(x = "rr_pre", y = "rr_exc", 
            add = "reg.line", conf.int = TRUE, 
            cor.coef = TRUE, cor.method = "pearson",
            xlab = "Pre-pandemic relative risk", ylab = "Pandemic relative risk") 

r %>% 
  ggscatter(x = "rr_pre", y = "rr_pan", 
            add = "reg.line", conf.int = TRUE, 
            cor.coef = TRUE, cor.method = "pearson",
            xlab = "Pre-pandemic relative risk", ylab = "Pandemic relative risk") 


r %>% 
  filter(state != "District of Columbia") %>% 
  ggscatter(x = "rr_pre", y = "rr_pan", 
            add = "reg.line", conf.int = TRUE, 
            cor.coef = TRUE, cor.method = "pearson",
            xlab = "Pre-pandemic relative risk", ylab = "Pandemic relative risk") 


r %>% 
  filter(state != "District of Columbia") %>% 
  ggscatter(x = "rr_pre", y = "rr_pan",
            fill = "pol",
            add = "reg.line", conf.int = TRUE, 
            cor.coef = TRUE, cor.method = "pearson",
            xlab = "Pre-pandemic relative risk", ylab = "Pandemic relative risk",
            palette = c("black", "red", "blue", "purple")) 

tt <- 
  r %>% 
  mutate(governor = if_else(pol <= 2, "#3157d4", "#db2a2a"),
         governor = if_else(state == "Nebraska", "#db2a2a", governor),
         governor = if_else(state == "District of Columbia", "#3157d4",governor),
         chamber = case_when(pol == 1 ~ "#3157d450",
                             pol == 2 ~ "#a618c950",
                             pol == 3 ~ "#a618c950",
                             pol == 4 ~ "#db2a2a50",
                             TRUE ~ "#a618c950"),
         gov = ifelse(pol <= 2, "dem","rep"),
         gov = case_when(state == "Nebraska" ~ "rep",
                         state == "District of Columbia" ~ "dem",
                         TRUE ~ gov))

cols <- c("#3157d4", "#db2a2a")

tt %>% 
  filter(state != "District of Columbia") %>% 
  ggplot(aes(rr_pre, rr_pan)) +
  # stat_summary(fun.data= mean_cl_normal) + 
  # geom_smooth(method='lm', col = "black", fill = "transparent")+
  geom_segment(x = 0.7, xend = 1.3, y = 0.7, yend = 1.3)+
  coord_fixed()+
  # scale_fill_manual(values = cols)+
  # scale_color_manual(values = cols)+
  scale_color_identity() +
  scale_fill_identity()+
  geom_text(aes(label = state_code), size = 4)+
  geom_point(aes(color = governor, fill = chamber), 
             size = 6, 
             pch = 21,
             stroke = 2,
             alpha = 0.8)+
  geom_text(aes(label = state_code))+
  geom_vline(xintercept = 1, lty = "dashed")+
  geom_hline(yintercept = 1, lty = "dashed")+
  scale_x_continuous(breaks = seq(0, 2, .1))+
  scale_y_continuous(breaks = seq(0, 2, .1))+
  labs(x = "Pre-pandemic relative risk", y = "Pandemic relative risk")+
  theme_bw()+
  theme(axis.text = element_text(size = 15),
        axis.title = element_text(size = 20),
  )

# ggsave("figures/fake_mess.png",
#        w = 10, h = 10)
# 


# without age-standardization
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~
# inequality- and pandemic-relaetd excess during the pandemic
bst_pan <- 
  exc2 %>% 
  group_by(age) %>% 
  filter(bsn_r == min(bsn_r)) %>% 
  arrange(age) %>% 
  ungroup()

# inq <- 
#   exc2 %>% 
#   left_join(bst_pan %>% 
#               select(age, bsn_r_bst = bsn_r),
#             by = join_by(age)) %>% 
#   mutate(exc_inq_r = bsn_r - bsn_r_bst,
#          exc_inq = exc_inq_r*exposure/1e5)
# 
# 
# inq %>% 
#   ungroup() %>% 
#   summarise(exc = sum(exc),
#             exc_inq = sum(exc_inq)) %>% 
#   mutate(ratio = exc_inq/exc,
#          prop_pan = exc/(exc+exc_inq))
# 
# # 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
