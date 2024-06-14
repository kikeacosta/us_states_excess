

source("code/02_age_standardization_excess_shortfall.R")
library("ggpubr")
# Letter figure 1
# ranked by pandemic excess, stacked, fill to governor party
exc_pre3 %>% 
  left_join(pol2, by = join_by(state)) |> 
  mutate(exc_typ = if_else(exc_typ == "exc_pre_r", "baseline shortfall","pandemic excess"),
         gov = if_else(gov == "dem","democrat","republican")) |> 
  ggplot(aes(x=exc_r, y=reorder(state, rnk), fill = gov,alpha=exc_typ)) + 
  geom_bar(position="stack", stat="identity",
           col = "grey30",
           width = .8)+
  scale_fill_manual(values = c("#1212bb","#bb1212")) +
  scale_alpha_manual(values = c(.5,1))+
  labs(x = "Age-standardized death rates (/100K)",
       fill = "governor party",
       alpha = "excess type")+
  theme_bw() +
  theme(legend.position = c(.75, .3),
        legend.background = element_blank(),
        legend.text = element_text(size = 11),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 11),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_blank())

ggsave("figures/excess_pre_and_pandemic.png",
       w = 6,
       h = 7.5)

# A second version of the main figure, where we sort on total excess:
# ranked to overall excess; forgot to add in national total with this one
exc_pre3 %>% 
  left_join(pol2,by=join_by(state)) |> 
  mutate(exc_typ = if_else(exc_typ == "exc_pre_r", "baseline shortfall","pandemic excess"),
         gov = if_else(gov == "dem","democrat","republican")) |> 
  ggplot(aes(x=exc_r, y=reorder(state, exc_r), fill = gov,alpha=exc_typ)) + 
  geom_bar(position="stack", stat="identity",
           col = "grey30",
           width = .8)+
  scale_fill_manual(values = c("#1212bb","#bb1212")) +
  scale_alpha_manual(values = c(.5,1))+
  labs(x = "Age-standardized death rates (/100K)",
       fill = "governor party",
       alpha = "excess type")+
  theme_bw() +
  theme(legend.position = c(.75, .3),
        legend.background = element_blank(),
        legend.text = element_text(size = 11),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 11),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_blank())

ggsave("figures/excess_pre_and_pandemic2.png",
       w = 6,
       h = 7.5)


# Supplementary Figures, for the inquisitive reader

# P-score by state
pscs %>% 
  filter(exc_typ == "psc_pan") %>% 
  ggplot() + 
  # geom_bar(position="dodge", stat="identity", fill = "black")+
  # scale_fill_manual(values = cols)+
  geom_point(aes(x=psc, y=reorder(state, rnk)))+
  coord_cartesian(xlim = c(0, .26))+
  labs(x = "P-score (excess / baseline)")+
  theme_bw()+
  theme(axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_blank())

# -------------------------------------------------------
# some plots comparing crude and age-standardized state 
# rankings over full cumulative period
# 1) excess rates
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

# 2) P-scores
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
  theme_minimal() +
  geom_text(mapping = aes(x=age+ageint/2),nudge_y =.1) +
  guides(color = "none") +
  labs(x = "age",
       y = "best practice mortality rate", 
       title = "state composition of best practice mortality schedule")

# --------------------------------------
# Age-standardized excess death rates
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

# -------------------------------

# compare
exc_pre3 %>% 
  pivot_wider(names_from = exc_typ, values_from = exc_r) %>% 
  left_join(pol2, by = join_by(state)) |> 
  left_join(codes, by = join_by(state)) |> 
  #filter(state_code != "DC") |> 
  ggplot(aes(exc_pre_r, exc_r, color = gov, label = state_code))+
  geom_text()+
  theme_bw() +
  labs(y = "Ppandemic excess",
       x = "Baseline shortfall") +
  scale_color_manual(values = c("#bb1212","#1212bb")) +
  geom_smooth(method = "lm")


exc_pre3 %>% 
  pivot_wider(names_from = exc_typ, values_from = exc_r) %>% 
  ggscatter(x = "exc_pre_r", y = "exc_r", 
            add = "reg.line", conf.int = TRUE, 
            cor.coef = TRUE, cor.method = "pearson",
            xlab = "Baseline shortfall", ylab = "Pandemic excess") 

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
            xlab = "Baseline shortfall", ylab = "Pandemic excess") 

ggsave("figures/scatter_plot_prepand_pand_excess_noDC.png",
       w = 5,
       h = 5)


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

r %>% 
  filter(state != "District of Columbia") %>% 
  ggscatter(x = "rr_pre", y = "rr_exc", 
            add = "reg.line", conf.int = TRUE, 
            cor.coef = TRUE, cor.method = "pearson",
            xlab = "Baseline relative risk", ylab = "Pandemic relative risk") 


# wow, now that's a straight line
r %>% 
  ggscatter(x = "rr_pre", y = "rr_pan", 
            add = "reg.line", conf.int = TRUE, 
            cor.coef = TRUE, cor.method = "pearson",
            xlab = "Baseline relative risk", ylab = "Pandemic relative risk") 

# still straight after we remove DC
r %>% 
  filter(state != "District of Columbia") %>% 
  ggscatter(x = "rr_pre", y = "rr_pan", 
            add = "reg.line", conf.int = TRUE, 
            cor.coef = TRUE, cor.method = "pearson",
            xlab = "Baseline relative risk", ylab = "Pandemic relative risk") 

# relationship holds by detailed party composition too 
# not sure how to make all Rsq show up, but lines at 
# least quite parallel.
r %>% 
  filter(state != "District of Columbia") %>% 
  mutate(pol = as.factor(pol)) |> 
  ggscatter(x = "rr_pre", y = "rr_pan",
            fill = "pol",
            add = "reg.line", conf.int = TRUE, 
            cor.coef = TRUE, cor.method = "pearson",
            xlab = "Baseline relative risk", ylab = "Pandemic relative risk",
            palette = c("black", "red", "blue", "purple")) 


