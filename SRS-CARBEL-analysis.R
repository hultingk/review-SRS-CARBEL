setwd("/Users/katherinehulting/Documents/MSU/SRS founder plant interactions/review-SRS-CARBEL")

# load libraries
library(tidyverse)
library(glmmTMB)
library(car)
library(DHARMa)
library(jtools)
library(ggeffects)
library(emmeans)
library(ggpubr)
library(GGally)
library(performance)
library(svglite)

#### LOAD DATA ####
visitation <- read.csv("CARBEL-arthropods.csv") # arthropod visitation data
patch_type <- read.csv("Patch_type.csv") # patch type data
floral <- read.csv("CARBEL-floral.csv") # floral data
seed <- read.csv("CARBEL-seeds.csv") # seed data

#### ARTHROPOD DATA WRANGLING ####
# calculate # of pollinator visits per plant per sampling round
pollinator <- visitation %>%
  filter(visitor_type == "pollinator") %>%
  group_by(plant_ID, sampling_round) %>%
  summarize(pollinator_visits = n())

# calculate # of florivore visits per plant per sampling round
florivore <- visitation %>%
  filter(visitor_type == "florivore") %>%
  group_by(plant_ID, sampling_round) %>%
  summarize(florivore = n())

# calculate # of spider visits per plant per sampling round
spider <- visitation %>%
  filter(visitor_type == "spider") %>%
  group_by(plant_ID, sampling_round) %>%
  summarize(spider = n())

# calculate # of total arthropods per plant per sampling round, join to other dataframes
arthropods <- visitation %>%
  count(plant_ID, sampling_round) %>%
  dplyr::select(!c("n")) %>%
  left_join(pollinator, by = c("plant_ID", "sampling_round")) %>%
  left_join(florivore, by = c("plant_ID", "sampling_round")) %>%
  left_join(spider, by = c("plant_ID", "sampling_round"))

arthropods[is.na(arthropods)] <- 0 # replace NAs with 0s

arthropods <- arthropods %>%
  separate_wider_delim(plant_ID, delim = ".", names = c("block", "patch", "corner", "distance"), cols_remove
                       = FALSE) %>% # separate plant ID components into different columns
  left_join(patch_type, by = c("block" = "Block", # join patch type info
                               "patch" = "Patch"))

## classify distance as edge/interior
arthropods <- arthropods %>%
  mutate(edge_type = if_else(distance %in% c("0", "1", "1RECRUIT"), "edge", "interior"))

arthropods$distance <- str_replace(arthropods$distance, "1-2", "2")
arthropods$distance <- str_replace(arthropods$distance, "1RECRUIT", "1")
arthropods$distance <- str_replace(arthropods$distance, "2RECRUIT", "2")
arthropods$distance <- str_replace(arthropods$distance, "3RECRUIT", "3")
arthropods$distance <- str_replace(arthropods$distance, "~14m", "2")

#### FLORAL DATA WRANGLING ####
# calculating total floral abundance surrounding each focal plant for each sampling round
floral_abundance <- floral %>%
  dplyr::mutate(avg_flowers = base::rowMeans(dplyr::select(floral, avg_1:avg_10), na.rm = TRUE)) %>% # averaging the # of flowers per individual for each row
  dplyr::mutate(floral_resources = avg_flowers*number_individuals) %>% # multiplying avg # of flowers by total # of individuals for estimate of floral resources
  group_by(plant_ID, sampling_round) %>%
  dplyr::summarize(floral_abundance = sum(floral_resources))

# calculating CARBEL floral abundance surrounding each focal plant for each sampling round
carbel_local_abund <- floral %>%
  dplyr::mutate(avg_flowers = base::rowMeans(dplyr::select(floral, avg_1:avg_10), na.rm = TRUE)) %>% # averaging the # of flowers per individual for each row
  dplyr::mutate(floral_resources = avg_flowers*number_individuals) %>% # multiplying avg # of flowers by total # of individuals for estimate of floral resources
  filter(floral_code == "CARBEL") %>%
  group_by(plant_ID, sampling_round) %>%
  dplyr::summarize(carbel_local_abund = sum(floral_resources))

# CARBEL # of inflorescences on focal plant for each sampling round
focal_carbel <- floral %>%
  dplyr::select(plant_ID, sampling_round, focal_count) %>%
  distinct()

# joining to arthropod data
arthropods <- arthropods %>%
  left_join(floral_abundance, by = c("plant_ID", "sampling_round")) %>%
  left_join(focal_carbel, by = c("plant_ID", "sampling_round"))

arthropods$log_floral_abundance <- log(arthropods$floral_abundance) # log transforming floral abundance
arthropods$s.log_floral_abundance <- as.numeric(scale(arthropods$log_floral_abundance)) # scaling and centering
arthropods$s.focal_count <- as.numeric(scale(arthropods$focal_count)) # scaling and centering
 
# removing first sampling round for florivore and spider analysis - not collected first sampling round
arthropods.no_round1 <- arthropods %>%
  filter(sampling_round != 1)

#### ARTHROPOD DATA ANALYSIS ####
##### Floral analysis ######
# only want one measure of local floral abundance per focal plant
avg_floral_abundance <- arthropods %>% # averaging across sampling rounds for 1 measure per focal plant
  group_by(plant_ID, block, patch, corner, distance, edge_type, Type) %>%
  summarise(avg_floral_abund = mean(floral_abundance)) 
avg_floral_abundance$log_avg_floral_abund <- log(avg_floral_abundance$avg_floral_abund) # log transforming

# How does patch type and distance from edge affect floral abundance?
m0 <- glmmTMB(log_avg_floral_abund ~ Type * edge_type + (1|block/patch/corner), 
              data = avg_floral_abundance,
              family = "gaussian")
summary(m0)
plot(simulateResiduals(m0)) # residuals
# posthoc
Anova(m0, type = "III")
m0.posthoc <- emmeans(m0, ~Type * edge_type)
pairs(m0.posthoc, simple = "edge_type")
pairs(m0.posthoc, simple = "Type")



##### Figure S1: floral plot #####
figureS1 <- arthropods %>%
  ggplot() +
  geom_boxplot(aes(Type, log_floral_abundance, fill = edge_type)) +
  theme_classic() +
  labs(title = NULL,
       x = "Patch Type",
       y = "Log Floral Abundance") +
  theme(axis.text = element_text(size = 20)) +
  theme(axis.title = element_text(size = 22)) +
  theme(legend.text = element_text(size = 16)) +
  theme(legend.title = element_text(size = 18)) +
  guides(fill=guide_legend(title="Distance from Edge")) +
  scale_fill_brewer(palette="BuPu")
figureS1
# exporting
svglite(file = "figureS1.svg", width = 9, height = 6)
plot(figureS1)
dev.off()


##### Pollinator analysis #####
m1 <- glmmTMB(pollinator_visits ~ Type + edge_type + s.log_floral_abundance + s.focal_count + (1|block/patch/corner) + (1|sampling_round), 
              data = arthropods,
              family = poisson())
summary(m1)
# model checking
plot(simulateResiduals(m1))
check_overdispersion(m1) # not overdispersed, good evidence for poisson
check_zeroinflation(m1) # not zero inflated
# posthoc
Anova(m1, type = "III")
m1.posthoc <- emmeans(m1, "Type")
pairs(m1.posthoc)
(exp(-0.79108)-1) *100 # rectangular patches have a 54% decrease in pollinator visitation
(exp(-1.05844)-1) *100 # winged patches have a 65% decrease in pollinator visitation


##### Figure 2a: Pollinator plot #####
# connectivity ~ pollinator visitation plot
m1.stat.test <- tibble::tribble(
  ~group1, ~group2,   ~p.adj,
  "Connected",     "Rectangular", "*0.01",
  "Connected",     "Winged", "**0.001",
  "Rectangular",     "Winged", "n.s."
)
predictm1 <- ggpredict(m1, terms=c("Type [all]"), back.transform = T, allow.new.levels=TRUE)
figure2a <- predictm1 %>% ggplot(aes(x = x, y = predicted)) +
  geom_jitter(aes(x = Type, y = pollinator_visits), data = arthropods, alpha = 0.1, width = 0.1, height = 0.1, size = 3.5)+ 
  geom_point(size = 4.5)+ 
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.15, linewidth = 2) +
  theme_classic()+
  #geom_text(data = m1.stat.test, aes(x = ptype, y = height, label = significance), size = 6) +
  stat_pvalue_manual(
    m1.stat.test, 
    size = 7,
    bracket.size = 1,
    y.position = 2.4, step.increase = 0.12,
    label = "p.adj"
  ) +
  labs(title = NULL,
       x = NULL,
       y = "Pollinator Visits") +
  theme(axis.text = element_text(size = 22)) +
  theme(axis.title = element_text(size = 24)) +
  #theme(panel.border = element_rect(colour = "black", fill=NA, linewidth=1.5)) +
  ylim(-0.1, 3.3)
figure2a



##### Florivore analysis #####
m3 <- glmmTMB(florivore ~ Type + edge_type + s.log_floral_abundance + s.focal_count + (1|block/patch/corner) + (1|sampling_round), 
              data = arthropods.no_round1,
              family = "nbinom2")
summary(m3)
# model checking
plot(simulateResiduals(m3))
check_overdispersion(m3) # overdispersed, good evidence for negative binomial
check_zeroinflation(m3) # not zero inflated
# posthoc
Anova(m3, type = "III")
m3.posthoc <- emmeans(m3, "Type")
pairs(m3.posthoc)
(exp(1.13350)-1) *100 # rectangular patches have a 211% increase in florivores from connected patches

##### Figure 2b: Florivore plot #####
# florivore ~ connectivity plot
m3.stat.test <- tibble::tribble(
  ~group1, ~group2,   ~p.adj,
  "Connected",     "Rectangular", "*0.02",
  "Connected",     "Winged", "n.s.",
  "Rectangular",     "Winged", "*0.02"
)
predictm3 <- ggpredict(m3, terms=c("Type [all]"), back.transform = T, allow.new.levels=TRUE)
figure2b <- predictm3 %>% ggplot(aes(x = x, y = predicted)) +
  geom_jitter(aes(x = Type, y = florivore), data = arthropods.no_round1, alpha = 0.1, width = 0.1, height = 0.1, size = 3.5)+ 
  geom_point(size = 4.5)+ 
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.15, linewidth = 2) +
  theme_classic()+
  stat_pvalue_manual(
    m3.stat.test, 
    size = 7,
    bracket.size = 1,
    y.position = 9.6, step.increase = 0.12,
    label = "p.adj"
  ) +
  labs(title = NULL,
       x = NULL,
       y = "Florivore Visits") +
  theme(axis.text = element_text(size = 22)) +
  theme(axis.title = element_text(size = 24)) +
  #theme(panel.border = element_rect(colour = "black", fill=NA, linewidth=1.5)) +
  ylim(-0.1, 12.3)
figure2b



##### Spider analysis #####
m4 <- glmmTMB(spider ~ Type + edge_type + s.focal_count + (1|block/patch/corner) + (1|sampling_round), 
              data = arthropods.no_round1,
              family = "nbinom2")
summary(m4)
# model checking
plot(simulateResiduals(m4))
check_overdispersion(m4) # not overdispersed, good evidence for poisson
check_zeroinflation(m4) # not zero inflated
# posthoc
Anova(m4, type = "III")
m4.posthoc <- emmeans(m4, "Type")
pairs(m4.posthoc)



##### Figure 2c: Spider plot #####
# connectivity, spider visitation
m4.stat.test <- tibble::tribble(
  ~group1, ~group2,   ~p.adj,
  "Connected",     "Rectangular", "0.06",
  "Connected",     "Winged", "n.s.",
  "Rectangular",     "Winged", "n.s."
)
predictm4 <- ggpredict(m4, terms=c("Type [all]"), back.transform = T, allow.new.levels=TRUE)
figure2c <- predictm4 %>% ggplot(aes(x = x, y = predicted)) +
  geom_jitter(aes(x = Type, y = spider), data = arthropods.no_round1, alpha = 0.1, width = 0.1, height = 0.1, size = 3.5)+ 
  geom_point(size = 4.5)+ 
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.15, linewidth = 2) +
  theme_classic()+
  stat_pvalue_manual(
    m4.stat.test, 
    size = 7,
    bracket.size = 1,
    y.position = 3.2, step.increase = 0.12,
    label = "p.adj"
  ) +
  labs(title = NULL,
       x = "Patch Type",
       y = "Spider Visits") +
  theme(axis.text = element_text(size = 22)) +
  theme(axis.title = element_text(size = 24)) +
  #theme(panel.border = element_rect(colour = "black", fill=NA, linewidth=1.5)) +
  ylim(-0.1, 4.3)
figure2c


##### Figure 2: multipanel plot #####
pdf(file = "Figure2.pdf", width = 10, height = 18)
cowplot::plot_grid(figure2a, figure2b, figure2c, labels = c('A', 'B', 'C'),
                   label_size = 28, nrow=3, ncol=1, label_x = 0.15, label_y = 0.9, align = "hv")
dev.off()


#### FRUIT-FLOWER RATIO ANALYSIS ####
##### Fruit-flower ratio data wrangling #####
seed <- seed %>%
  mutate(pollinated_seeds = viable + no_predation) %>% # calculating # of pollinated seeds as viable seeds + predated seeds
  mutate(total_seeds = pollinated_seeds + nonviable) %>% # calculating total # of seed structures on inflorescence
  mutate(pollination_rate = pollinated_seeds/total_seeds) %>% # calculating pollination rate as fruit-flower ratio
  filter(plant_ID != "54S.C.I.1") # not in pollinator/floral data - exclude from analysis

# joining pollination data to site data
seed <- seed %>% 
  left_join(patch_type, by = c("block" = "Block",
                               "patch" = "Patch"))
# classifying distance from edge as edge or interior
seed <- seed %>% 
  mutate(edge_type = if_else(distance %in% c("0", "1", "1RECRUIT"), "edge", "interior"))
seed$distance <- str_replace(seed$distance, "~1-2", "2")
seed$distance <- str_replace(seed$distance, "1RECRUIT", "1")
seed$distance <- str_replace(seed$distance, "2RECRUIT", "2")
seed$distance <- str_replace(seed$distance, "3RECRUIT", "3")
seed$distance <- str_replace(seed$distance, "~14m", "2")

# calculating average # of flowering inflorescences on each focal plant
avg_focal_carbel <- focal_carbel %>% # average # of flowering inflorescences on focal plant across sampling rounds
  group_by(plant_ID) %>%
  mutate(avg_focal_carbel = mean(focal_count)) %>%
  distinct(plant_ID, .keep_all = TRUE) %>% # keeping all columns, one row per plant
  dplyr::select(!c("focal_count", "sampling_round"))

# calculating average local carphephorus flowering abundance across sampling rounds 
avg_carbel_local_abund <- carbel_local_abund %>%
  group_by(plant_ID) %>%
  mutate(avg_carbel_local_abund = mean(carbel_local_abund)) %>%
  distinct(plant_ID, .keep_all = TRUE) %>% # keeping all columns, one row per plant
  dplyr::select(!c("sampling_round","carbel_local_abund"))

# calculating full patch carphephorus flowering
patch_carbel <- avg_focal_carbel %>%
  left_join(avg_carbel_local_abund, by = c("plant_ID")) %>% # joining focal counts to local counts to obtain # per subplot
  mutate(avg_carbel_local_abund = if_else(is.na(avg_carbel_local_abund), 0, avg_carbel_local_abund)) %>% # putting 0s if no local CARBEL
  mutate(total_local_carbel = avg_carbel_local_abund + avg_focal_carbel) %>% # adding focal count to local CARBEL count
  separate_wider_delim(plant_ID, delim = ".", names = c("block", "patch", "corner", "distance"), cols_remove
                       = FALSE) %>% # separate plant ID components into different columns
  mutate(patch_ID = paste(block, patch, sep = ".")) %>%
  group_by(patch_ID) %>% # grouping by patch
  mutate(patch_carbel = sum(total_local_carbel)) %>% # summing # of flowering CARBEL per patch from subplots
  ungroup(patch_ID) %>%
  select(!c("block", "patch", "corner", "distance"))

# joining patch flowering CARBEL to seed data
seed <- seed %>%
  left_join(patch_carbel, by = "plant_ID")

# scaling and centering continuous variables
seed$s.avg_focal_carbel <- scale(as.numeric(seed$avg_focal_carbel))
seed$s.patch_carbel <- scale(as.numeric(seed$patch_carbel))


##### Fruit-flower ratio analysis #####
m5 <- glmmTMB(pollination_rate ~ Type + edge_type + s.avg_focal_carbel + s.patch_carbel + (1|block/patch/corner) + (1|plant_ID), 
              data = seed,
              family = "betabinomial",
              weights = total_seeds)
summary(m5)
Anova(m5)
m5.post <- emmeans(m5, "Type")
pairs(m5.post)
plot(simulateResiduals(m5))

##### Figure 3: Fruit-flower ratio plot #####
m5.stat.test <- tibble::tribble(
  ~group1, ~group2,   ~p.adj,
  "Connected",     "Rectangular", "n.s.",
  "Connected",     "Winged", "n.s.",
  "Rectangular",     "Winged", "n.s."
)
predictm5 <- ggpredict(m5, terms=c("Type [all]"), back.transform = T, allow.new.levels=TRUE)
figure3 <- predictm5 %>% ggplot(aes(x = x, y = predicted)) +
  #geom_point(aes(x = s.avg_focal_carbel, y = pollination_rate), data = seed) +
  #geom_line(aes(x = x, y = predicted)) +
  #geom_ribbon(aes(x = x, ymin = conf.low, ymax = conf.high),
  #            alpha = 0.2) +
  geom_jitter(aes(x = Type, y = pollination_rate), data = seed, alpha = 0.1, width = 0.1, height = 0, size = 3)+ 
  geom_point(size = 4.5)+ 
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.15, linewidth = 2) +
  theme_classic()+
  stat_pvalue_manual(
    m5.stat.test, 
    size = 7,
    bracket.size = 1,
    y.position = 0.6, step.increase = 0.1,
    label = "p.adj"
  ) +
  labs(title = NULL,
       x = "Patch Type",
       y = "Fruit-Flower Ratio") +
  theme(axis.text = element_text(size = 22)) +
  theme(axis.title = element_text(size = 24)) +
  #theme(panel.border = element_rect(colour = "black", fill=NA, linewidth=1)) +
  ylim(0, 0.8)
figure3
# exporting
svglite(file = "figure3.svg", width = 9, height = 6)
plot(figure3)
dev.off()









