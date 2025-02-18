# Code for manuscript: Habitat fragmentation affects plant-arthropod interactions through connectivity loss and edge effects
# owner: Katherine A. Hulting, hultingk@msu.edu

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
library(vegan)
library(piecewiseSEM)
library(multcompView)


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

#pollinator.rich <- visitation %>%
#  filter(visitor_type == "pollinator") %>%
#  mutate(ID_sampling_round = paste(plant_ID, sampling_round, sep = "-")) %>%
#  count(ID_sampling_round, visitor_species) %>%
#  mutate(n = if_else(n == 0, 0, 1)) %>%
#  pivot_wider(names_from = visitor_species, values_from = n, values_fill = 0) %>%
#  column_to_rownames(var="ID_sampling_round")
#pollinator.rich <- as.data.frame(rowSums(pollinator.rich))
#pollinator.rich <- pollinator.rich %>%
#  rownames_to_column(var="ID_sampling_round") %>%
#  mutate(pollinator.richness = `rowSums(pollinator.rich)`) %>%
#  separate(ID_sampling_round, c("plant_ID", "sampling_round"), sep = "-") %>%
#  mutate(sampling_round = as.numeric(sampling_round)) %>%
#  select(c("plant_ID", "sampling_round", "pollinator.richness"))
#cor(arthropods$pollinator_visits, arthropods$pollinator.richness, method = "spearman")

# calculate # of total arthropods per plant per sampling round, join to other dataframes
arthropods <- visitation %>%
  count(plant_ID, sampling_round) %>% # obtaining one row for each plant per sampling round
  dplyr::select(!c("n")) %>%
  left_join(pollinator, by = c("plant_ID", "sampling_round")) %>%
  left_join(florivore, by = c("plant_ID", "sampling_round")) %>%
  left_join(spider, by = c("plant_ID", "sampling_round")) #%>%
  #left_join(pollinator.rich, by =c("plant_ID", "sampling_round") )

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
##### Floral analysis: supplemental information ######
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
m0.posthoc <- emmeans(m0, ~ Type * edge_type)
pairs(m0.posthoc, simple = "edge_type")
pairs(m0.posthoc)

##### Figure S1: floral plot #####
m0.predict <- ggpredict(m0, terms=c("Type [all]", "edge_type [all]"), back_transform = T)
figureS2 <- m0.predict %>% 
  ggplot() +
  geom_point(aes(x = x, y = predicted, color = group), size = 4.5, data = m0.predict,  position = position_dodge(0.5))+ 
  geom_errorbar(aes(x = x, y = predicted, ymin = conf.low, ymax = conf.high, color = group),data = m0.predict, width = 0.4, linewidth = 2,  position = position_dodge(0.5)) +
  theme_classic() +
  geom_jitter(aes(x = Type, y = log_avg_floral_abund, color = edge_type), data = avg_floral_abundance, alpha = 0.2, size = 3.8, position = position_jitterdodge(jitter.width = 0.1, jitter.height = 0,
                                                                                                                                                                dodge.width = 0.5)) +
  #geom_text(data = m1.stat.test, aes(x = ptype, y = height, label = significance), size = 6) +
  labs(title = NULL,
       x = NULL,
       y = "Log Floral Abundance") +
  scale_color_manual(values = c("blue4", "lightblue3"),
                     labels = c("Edge", "Interior"),
                     name = "Distance from Edge") +
  theme(axis.text = element_text(size = 22)) +
  theme(axis.title = element_text(size = 26)) +
  theme(legend.text = element_text(size = 16)) +
  theme(legend.title = element_text(size = 18)) 
  #theme(panel.border = element_rect(colour = "black", fill=NA, linewidth=1.5)) +
svglite(file = "figureS2.svg", width = 9.5, height = 6)
plot(figureS2)
dev.off()
  

figureS1 <- avg_floral_abundance %>%
  ggplot() +
  geom_boxplot(aes(Type, log_avg_floral_abund, fill = edge_type)) +
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


######## Arthropod correlations: Table S1 ########
# checking for relationships between arthropod groups
#m6 <- glmmTMB(pollinator_visits ~ s.log_floral_abundance + s.focal_count + florivore + spider + (1|block/patch/corner) + (1|sampling_round), 
#              data = arthropods.no_round1,
#              family = poisson())
#summary(m6)
#plot(simulateResiduals(m6))
#check_overdispersion(m6)
#check_zeroinflation(m6)
#Anova(m6)

#m7 <- glmmTMB(florivore ~ s.log_floral_abundance + s.focal_count + pollinator_visits + spider + (1|block/patch/corner) + (1|sampling_round), 
#              data = arthropods.no_round1,
#              family = nbinom2())
#summary(m7)
#plot(simulateResiduals(m7))
#check_overdispersion(m7)
#check_zeroinflation(m7)
#Anova(m7)

#m8 <- glmmTMB(spider ~ s.log_floral_abundance + s.focal_count + pollinator_visits + florivore + (1|block/patch/corner) + (1|sampling_round), 
#              data = arthropods.no_round1,
#              family = nbinom2())
#summary(m8)
#plot(simulateResiduals(m8))
#check_overdispersion(m8)
#check_zeroinflation(m8)
#Anova(m8)




##### Pollinator analysis: Figure 2a, Table S2, S3 #####
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
(exp(-0.86975)-1) *100 # rectangular patches have a 58% decrease in pollinator visitation
(exp(-1.16974)-1) *100 # winged patches have a 69% decrease in pollinator visitation

(exp(0.30640)-1) *100 # rectangular patch SE 36%
(exp(0.30318)-1) *100 # winged patch SE 35%

##### Figure 2a: Pollinator plot #####
# connectivity ~ pollinator visitation plot
m1.stat.test <- tibble::tribble(
  ~group1, ~group2,   ~p.adj,
  "Connected",     "Rectangular", "*0.01",
  "Connected",     "Winged", "**0.001",
  "Rectangular",     "Winged", "n.s."
)
predictm1 <- ggpredict(m1, terms=c("Type [all]"), back_transform = T)
figure2a <- predictm1 %>% ggplot(aes(x = x, y = predicted)) +
  geom_jitter(aes(x = Type, y = pollinator_visits), data = arthropods, alpha = 0.1, width = 0.1, height = 0.3, size = 7)+ 
  geom_point(size = 12)+ 
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0, linewidth = 5) +
  theme_bw()+
  #geom_text(data = m1.stat.test, aes(x = ptype, y = height, label = significance), size = 6) +
 # stat_pvalue_manual(
  #  m1.stat.test, 
  #  size = 9,
  #  bracket.size = 1.5,
  #  y.position = 2.4, step.increase = 0.12,
  #  label = "p.adj"
 # ) +
  labs(title = NULL,
       x = NULL,
       y = "Pollinator Visits") +
  theme(axis.text = element_text(size = 30)) +
  theme(axis.title = element_text(size = 34)) #+
  #theme(panel.border = element_rect(colour = "black", fill=NA, linewidth=1.5)) +
 # ylim(-0.1, 3.3)
figure2a


##### Florivore analysis: Figure 2b, Table S2, S3 #####
m2 <- glmmTMB(florivore ~ Type + edge_type + s.log_floral_abundance + s.focal_count + (1|block/patch/corner) + (1|sampling_round), 
              data = arthropods.no_round1,
              family = "nbinom2")
summary(m2)
# model checking
plot(simulateResiduals(m2))
check_overdispersion(m2) # overdispersed, good evidence for negative binomial
check_zeroinflation(m2) # not zero inflated
# posthoc
Anova(m2, type = "III")
m2.posthoc <- emmeans(m2, "Type")
pairs(m2.posthoc)
(exp(1.14602)-1) *100 # rectangular patches have a 215% increase in florivores from connected patches
(exp(0.51873)-1) *100 # interior plots have about a 68% increase in florivores compared to interior plots

(exp(0.41276)-1) *100 # rectangular patch SE

##### Figure 2b: Florivore plot #####
# florivore ~ connectivity plot
m2.stat.test <- tibble::tribble(
  ~group1, ~group2,   ~p.adj,
  "Connected",     "Rectangular", "*0.02",
  "Connected",     "Winged", "n.s.",
  "Rectangular",     "Winged", "*0.02"
)
predictm2 <- ggpredict(m2, terms=c("Type [all]"), back_transform = T)
figure2b <- predictm2 %>% ggplot(aes(x = x, y = predicted)) +
  geom_jitter(aes(x = Type, y = florivore), data = arthropods.no_round1, alpha = 0.1, width = 0.1, height = 0.3, size = 7)+ 
  geom_point(size = 12)+ 
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0, linewidth = 5) +
  theme_bw()+
  #stat_pvalue_manual(
 #   m2.stat.test, 
  #  size = 9,
  #  bracket.size = 1.5,
  #  y.position = 9.6, step.increase = 0.12,
  #  label = "p.adj"
  #) +
  labs(title = NULL,
       x = NULL,
       y = "Florivore Visits") +
  theme(axis.text = element_text(size = 30)) +
  theme(axis.title = element_text(size = 34))# +
  #theme(panel.border = element_rect(colour = "black", fill=NA, linewidth=1.5)) +
 # ylim(-0.1, 12.3)

figure2b



# florivore ~ edge
#m2b.stat.test <- tibble::tribble(
#  ~group1, ~group2,   ~p.adj,
#  "edge",     "interior", "0.06",
#)
#predictm2 <- ggpredict(m2, terms=c("edge_type [all]"), back.transform = T, allow.new.levels=TRUE)
#figurem2b <- predictm2 %>% ggplot(aes(x = x, y = predicted)) +
#  geom_jitter(aes(x = edge_type, y = florivore), data = arthropods.no_round1, alpha = 0.1, width = 0.1, height = 0.1, size = 5)+ 
#  geom_point(size = 5)+ 
#  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.15, linewidth = 2.5) +
#  theme_classic()+
#  stat_pvalue_manual(
#    m2b.stat.test, 
#    size = 9,
#    bracket.size = 1.5,
#    y.position = 9.6, step.increase = 0.12,
#    label = "p.adj"
#  ) +
#  labs(title = NULL,
#       x = NULL,
#       y = "Florivore Visits") +
#  theme(axis.text = element_text(size = 26)) +
#  theme(axis.title = element_text(size = 30)) +
  #theme(panel.border = element_rect(colour = "black", fill=NA, linewidth=1.5)) +
#  ylim(-0.1, 12.3)
#figurem2b


##### Spider analysis: Figure 2c, Table S2, S3  #####
m3 <- glmmTMB(spider ~ Type + edge_type + s.focal_count + (1|block/patch/corner) + (1|sampling_round), 
              data = arthropods.no_round1,
              family = "nbinom2")
summary(m3)
# model checking
plot(simulateResiduals(m3))
check_overdispersion(m3) # not overdispersed, good evidence for poisson
check_zeroinflation(m3) # not zero inflated
# posthoc
Anova(m3, type = "III")
m3.posthoc <- emmeans(m3, "Type")
pairs(m3.posthoc)
(exp(-0.9072)-1) *100 # rectangular patches have a 60% decrease in spiders from connected patches

(exp(0.4034)-1) *100 # rectangular patch SE 50%

##### Figure 2c: Spider plot #####
# connectivity, spider visitation
m3.stat.test <- tibble::tribble(
  ~group1, ~group2,   ~p.adj,
  "Connected",     "Rectangular", "0.06",
  "Connected",     "Winged", "n.s.",
  "Rectangular",     "Winged", "n.s."
)
predictm3 <- ggpredict(m3, terms=c("Type [all]"), back_transform = T)
figure2c <- predictm3 %>% ggplot(aes(x = x, y = predicted)) +
  geom_jitter(aes(x = Type, y = spider), data = arthropods.no_round1, alpha = 0.1, width = 0.1, height = 0.3, size = 7)+ 
  geom_point(size = 12)+ 
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0, linewidth = 5) +
  theme_bw()+
 # stat_pvalue_manual(
  #  m3.stat.test, 
  #  size = 9,
  #  bracket.size = 1.5,
  #  y.position = 3.2, step.increase = 0.12,
  #  label = "p.adj"
 # ) +
  labs(title = NULL,
       x = "Patch Type",
       y = "Spider Visits") +
  theme(axis.text = element_text(size = 30)) +
  theme(axis.title = element_text(size = 34)) #+
  #theme(panel.border = element_rect(colour = "black", fill=NA, linewidth=1.5)) +
 # ylim(-0.1, 4.3)
figure2c



##### Figure 2: multipanel plot #####
#pdf(file = "Figure2.pdf", width = 10, height = 20)
#cowplot::plot_grid(figure2a, figure2b, figure2c, labels = c('(a)', '(b)', '(c)'),
#                   label_size =30, nrow=3, ncol=1, label_x = 0.11, label_y = 0.92, align = "hv")
#dev.off()

svglite(file = "Figure2.svg", width = 10, height = 22)
cowplot::plot_grid(figure2a, figure2b, figure2c, labels = c('(a)', '(b)', '(c)'),
                   label_size =30, nrow=3, ncol=1, label_x = 0.11, label_y = 0.92, align = "hv")
dev.off()


#### FRUIT-FLOWER RATIO ANALYSIS: Figure 3, Table S2, S3 ####
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
seed$s.avg_focal_carbel <- as.numeric(scale(seed$avg_focal_carbel))
seed$s.patch_carbel <- as.numeric(scale(seed$patch_carbel))

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
predictm5 <- ggpredict(m5, terms=c("Type [all]"), back_transform = T)
figure3 <- predictm5 %>% ggplot(aes(x = x, y = predicted)) +
  #geom_point(aes(x = s.avg_focal_carbel, y = pollination_rate), data = seed) +
  #geom_line(aes(x = x, y = predicted)) +
  #geom_ribbon(aes(x = x, ymin = conf.low, ymax = conf.high),
  #            alpha = 0.2) +
  geom_jitter(aes(x = Type, y = pollination_rate), data = seed, alpha = 0.1, width = 0.1, height = 0, size = 8)+ 
  geom_point(size = 14)+ 
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0, linewidth = 5) +
  theme_bw()+
  #stat_pvalue_manual(
  #  m5.stat.test, 
  #  size = 9,
  #  bracket.size = 1.5,
  #  y.position = 0.6, step.increase = 0.1,
  #  label = "p.adj"
  #) +
  labs(title = NULL,
       x = "Patch Type",
       y = "Fruit-Flower Ratio") +
  theme(axis.text = element_text(size = 30)) +
  theme(axis.title = element_text(size = 34)) #+
  #theme(panel.border = element_rect(colour = "black", fill=NA, linewidth=1)) +
 # ylim(0, 0.8)
figure3
# exporting
pdf(file = "Figure3.pdf", width = 10, height = 8)
figure3
dev.off()







#### SEM ####
srs_sem <- psem(
  glmmTMB(log_floral_abundance ~ Type + edge_type + (1|block) + (1|sampling_round),
          data = arthropods.no_round1,
          family = "gaussian"),
  glmmTMB(pollinator_visits ~ Type + edge_type + focal_count + log_floral_abundance + (1|block) + (1|sampling_round),
          data = arthropods.no_round1,
          family = "poisson"),
  glmmTMB(florivore ~ Type + edge_type + focal_count + log_floral_abundance + (1|block) + (1|sampling_round),
          data = arthropods.no_round1,
          family = "nbinom2"),
  glmmTMB(spider ~ Type + edge_type + focal_count + log_floral_abundance + (1|block) + (1|sampling_round),
          data = arthropods.no_round1,
          family = "nbinom2"),
  data = arthropods.no_round1
)

srs_sem <- psem(
  glmmTMB(log_floral_abundance ~ patch_type + edge_type + (1|block) + (1|sampling_round),
          data = arthropods.no_round1,
          family = "gaussian"),
  glmmTMB(pollinator ~ patch_type + edge_type + focal_count + log_floral_abundance + (1|block) + (1|sampling_round),
          data = arthropods.no_round1,
          family = "poisson"),
  glmmTMB(florivore ~ patch_type + edge_type + focal_count + log_floral_abundance + (1|block) + (1|sampling_round),
          data = arthropods.no_round1,
          family = "nbinom2"),
  glmmTMB(spider ~ patch_type + edge_type + focal_count + log_floral_abundance + (1|block) + (1|sampling_round),
          data = arthropods.no_round1,
          family = "nbinom2"),
  data = arthropods.no_round1
)
anova(srs_sem)




lapply(srs_sem[-length(srs_sem)],
       emmeans, pairwise ~ Type)

dSep(srs_sem, conserve = TRUE)
fisherC(srs_sem, conserve = TRUE)
coefs(srs_sem, intercepts = T)

summary(srs_sem, conserve = TRUE)
plot(srs_sem)



