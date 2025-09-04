# loading libraries
librarian::shelf(tidyverse, glmmTMB, emmeans, DHARMa, car, performance)

source(here::here("01_wrangling.R"))

# loading data
arthropods <- read.csv(file = file.path("data", "L1", "arthropods.csv"))
seed <- read.csv(file = file.path("data", "L1", "seed.csv"))
arthropods$patch_type <- as.factor(arthropods$patch_type)
arthropods$edge_type <- as.factor(arthropods$edge_type)

#### FLORAL ABUNDANCE MODELS ####
# Number of focal plant inforesences
m_focal_count <- glmmTMB(focal_count ~ edge_type + (1|block/plant_ID),
                         family = "nbinom2",
                         data = arthropods)
summary(m_focal_count)
Anova(m_focal_count, type = "III")
plot(simulateResiduals(m_focal_count))
check_overdispersion(m_focal_count)
pairs(emmeans(m_focal_count, ~ edge_type))


# Local community floral abundance 
m_floral_abund <- glmmTMB(log_floral_abundance ~ patch_type * edge_type  + (1|block/plant_ID),
                          family = "gaussian",
                          data = arthropods)
summary(m_floral_abund)
Anova(m_floral_abund, type = "III")
plot(simulateResiduals(m_floral_abund))
# pairwise comparisons
pairs(emmeans(m_floral_abund, ~ patch_type*edge_type), simple = "patch_type")
pairs(emmeans(m_floral_abund, ~ patch_type*edge_type), simple = "edge_type")
pairs(emmeans(m_floral_abund, ~ edge_type), simple = "edge_type")
emmeans(m_floral_abund, ~ edge_type, type = "response")

(5.70-6.56)/6.56 * 100 # -13.39564
6.56 + 1.96 * 0.434



# full patch Carphephorus abundance
# sumarizing by patch - one unique value per patch
patch_carbel_unique <- seed %>%
  group_by(block, patch) %>%
  summarize(patch_carbel = max(patch_carbel)) %>% # only one value per patch - taking the max provides one row per patch
  left_join(patch_type, by = c("block" = "Block", # join patch type info
                               "patch" = "Patch")) %>%
  rename(patch_type = "Type")

# model
m_patch_carbel <- glmmTMB(patch_carbel ~ patch_type + (1|block), 
                          data = patch_carbel_unique,
                          family = "gaussian")
summary(m_patch_carbel)
plot(simulateResiduals(m_patch_carbel))
Anova(m_patch_carbel, type = "III")
# pairwise comparisons
pairs(emmeans(m_patch_carbel, ~ patch_type))





##### ARTHROPOD MODELS #####
# pollinator
m_pollinator <- glmmTMB(pollinator ~ patch_type * edge_type + log_floral_abundance + focal_count + florivore + spider + (1|block/plant_ID), 
                        family = "nbinom2",
                        data = arthropods)
summary(m_pollinator)
Anova(m_pollinator, type = "III")
plot(simulateResiduals(m_pollinator))
check_overdispersion(m_pollinator)

# pairwise comparisons
pairs(emmeans(m_pollinator, ~ patch_type*edge_type), simple = "patch_type")
pairs(emmeans(m_pollinator, ~ patch_type*edge_type), simple = "edge_type")

# calculate percent changes from connected patches
emmeans(m_pollinator, ~ patch_type*edge_type, type = "response")
pairs(emmeans(m_pollinator, ~ patch_type*edge_type), simple = "patch_type")
confint(emmeans(m_pollinator, ~ patch_type*edge_type), calc = c(n = ~.wgt.), type = "response")


(0.2402681-0.6120141)/0.6120141 * 100 # -60.74141 % decrease from connected to rectangular at the edge
0.612 + 1.96 * 0.1690

(exp(0.935)-1) *100 # -60.74141
0.935 + 1.96 * 0.438 # 1.79348
0.935 - 1.96 * 0.438 # 0.07652
(exp(-1.79348)-1)*100 #-83.29664 upper CI
(exp(0.07652)-1)*100 #-7.728974 lower CI


(0.1228243-0.6120141)/0.6120141 * 100 # -79.93113 % decrease from connected to rectangular at the edge
(exp(1.606)-1) *100 # -79.93112
1.606 + 1.96 * 0.638 # 2.85648
1.606 - 1.96 * 0.638 # 0.35552
(exp(-2.85648)-1)*100 #-94.25293 upper CI
(exp(-0.35552)-1)*100 #-29.91911 lower CI


# no significant difference at the interior of the patches


# spider
m_spider <- glmmTMB(spider ~ patch_type * edge_type + log_floral_abundance + focal_count + florivore + (1|block/plant_ID), 
                        family = "nbinom2",
                        data = arthropods)
summary(m_spider)
Anova(m_spider, type = "III")

plot(simulateResiduals(m_spider))
check_overdispersion(m_spider)
# pairwise comparisons
pairs(emmeans(m_spider, ~ patch_type*edge_type), simple = "patch_type")
pairs(emmeans(m_spider, ~ patch_type), simple = "patch_type")
pairs(emmeans(m_spider, ~ patch_type*edge_type), simple = "edge_type")





# florivore
m_florivore <- glmmTMB(florivore ~ patch_type * edge_type + log_floral_abundance + focal_count + (1|block/plant_ID), 
                    family = "nbinom2",
                    data = arthropods)
summary(m_florivore)
Anova(m_florivore, type = "III")
plot(simulateResiduals(m_florivore))
check_overdispersion(m_florivore)

# pairwise comparisons
pairs(emmeans(m_florivore, ~ patch_type*edge_type), simple = "patch_type")
pairs(emmeans(m_florivore, ~ patch_type*edge_type), simple = "edge_type")

emmeans(m_florivore, ~ patch_type*edge_type, type = "response")
pairs(emmeans(m_florivore, ~ edge_type))
(exp(-0.522)-1)*100 # 40.66673% decrease from interior to edge plots
-0.522 + 1.96 * 0.269 # 0.00524
-0.522 - 1.96 * 0.269 # -1.04924
(exp(0.00524)-1)*100 # 0.5253753 upper CI
(exp(-1.04924)-1)*100 # 0.5253753 lower CI



(0.343-0.813)/0.813 * 100 # -57.81058 % decrease from connected to rectangular at the edge
(exp(-1.729)-1) *100 # -82.25382 decrease from connected to rectangular
-1.729 + 1.96 * 0.703 # -0.35112
-1.729 - 1.96 * 0.703 # -3.10688
(exp(-0.35112)-1)*100 # -29.61007 upper CI
(exp(-3.10688)-1)*100 # -95.52597 lower CI


(0.410-1.501)/1.501 * 100 # -72.68488 % decrease from connected to rectangular at the interior
(exp(-1.2974)-1) *100 # -72.67587 decrease from connected to rectangular
-1.2974 + 1.96 * 0.483 # -0.35072
-1.2974 - 1.96 * 0.483 # -2.24408
(exp(-0.35072)-1)*100 # -29.58191 upper CI
(exp(-2.24408)-1)*100 # -89.3975 lower CI


#### POLLINATION ####
# fruit:flower ratio
m_seed <- glmmTMB(pollination_rate ~ edge_type + avg_focal_carbel + patch_carbel + avg_pollinator + avg_florivore + (1|block/plant_ID), 
                  data = seed,
                  family = "betabinomial",
                  weights = total_seeds)
summary(m_seed)
Anova(m_seed, type = "III")
plot(simulateResiduals(m_seed))
check_model(m_seed)


