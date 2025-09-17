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





#### ARTHROPOD MODELS #####
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
pairs(emmeans(m_florivore, ~ patch_type), simple = "patch_type")
pairs(emmeans(m_florivore, ~ edge_type), simple = "edge_type")
pairs(emmeans(m_florivore, ~ patch_type*edge_type), simple = "patch_type")
pairs(emmeans(m_florivore, ~ patch_type*edge_type), simple = "edge_type")




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


