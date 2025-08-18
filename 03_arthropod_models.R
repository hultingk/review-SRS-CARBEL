# loading libraries
librarian::shelf(tidyverse, glmmTMB, emmeans, DHARMa, car)

# loading data
arthropods <- read.csv(file = file.path("data", "L1", "arthropods.csv"))
seed <- read.csv(file = file.path("data", "L1", "seed.csv"))

# pollinator
m_pollinator <- glmmTMB(pollinator ~ patch_type * edge_type + log_floral_abundance + focal_count + spider + florivore + (1|block) + (1|sampling_round), 
             family = "poisson",
             data = arthropods)
summary(m_pollinator)
Anova(m_pollinator)
plot(simulateResiduals(m_pollinator))

# pairwise comparisons
pairs(emmeans(m_pollinator, ~ patch_type*edge_type), simple = "patch_type")
pairs(emmeans(m_pollinator, ~ patch_type*edge_type), simple = "edge_type")

# spider
m_spider <- glmmTMB(spider ~ patch_type * edge_type + log_floral_abundance + focal_count + florivore + (1|block) + (1|sampling_round), 
                        family = "nbinom2",
                        data = arthropods)
summary(m_spider)
Anova(m_spider)
plot(simulateResiduals(m_spider))

# pairwise comparisons
pairs(emmeans(m_spider, ~ patch_type*edge_type), simple = "patch_type")
pairs(emmeans(m_spider, ~ patch_type*edge_type), simple = "edge_type")


# florivore
m_florivore <- glmmTMB(florivore ~ patch_type * edge_type + log_floral_abundance + focal_count + (1|block) + (1|sampling_round), 
                    family = "nbinom2",
                    data = arthropods)
summary(m_florivore)
Anova(m_florivore)
plot(simulateResiduals(m_florivore))

# pairwise comparisons
pairs(emmeans(m_florivore, ~ patch_type*edge_type), simple = "patch_type")
pairs(emmeans(m_florivore, ~ patch_type*edge_type), simple = "edge_type")


# fruit:flower ratio
m_seed <- glmmTMB(pollination_rate ~ patch_type * edge_type + avg_focal_carbel + patch_carbel + avg_pollinator + avg_spider + avg_florivore + (1|block) + (1|plant_ID), 
                  data = seed,
                  family = "betabinomial",
                  weights = total_seeds)
summary(m_seed)
Anova(m_seed)
plot(simulateResiduals(m_seed))

# pairwise comparisons
pairs(emmeans(m_seed, ~ patch_type*edge_type), simple = "patch_type")
pairs(emmeans(m_seed, ~ patch_type*edge_type), simple = "edge_type")

