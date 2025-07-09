# loading libraries
librarian::shelf(tidyverse, glmmTMB, emmeans, DHARMa)

# loading data
arthropods <- read.csv(file = file.path("data", "L1", "arthropods.csv"))



# pollinator
m1b <- glmmTMB(pollinator ~ patch_type * edge_type + log_floral_abundance + focal_count + spider + florivore + (1|block) + (1|sampling_round), 
             family = "poisson",
             data = arthropods)
summary(m1b)
plot(simulateResiduals(m1b))

# pairwise comparisons
pairs(emmeans(m1b, ~ patch_type*edge_type), simple = "patch_type")


