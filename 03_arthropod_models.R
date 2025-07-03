# loading libraries
librarian::shelf(tidyverse, fixest, glmmTMB, emmeans)

# loading data
arthropods <- read.csv(file = file.path("data", "L1", "arthropods.csv"))



# pollinator
m1 <- fepois(pollinator ~ patch_type * edge_type + log_floral_abundance + focal_count + spider + florivore | block + sampling_round, 
             cluster = "plant_ID",
             data = arthropods)
etable(m1)
summary(m1)
# pairwise comparisons
pairs(emmeans(m1, ~ patch_type))


m1 <- glmmTMB(pollinator ~ patch_type * edge_type + log_floral_abundance + focal_count + spider + florivore + (1|block) + (1|sampling_round), 
             family = "poisson",
             data = arthropods)
summary(m1)
# pairwise comparisons
pairs(emmeans(m1, ~ patch_type))


# florivore 
m2 <- fepois(florivore ~ patch_type * edge_type + log_floral_abundance + focal_count | block + sampling_round, 
             cluster = "plant_ID",
             data = arthropods)
etable(m2)
summary(m2)
# pairwise comparisons
pairs(emmeans(m2, ~ patch_type))



# spider 
m3 <- fepois(spider ~ patch_type * edge_type + log_floral_abundance + focal_count + florivore | block + sampling_round, 
             cluster = "plant_ID",
             data = arthropods)
etable(m3)
summary(m3)
# pairwise comparisons
pairs(emmeans(m3, ~ patch_type))
