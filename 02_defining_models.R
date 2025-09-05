# loading libraries
librarian::shelf(tidyverse, dagitty, ggdag)

# loading data
arthropods <- read.csv(file = file.path("data", "L1", "arthropods.csv"))
seed <- read.csv(file = file.path("data", "L1", "seed.csv"))

# DAG for pollinator visits as outcome
DAG_pollinator <- dagify(
  focal_count ~ edge_type,
  log_floral_abundance ~ edge_type + patch_type,
  florivore ~ patch_type + edge_type + log_floral_abundance + focal_count,
  spider ~ patch_type + edge_type + log_floral_abundance + focal_count + florivore,
  pollinator ~ patch_type + edge_type + log_floral_abundance + focal_count + florivore + spider,
  exposure = c("patch_type", "edge_type"),
  outcome = "pollinator")
plot(DAG_pollinator)
# adjustments needed for direct effect of patch type and distance from the edge
adjustmentSets(DAG_pollinator, exposure = c("patch_type", "edge_type"), outcome = "pollinator", effect = "direct")


# plot showing this
ggdag_adjustment_set(DAG_pollinator, 
                     shadow = T, 
                     exposure = c("patch_type", "edge_type"), 
                     outcome = "pollinator", 
                     effect = "direct") +
  theme_dag_blank()



# DAG for spider visits as outcome
DAG_spider <- dagify(
  focal_count ~ edge_type,
  log_floral_abundance ~ edge_type + patch_type,
  florivore ~ patch_type + edge_type + log_floral_abundance + focal_count,
  spider ~ patch_type + edge_type + log_floral_abundance + focal_count + florivore,
  pollinator ~ patch_type + edge_type + log_floral_abundance + focal_count + florivore + spider,
  exposure = c("patch_type", "edge_type"),
  outcome = "spider")
plot(DAG_spider)
# adjustments needed for direct effect of patch type and distance from the edge
adjustmentSets(DAG_spider, exposure = c("patch_type", "edge_type"), outcome = "spider", effect = "direct")

# plot showing this
ggdag_adjustment_set(DAG_spider, 
                     shadow = T, 
                     exposure = c("patch_type", "edge_type"), 
                     outcome = "spider", 
                     effect = "direct") +
  theme_dag_blank()


# DAG for florivore visits as outcome
DAG_florivore <- dagify(
  focal_count ~ edge_type,
  log_floral_abundance ~ edge_type + patch_type,
  florivore ~ patch_type + edge_type + log_floral_abundance + focal_count,
  spider ~ patch_type + edge_type + log_floral_abundance + focal_count + florivore,
  pollinator ~ patch_type + edge_type + log_floral_abundance + focal_count + florivore + spider,
  exposure = c("patch_type", "edge_type"),
  outcome = "florivore")
plot(DAG_florivore)
# adjustments needed for direct effect of patch type and distance from the edge
adjustmentSets(DAG_florivore, exposure = c("patch_type", "edge_type"), outcome = "florivore", effect = "direct")

# plot showing this
ggdag_adjustment_set(DAG_florivore, 
                     shadow = T, 
                     exposure = c("patch_type", "edge_type"), 
                     outcome = "florivore", 
                     effect = "direct") +
  theme_dag_blank()




# DAG for fruit-flower ratio as outcome
DAG_seed <- dagify(
  focal_count ~ edge_type,
  log_floral_abundance ~ edge_type + patch_type,
  patch_carbel ~ patch_type, 
  florivore ~ patch_type + edge_type + log_floral_abundance + focal_count,
  spider ~ patch_type + edge_type + log_floral_abundance + focal_count + florivore,
  pollinator ~ patch_type + edge_type + log_floral_abundance + focal_count + florivore + spider,
  pollination_rate ~ edge_type + focal_count + patch_carbel + florivore + pollinator,
  exposure = c("edge_type"),
  outcome = "pollination_rate")
plot(DAG_seed)
# adjustements needed for direct effect of patch type and distance from the edge
adjustmentSets(DAG_seed, exposure = c("edge_type"), outcome = "pollination_rate", effect = "direct")



# DAG for number of focal plant infloresences
DAG_focal <- dagify(
  focal_count ~ edge_type,
  log_floral_abundance ~ edge_type + patch_type,
  patch_carbel ~ patch_type, 
  florivore ~ patch_type + edge_type + log_floral_abundance + focal_count,
  spider ~ patch_type + edge_type + log_floral_abundance + focal_count + florivore,
  pollinator ~ patch_type + edge_type + log_floral_abundance + focal_count + florivore + spider,
  pollination_rate ~ edge_type + focal_count + patch_carbel + florivore + pollinator,
  exposure = c("edge_type"),
  outcome = "focal_count")
plot(DAG_focal)
# adjustements needed for direct effect of patch type and distance from the edge
adjustmentSets(DAG_focal, exposure = c("edge_type"), outcome = "focal_count", effect = "direct")



# DAG for local floral abundance
DAG_local <- dagify(
  focal_count ~ edge_type,
  log_floral_abundance ~ edge_type + patch_type,
  patch_carbel ~ patch_type, 
  florivore ~ patch_type + edge_type + log_floral_abundance + focal_count,
  spider ~ patch_type + edge_type + log_floral_abundance + focal_count + florivore,
  pollinator ~ patch_type + edge_type + log_floral_abundance + focal_count + florivore + spider,
  pollination_rate ~ edge_type + focal_count + patch_carbel + florivore + pollinator,
  exposure = c("edge_type", "patch_type"),
  outcome = "log_floral_abundance")
plot(DAG_local)
# adjustements needed for direct effect of patch type and distance from the edge
adjustmentSets(DAG_local, exposure = c("edge_type", "patch_type"), outcome = "log_floral_abundance", effect = "direct")



# DAG for patch carphephorus abundance
DAG_patch_carbel <- dagify(
  focal_count ~ edge_type,
  log_floral_abundance ~ edge_type + patch_type,
  patch_carbel ~ patch_type, 
  florivore ~ patch_type + edge_type + log_floral_abundance + focal_count,
  spider ~ patch_type + edge_type + log_floral_abundance + focal_count + florivore,
  pollinator ~ patch_type + edge_type + log_floral_abundance + focal_count + florivore + spider,
  pollination_rate ~ edge_type + focal_count + patch_carbel + florivore + pollinator,
  exposure = c("patch_type"),
  outcome = "patch_carbel")
plot(DAG_patch_carbel)
# adjustements needed for direct effect of patch type and distance from the edge
adjustmentSets(DAG_patch_carbel, exposure = c("patch_type"), outcome = "patch_carbel", effect = "direct")







