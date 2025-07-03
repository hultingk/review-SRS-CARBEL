# loading libraries
librarian::shelf(tidyverse, dagitty, ggdag)

# loading data
arthropods <- read.csv(file = file.path("data", "L1", "arthropods.csv"))


# DAG for pollinator visits as outcome
DAG_pollinator <- dagify(pollinator ~ patch_type + edge_type + log_floral_abundance + focal_count + florivore + spider,
       log_floral_abundance ~ patch_type + edge_type,
       focal_count ~ patch_type + edge_type,
       spider ~ patch_type + edge_type + log_floral_abundance + focal_count + florivore,
       florivore ~ patch_type + edge_type + log_floral_abundance + focal_count,
       exposure = c("patch_type", "edge_type"),
       outcome = "pollinator")
plot(DAG_pollinator)
# adjustements needed for direct effect of patch type and distance from the edge
adjustmentSets(DAG_pollinator, exposure = c("patch_type", "edge_type"), outcome = "pollinator", effect = "direct")

# plot showing this
ggdag_adjustment_set(DAG_pollinator, 
                     shadow = T, 
                     exposure = c("patch_type", "edge_type"), 
                     outcome = "pollinator", 
                     effect = "direct") +
  theme_dag_blank()



# DAG for spider visits as outcome
DAG_spider <- dagify(pollinator ~ patch_type + edge_type + log_floral_abundance + focal_count + florivore + spider,
                         log_floral_abundance ~ patch_type + edge_type,
                         focal_count ~ patch_type + edge_type,
                         spider ~ patch_type + edge_type + log_floral_abundance + focal_count + florivore,
                         florivore ~ patch_type + edge_type + log_floral_abundance + focal_count,
                         exposure = c("patch_type", "edge_type"),
                         outcome = "spider")
plot(DAG_spider)
# adjustements needed for direct effect of patch type and distance from the edge
adjustmentSets(DAG_spider, exposure = c("patch_type", "edge_type"), outcome = "spider", effect = "direct")

# plot showing this
ggdag_adjustment_set(DAG_spider, 
                     shadow = T, 
                     exposure = c("patch_type", "edge_type"), 
                     outcome = "spider", 
                     effect = "direct") +
  theme_dag_blank()


# DAG for florivore visits as outcome
DAG_florivore <- dagify(pollinator ~ patch_type + edge_type + log_floral_abundance + focal_count + florivore + spider,
                     log_floral_abundance ~ patch_type + edge_type,
                     focal_count ~ patch_type + edge_type,
                     spider ~ patch_type + edge_type + log_floral_abundance + focal_count + florivore,
                     florivore ~ patch_type + edge_type + log_floral_abundance + focal_count,
                     exposure = c("patch_type", "edge_type"),
                     outcome = "florivore")
plot(DAG_florivore)
# adjustements needed for direct effect of patch type and distance from the edge
adjustmentSets(DAG_florivore, exposure = c("patch_type", "edge_type"), outcome = "florivore", effect = "direct")

# plot showing this
ggdag_adjustment_set(DAG_florivore, 
                     shadow = T, 
                     exposure = c("patch_type", "edge_type"), 
                     outcome = "florivore", 
                     effect = "direct") +
  theme_dag_blank()


