# loading libraries
librarian::shelf(tidyverse, vegan)

##### LOAD DATA ####
visitation <- read.csv(file = file.path("data", "L0", "CARBEL-arthropods.csv")) # arthropod visitation data
patch_type <- read.csv(file = file.path("data", "L0", "Patch_type.csv")) # patch type data
floral <- read.csv(file = file.path("data", "L0", "CARBEL-floral.csv")) # floral data
seed <- read.csv(file = file.path("data", "L0", "CARBEL-seeds.csv")) # seed data

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

# converting 0s to NA for spider and florivore 1st samping round
arthropods <- arthropods %>%
  mutate(spider = if_else(sampling_round == 1, NA, spider)) %>%
  mutate(florivore = if_else(sampling_round == 1, NA, florivore))

# renaming columns 
arthropods <- arthropods %>%
  rename(pollinator = pollinator_visits,
         patch_type = Type)

# removing first sampling round for florivore and spider analysis - not collected first sampling round
arthropods.no_round1 <- arthropods %>%
  filter(sampling_round != 1)

# writing cleaned file
#write_csv(arthropods, file = file.path("data", "L1", "arthropods.csv"))
#write_csv(arthropods.no_round1, file = file.path("data", "L1", "arthropods_noRound1.csv"))



##### FRUIT-FLOWER RATIO DATA WRANGLING #####
seed <- seed %>%
  mutate(pollinated_seeds = viable + no_predation) %>% # calculating # of pollinated seeds as viable seeds + predated seeds
  mutate(total_seeds = pollinated_seeds + nonviable) %>% # calculating total # of seed structures on inflorescence
  mutate(pollination_rate = pollinated_seeds/total_seeds) %>% # calculating pollination rate as fruit-flower ratio
  filter(plant_ID != "54S.C.I.1") # not in pollinator/floral data - exclude from analysis

# joining pollination data to site data
seed <- seed %>% 
  left_join(patch_type, by = c("block" = "Block",
                               "patch" = "Patch"))

# cleaning distances from edge - categorizing into their 4 distances
seed$distance <- str_replace(seed$distance, "~1-2", "2")
seed$distance <- str_replace(seed$distance, "1RECRUIT", "1")
seed$distance <- str_replace(seed$distance, "2RECRUIT", "2")
seed$distance <- str_replace(seed$distance, "3RECRUIT", "3")
seed$distance <- str_replace(seed$distance, "~14m", "2")
# classifying distance from edge as edge or interior
seed <- seed %>% 
  mutate(edge_type = if_else(distance %in% c("0", "1"), "edge", "interior"))


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


# creating average arthropod visitation variables




