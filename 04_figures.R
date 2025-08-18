# loading libraries
librarian::shelf(tidyverse, ggeffects)

source(here::here("03_arthropod_models.R"))

# pollinator figure
predict_pollinator <- ggpredict(m_pollinator, terms=c("patch_type [all]", "edge_type [all]"), back_transform = T)
figure2a <- predict_pollinator %>% 
  ggplot(aes(x = x, y = predicted, color = group)) +
  geom_jitter(aes(x = patch_type, y = pollinator, color = edge_type), data = arthropods, 
              alpha = 0.3, size = 7, position = position_jitterdodge(jitter.width = 0.1,
                                                                     jitter.height = 0.2))+ 
  geom_point(size = 12, position = position_dodge(width = 0.7))+ 
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0, linewidth = 5, position = position_dodge(width = 0.7)) +
  theme_bw(base_size = 30)+
  scale_color_manual(values = c("#A95E35", "#DCB254"), name = "Edge Distance", labels = c("Edge", "Interior")) +
  labs(title = NULL,
       x = NULL,
       y = "Pollinator Visits") +
  theme(legend.text = element_text(size = 24)) +
  theme(legend.title = element_text(size = 26)) #+
#theme(panel.border = element_rect(colour = "black", fill=NA, linewidth=1.5)) +
# ylim(-0.1, 3.3)
figure2a





# spider figure
predict_spider <- ggpredict(m_spider, terms=c("patch_type [all]", "edge_type [all]"), back_transform = T)
figure2b <- predict_spider %>% 
  ggplot(aes(x = x, y = predicted, color = group)) +
  geom_jitter(aes(x = patch_type, y = spider, color = edge_type), data = arthropods, 
              alpha = 0.3, size = 7, position = position_jitterdodge(jitter.width = 0.1,
                                                                     jitter.height = 0.2))+ 
  geom_point(size = 12, position = position_dodge(width = 0.7))+ 
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0, linewidth = 5, position = position_dodge(width = 0.7)) +
  theme_bw(base_size = 30)+
  scale_color_manual(values = c("#A95E35", "#DCB254"), name = "Edge Distance", labels = c("Edge", "Interior")) +
  labs(title = NULL,
       x = NULL,
       y = "Spider Visits") +
  theme(legend.text = element_text(size = 24)) +
  theme(legend.title = element_text(size = 26)) #+
#theme(panel.border = element_rect(colour = "black", fill=NA, linewidth=1.5)) +
# ylim(-0.1, 3.3)
figure2b



# florivore figure
predict_florivore <- ggpredict(m_florivore, terms=c("patch_type [all]", "edge_type [all]"), back_transform = T)
figure2c <- predict_florivore %>% 
  ggplot(aes(x = x, y = predicted, color = group)) +
  geom_jitter(aes(x = patch_type, y = florivore, color = edge_type), data = arthropods, 
              alpha = 0.3, size = 7, position = position_jitterdodge(jitter.width = 0.1,
                                                                     jitter.height = 0.2))+ 
  geom_point(size = 12, position = position_dodge(width = 0.7))+ 
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0, linewidth = 5, position = position_dodge(width = 0.7)) +
  theme_bw(base_size = 30)+
  scale_color_manual(values = c("#A95E35", "#DCB254"), name = "Edge Distance", labels = c("Edge", "Interior")) +
  labs(title = NULL,
       x = NULL,
       y = "Florivore Visits") +
  theme(legend.text = element_text(size = 24)) +
  theme(legend.title = element_text(size = 26)) #+
#theme(panel.border = element_rect(colour = "black", fill=NA, linewidth=1.5)) +
# ylim(-0.1, 3.3)
figure2c



# fruit-flower figure
predict_seed <- ggpredict(m_seed, terms=c("patch_type [all]", "edge_type [all]"), back_transform = T)
figure3 <- predict_seed %>% 
  ggplot(aes(x = x, y = predicted, color = group)) +
  geom_jitter(aes(x = patch_type, y = pollination_rate, color = edge_type), data = seed, 
              alpha = 0.3, size = 7, position = position_jitterdodge(jitter.width = 0.1,
                                                                     jitter.height = 0))+ 
  geom_point(size = 12, position = position_dodge(width = 0.7))+ 
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0, linewidth = 5, position = position_dodge(width = 0.7)) +
  theme_bw(base_size = 30)+
  scale_color_manual(values = c("#A95E35", "#DCB254"), name = "Edge Distance", labels = c("Edge", "Interior")) +
  labs(title = NULL,
       x = NULL,
       y = "Fruit-Flower Ratio") +
  theme(legend.text = element_text(size = 24)) +
  theme(legend.title = element_text(size = 26)) #+
#theme(panel.border = element_rect(colour = "black", fill=NA, linewidth=1.5)) +
# ylim(-0.1, 3.3)
figure3
