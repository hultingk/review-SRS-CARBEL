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
  scale_color_manual(values = c("#92C5A6", "#054C31"), name = "Distance from Edge", labels = c("Edge", "Interior")) +
  labs(title = NULL,
       x = NULL,
       y = "Pollinator Visitation Abundance") +
  theme(legend.text = element_text(size = 24)) +
  theme(legend.title = element_text(size = 26))
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
  scale_color_manual(values = c("#92C5A6", "#054C31"), name = "Distance from Edge", labels = c("Edge", "Interior")) +
  labs(title = NULL,
       x = NULL,
       y = "Spider Visitation Abundance") +
  theme(legend.text = element_text(size = 24)) +
  theme(legend.title = element_text(size = 26))
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
  scale_color_manual(values = c("#92C5A6", "#054C31"), name = "Distance from Edge", labels = c("Edge", "Interior")) +
  labs(title = NULL,
       x = "Patch Type",
       y = "Florivore Visitation Abundance") +
  theme(legend.text = element_text(size = 24)) +
  theme(legend.title = element_text(size = 26))
figure2c



# fruit-flower figure
predict_seed <- ggpredict(m_seed, terms=c("edge_type [all]"), back_transform = T)
figure3 <- predict_seed %>% 
  ggplot(aes(x = x, y = predicted)) +
  geom_jitter(aes(x = edge_type, y = pollination_rate), data = seed, 
              alpha = 0.3, size = 7, color = "#054C31", width = 0.15)+ 
  geom_point(size = 12, color = "#054C31")+ 
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0, linewidth = 5, color = "#054C31") +
  theme_bw(base_size = 30)+
  labs(title = NULL,
       x = "Distance from Edge",
       y = "Fruit-Flower Ratio") +
  scale_x_discrete(labels = c('Edge', 'Interior')) +
  theme(legend.text = element_text(size = 24)) +
  theme(legend.title = element_text(size = 26))
figure3




#### EXPORTING ####
pdf(file = "Figure2.pdf", width = 13.5, height = 22)
cowplot::plot_grid(figure2a, figure2b, figure2c,
                   nrow=3, ncol=1, label_x = 0.13, label_y = 0.92, align = "hv")
dev.off()


pdf(file = "Figure3.pdf", width = 10, height = 7)
figure3
dev.off()



### Supplementary floral figures 
# focal plant infloresences
predict_focal <- ggpredict(m_focal_count, terms=c("edge_type [all]"), back_transform = T)
figureS1a <- predict_focal %>% 
  ggplot(aes(x = x, y = predicted)) +
  geom_jitter(aes(x = edge_type, y = focal_count), data = arthropods, 
              alpha = 0.3, size = 7, width = 0.15, color = "#054C31") + 
  geom_point(size = 12, color = "#054C31") + 
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0, linewidth = 5, color = "#054C31") +
  theme_bw(base_size = 30) +
  labs(title = NULL,
       x = "Distance from Edge",
       y = "Number of Inforesences \n(individual-scale)") +
  scale_x_discrete(labels = c('Edge', 'Interior')) +
  theme(legend.text = element_text(size = 24)) +
  theme(legend.title = element_text(size = 26)) 
figureS1a



# local floral abundance
predict_floral_abund <- ggpredict(m_floral_abund, terms=c("patch_type [all]", "edge_type [all]"), back_transform = T)
figureS1b <- predict_floral_abund %>% 
  ggplot(aes(x = x, y = predicted, color = group)) +
  geom_jitter(aes(x = patch_type, y = log_floral_abundance, color = edge_type), data = arthropods, 
              alpha = 0.3, size = 7, position = position_jitterdodge(jitter.width = 0.15,
                                                                     jitter.height = 0))+ 
  geom_point(size = 12, position = position_dodge(width = 0.7))+ 
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0, linewidth = 5, position = position_dodge(width = 0.7)) +
  theme_bw(base_size = 30)+
  scale_color_manual(values = c("#92C5A6", "#054C31"), name = "Distance from Edge", labels = c("Edge", "Interior")) +
  labs(title = NULL,
       x = "Patch Type",
       y = "Log Community Floral \nAbundance (local-scale)") +
  theme(legend.text = element_text(size = 24)) +
  theme(legend.title = element_text(size = 26))
figureS1b



# full patch Carphephorus flowering abundance
predict_patch_carbel <- ggpredict(m_patch_carbel, terms=c("patch_type [all]"), back_transform = T)
figureS1c <- predict_patch_carbel %>% 
  ggplot(aes(x = x, y = predicted)) +
  geom_jitter(aes(x = patch_type, y = log_patch_carbel), data = patch_carbel_unique, 
              alpha = 0.3, size = 7, color = "#054C31", width = 0.1, height = 0)+ 
  geom_point(size = 12, color = "#054C31")+ 
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0, linewidth = 5, color = "#054C31") +
  theme_bw(base_size = 30)+
  labs(title = NULL,
       x = "Patch Type",
       y = expression(
         atop("Log " * italic(Carphephorus) * " Flowering",
           "Abundance (patch-scale)"))) +
  theme(legend.text = element_text(size = 24)) +
  theme(legend.title = element_text(size = 26)) 
figureS1c



pdf(file = "FigureS1.pdf", width = 13.9, height = 22)
cowplot::plot_grid(figureS1a, figureS1b, figureS1c,
                   nrow=3, ncol=1, label_x = 0.13, label_y = 0.92, align = "hv")
dev.off()
