# Plotting shoreline change scenarios
# Mental models juxtaposed against technical literature
# Savannah Swinea
# April 14, 2026

library(rstudioapi)
library(readxl)
library(tidyverse)
library(reshape2)
library(ggpubr)

current_path = rstudioapi::getActiveDocumentContext()$path # finds the file path for this R script
setwd(dirname(current_path)) # sets your working directory equal to that file path

presabs <- read_excel(path = './FCM_Concept_PresAbs.xlsx', sheet = 3) # concept presence / absence

#____________________________________________________________________________________________________________
# Mangrove Expansion

mangexp <- read_excel(path = "./Sensitivity_MangMarsh_FullRange.xlsx")

mangexp_ML <- mangexp %>%
  filter(Label == "Marine Life") %>%
  t() %>%
  as.data.frame() %>%
  rownames_to_column(var = "RowNames") %>%
  left_join(presabs, by = c("RowNames" = "Respondent_ID")) %>%
  {
    colnames(.)[1:(ncol(.) - 20)] <- as.character(.[1, 1:(ncol(.) - 20)]) # replace the column names with the first row, except the last 20 columns that
    # already have the concept names
    . 
  } %>%
  filter(MarineLife == 1 & (Mangroves == 1 | SaltMarshes == 1)) %>%
  mutate(across(-1, as.numeric))

mangexp_SP <- mangexp %>%
  filter(Label == "Storm Protection") %>%
  t() %>%
  as.data.frame() %>%
  rownames_to_column(var = "RowNames") %>%
  left_join(presabs, by = c("RowNames" = "Respondent_ID")) %>%
  {
    colnames(.)[1:(ncol(.) - 20)] <- as.character(.[1, 1:(ncol(.) - 20)]) # replace the column names with the first row, except the last 20 columns that
    # already have the concept names
    . 
  } %>%
  filter(StormProtection == 1 & (Mangroves == 1 | SaltMarshes == 1)) %>%
  mutate(across(-1, as.numeric))

mangexp_WQ <- mangexp %>%
  filter(Label == "Water Quality") %>%
  t() %>%
  as.data.frame() %>%
  rownames_to_column(var = "RowNames") %>%
  left_join(presabs, by = c("RowNames" = "Respondent_ID")) %>%
  {
    colnames(.)[1:(ncol(.) - 20)] <- as.character(.[1, 1:(ncol(.) - 20)]) # replace the column names with the first row, except the last 20 columns that
    # already have the concept names
    . 
  } %>%
  filter(WaterQuality == 1 & (Mangroves == 1 | SaltMarshes == 1)) %>%
  mutate(across(-1, as.numeric))

mangexp_dfs <- list(mangexp_ML = mangexp_ML, mangexp_SP = mangexp_SP,
                    mangexp_WQ = mangexp_WQ)

mangexp <- data.frame(Level = names(mangexp_ML)[2:42])

for (i in seq_along(mangexp_dfs)) {
  
  current_df <- as.data.frame(mangexp_dfs[i])
  averages <- colMeans(current_df[, 2:42], na.rm = TRUE)
  
  confidence_intervals <- t(sapply(current_df[, 2:42], function(col) {
    t_test_result <- t.test(col, na.rm = TRUE)
    margin_of_error <- qt(0.975, df = length(col) - 1) * t_test_result$stderr
    
    c(t_test_result$conf.int, margin_of_error)
  }))
  
  margin_of_error <- confidence_intervals[, 3]
  
  df_name <- names(mangexp_dfs)[i]
  
  mangexp <- cbind(mangexp, setNames(data.frame(averages, margin_of_error), c(df_name, paste0(df_name, "_margin_of_error"))))
}

mangexp <- mangexp %>%
  mutate(Level_num = as.numeric(Level))

# Making a duplicate zero so both scenarios "meet" at 0
zero_mang <- mangexp %>%
  filter(Level_num == 0) %>%
  mutate(Scenario = "Mangrove Dominance",
         Understanding = factor(
           "Better Understood",
           levels = c("Poorly Understood", "Somewhat Understood", "Better Understood")
         )
  )

mangexpmarlifeGraph_2 <- mangexp %>%
  mutate(
    Scenario = case_when(
      Level_num <= 0 ~ "Marsh Dominance",
      Level_num > 0  ~ "Mangrove Dominance"
    ),
    Understanding = factor(
      "Better Understood",
      levels = c("Poorly Understood", "Somewhat Understood", "Better Understood")
    )
  ) %>%
  bind_rows(zero_mang) %>%
  ggplot(aes(x = Level_num, y = mangexp_ML, color = Scenario, fill = Scenario, linetype = Understanding)) +
  geom_line(show.legend = TRUE) +
  geom_ribbon(aes(ymin = mangexp_ML - mangexp_ML_margin_of_error,
                  ymax = mangexp_ML + mangexp_ML_margin_of_error), alpha = 0.2, color = NA, show.legend = FALSE) +
  geom_vline(xintercept = 0, color = "grey90", linewidth = 0.5, linetype = "solid") +
  theme_classic() +
  theme(axis.line.x = element_line(arrow = grid::arrow(length = unit(0.3, "cm"), 
                                                       ends = "last")),
        axis.title.x = element_text(angle = 0)) +
  labs(x = "Scenario Intensity",
       y = "Relative Change in Marine Life",
       color = NULL, fill = NULL) +
  scale_x_continuous(labels = function(x) abs(x)) +
  scale_y_continuous(limits = c(-0.32, 0), breaks = seq(-0.3, 0, by = 0.1)) +
  scale_color_manual(name = "Scenario", values = c("Marsh Dominance" = "#1B9E77", "Mangrove Dominance" = "#D95F02")) +
  scale_fill_manual(name = "Scenario", values = c("Marsh Dominance" = "#1B9E77", "Mangrove Dominance" = "#D95F02")) +
  annotate("label", 
           x = -0.85, y = -0.3,     
           label = "Past\nCondition", 
           fill = NA, 
           color = "black", 
           label.size = 0,       
           label.r = unit(0.15, "lines"),  
           size = 3.5) +  
  annotate("label", 
           x = 0.85, y = -0.3,     
           label = "Future\nProjection", 
           fill = NA, 
           color = "black", 
           label.size = 0,       
           label.r = unit(0.15, "lines"),  
           size = 3.5) +
  annotate("label", 
           x = -0.95, y = 0,     
           label = "2a", 
           fill = "white", 
           color = "black", 
           label.size = 0.3,       
           label.r = unit(0.15, "lines"),  
           size = 5,
           fontface = "bold") +
  scale_linetype_manual(name = "Level of Understanding", values = c("Poorly Understood" = "dotted",
                                                                    "Somewhat Understood" = "dashed",
                                                                    "Better Understood" = "solid"),
                        drop = FALSE) +
  guides(
    color = guide_legend(
      order = 1,
      override.aes = list(linewidth = 1.2),
      ncol = 1
    ),
    fill = guide_legend(
      order = 1,
      override.aes = list(alpha = 0.6),
      ncol = 1
    ),
    linetype = guide_legend(
      order = 2,
      override.aes = list(linewidth = 1.2),
      ncol = 1
    )
  ) +
  theme(
    legend.position = "bottom",
    legend.text  = element_text(size = 12),
    legend.title = element_text(size = 14)
  )

# Use if plotting lines on top of each other
levels <- seq(0, 1, by = 0.05)
mangexp_ML_t <- tibble(
  Level = rep(levels, times = 2),
  Scenario = rep(c("Mangrove Dominance", "Marsh Dominance"), each = length(levels)),
  mangexp_ML = c(seq(0.05, 0.05, length.out = length(levels)),
                 seq(-0.05, -0.05, length.out = length(levels))),
  Understanding = factor("Better Understood",
                         levels = c("Poorly Understood", "Somewhat Understood", "Better Understood")))

# Use if plotting lines next to each other
levels_pos <- seq(0, 1, by = 0.05)   # Mangrove Dominance
levels_neg <- seq(-1, 0, by = 0.05)  # Marsh Dominance

mangexp_ML_t <- tibble(
  Level = c(levels_pos, levels_neg),
  Scenario = c(
    rep("Mangrove Dominance", length(levels_pos)),
    rep("Marsh Dominance", length(levels_neg))
  ),
  mangexp_ML = c(
    rep(0.05, length(levels_pos)),
    rep(-0.05, length(levels_neg))
  ),
  Understanding = factor(
    "Better Understood",
    levels = c("Poorly Understood", "Somewhat Understood", "Better Understood")
  )
)

mangexp_ML_t_Graph <- ggplot(mangexp_ML_t, aes(x = Level, y = mangexp_ML, color = Scenario, linetype = Understanding)) +
  geom_line(linewidth = 3, show.legend = TRUE) +
  ylim(-2, 2) +
  theme_classic() +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  labs(x = "Scenario Intensity",
       y = "Relative Change in Marine Life",
       color = NULL,
       linetype = "Level of Understanding") +
  scale_x_continuous(limits = c(0, 1)) +
  scale_color_manual(values = c("Marsh Dominance" = "white",
                                "Mangrove Dominance" = "white")) +
  scale_linetype_manual(values = c("Poorly Understood" = "dotted",
                                   "Somewhat Understood" = "dashed",
                                   "Better Understood" = "solid"),
                        drop = FALSE) +
  annotate("label", 
           x = 0.5, y = 0.5,       
           label = "Species-Specific Response", 
           fill = "white", 
           color = "black", 
           label.size = 0.3,      
           label.r = unit(0.15, "lines"),  
           size = 4) +
  annotate("label", 
           x = 0.025, y = 2,     
           label = "2b", 
           fill = "white", 
           color = "black", 
           label.size = 0.3,       
           label.r = unit(0.15, "lines"),  
           size = 5,
           fontface = "bold") +
  guides(
    color = guide_legend(
      order = 1,
      override.aes = list(linewidth = 1.2),
      ncol = 1
    ),
    fill = guide_legend(
      order = 1,
      override.aes = list(alpha = 0.6),
      ncol = 1
    ),
    linetype = guide_legend(
      order = 2,
      override.aes = list(linewidth = 1.2),
      ncol = 1
    )
  ) +
  theme(
    legend.position = "bottom",
    legend.text  = element_text(size = 12),
    legend.title = element_text(size = 14)
  )

mangexpstormproGraph_2 <- mangexp %>%
  mutate(
    Scenario = case_when(
      Level_num <= 0 ~ "Marsh Dominance",
      Level_num > 0  ~ "Mangrove Dominance"
    ),
    Understanding = factor(
      "Better Understood",
      levels = c("Poorly Understood", "Somewhat Understood", "Better Understood")
    )
  ) %>%
  bind_rows(zero_mang) %>%
  ggplot(aes(x = Level_num, y = mangexp_SP, color = Scenario, fill = Scenario, linetype = Understanding)) +
  geom_line(show.legend = TRUE) +
  geom_ribbon(aes(ymin = mangexp_SP - mangexp_SP_margin_of_error,
                  ymax = mangexp_SP + mangexp_SP_margin_of_error), alpha = 0.2, color = NA, show.legend = FALSE) +
  geom_vline(xintercept = 0, color = "grey90", linewidth = 0.5, linetype = "solid") +
  theme_classic() +
  theme(axis.line.x = element_line(arrow = grid::arrow(length = unit(0.3, "cm"), 
                                                       ends = "last")),
        axis.title.x = element_text(angle = 0)) +
  labs(x = "Scenario Intensity",
       y = "Relative Change in Storm Protection",
       color = NULL, fill = NULL) +
  scale_x_continuous(labels = function(x) abs(x)) +
  scale_y_continuous(limits = c(-0.32, 0), breaks = seq(-0.3, 0, by = 0.1)) +
  scale_color_manual(name = "Scenario", values = c("Marsh Dominance" = "#1B9E77", "Mangrove Dominance" = "#D95F02")) +
  scale_fill_manual(name = "Scenario", values = c("Marsh Dominance" = "#1B9E77", "Mangrove Dominance" = "#D95F02")) +
  annotate("label", 
           x = -0.85, y = -0.3,     
           label = "Past\nCondition", 
           fill = "white", 
           color = "black", 
           label.size = 0,       
           label.r = unit(0.15, "lines"),  
           size = 3.5) +  
  annotate("label", 
           x = 0.85, y = -0.3,     
           label = "Future\nProjection", 
           fill = "white", 
           color = "black", 
           label.size = 0,       
           label.r = unit(0.15, "lines"),  
           size = 3.5) +
  annotate("label", 
           x = -0.95, y = 0,     
           label = "3a", 
           fill = "white", 
           color = "black", 
           label.size = 0.3,       
           label.r = unit(0.15, "lines"),  
           size = 5,
           fontface = "bold") +
  scale_linetype_manual(name = "Level of Understanding", values = c("Poorly Understood" = "dotted",
                                                                    "Somewhat Understood" = "dashed",
                                                                    "Better Understood" = "solid"),
                        drop = FALSE) +
  guides(
    color = guide_legend(
      order = 1,
      override.aes = list(linewidth = 1.2),
      ncol = 1
    ),
    fill = guide_legend(
      order = 1,
      override.aes = list(alpha = 0.6),
      ncol = 1
    ),
    linetype = guide_legend(
      order = 2,
      override.aes = list(linewidth = 1.2),
      ncol = 1
    )
  ) +
  theme(
    legend.position = "bottom",
    legend.text  = element_text(size = 12),
    legend.title = element_text(size = 14)
  )

# Use if plotting lines next to each other
levels_pos <- seq(0, 1, by = 0.05)   # Mangrove Dominance
levels_neg <- seq(-1, 0, by = 0.05)  # Marsh Dominance

mangexp_SP_t <- tibble(
  Level = c(levels_pos, levels_neg),
  Scenario = c(
    rep("Mangrove Dominance", length(levels_pos)),
    rep("Marsh Dominance", length(levels_neg))
  ),
  mangexp_SP = c(seq(0, 0.3, length.out = length(levels_pos)),
                 seq(-0.3, 0, length.out = length(levels_neg))),
  Understanding = factor(
    "Poorly Understood",
    levels = c("Poorly Understood", "Somewhat Understood", "Better Understood")
  )
)

mangexp_SP_t_Graph <- ggplot(mangexp_SP_t, aes(x = Level, y = mangexp_SP, color = Scenario, linetype = Understanding)) +
  geom_line(linewidth = 3, show.legend = TRUE) +
  geom_vline(xintercept = 0, color = "grey90", linewidth = 0.5, linetype = "solid") +
  theme_classic() +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  labs(x = "Scenario Intensity",
       y = "Relative Change in Storm Protection",
       color = NULL,
       linetype = "Level of Understanding") +
  #scale_x_continuous(limits = c(0, 1)) + if plotting lines next to each other
  scale_x_continuous(limits = c(-1, 1)) +
  scale_color_manual(values = c("Marsh Dominance" = "#1B9E77",
                                "Mangrove Dominance" = "#D95F02")) +
  scale_linetype_manual(values = c("Poorly Understood" = "dotted",
                                   "Somewhat Understood" = "dashed",
                                   "Better Understood" = "solid"),
                        drop = FALSE) +
  annotate("label", 
           x = -0.95, y = 0.3,     
           label = "3b", 
           fill = "white", 
           color = "black", 
           label.size = 0.3,       
           label.r = unit(0.15, "lines"),  
           size = 5,
           fontface = "bold") +
  guides(
    color = guide_legend(
      order = 1,
      override.aes = list(linewidth = 1.2),
      ncol = 1
    ),
    fill = guide_legend(
      order = 1,
      override.aes = list(alpha = 0.6),
      ncol = 1
    ),
    linetype = guide_legend(
      order = 2,
      override.aes = list(linewidth = 1.2),
      ncol = 1
    )
  ) +
  theme(
    legend.position = "bottom",
    legend.text  = element_text(size = 12),
    legend.title = element_text(size = 14)
  )

mangexpwaterqualGraph_2 <- mangexp %>%
  mutate(
    Scenario = case_when(
      Level_num <= 0 ~ "Marsh Dominance",
      Level_num > 0  ~ "Mangrove Dominance"
    ),
    Understanding = factor(
      "Better Understood",
      levels = c("Poorly Understood", "Somewhat Understood", "Better Understood")
    )
  ) %>%
  bind_rows(zero_mang) %>%
  ggplot(aes(x = Level_num, y = mangexp_WQ, color = Scenario, fill = Scenario, linetype = Understanding)) +
  geom_line(show.legend = TRUE) +
  geom_ribbon(aes(ymin = mangexp_WQ - mangexp_WQ_margin_of_error,
                  ymax = mangexp_WQ + mangexp_WQ_margin_of_error), alpha = 0.2, color = NA, show.legend = FALSE) +
  geom_vline(xintercept = 0, color = "grey90", linewidth = 0.5, linetype = "solid") +
  theme_classic() +
  theme(axis.line.x = element_line(arrow = grid::arrow(length = unit(0.3, "cm"), 
                                                       ends = "last")),
        axis.title.x = element_text(angle = 0)) +
  labs(title = "Local Knowledge (Mental Models)",
       x = "Scenario Intensity",
       y = "Relative Change in Water Quality",
       color = NULL, fill = NULL) +
  scale_x_continuous(labels = function(x) abs(x)) +
  scale_y_continuous(limits = c(-0.32, 0), breaks = seq(-0.3, 0, by = 0.1)) +
  scale_color_manual(name = "Scenario", values = c("Marsh Dominance" = "#1B9E77", "Mangrove Dominance" = "#D95F02")) +
  scale_fill_manual(name = "Scenario", values = c("Marsh Dominance" = "#1B9E77", "Mangrove Dominance" = "#D95F02")) +
  annotate("label", 
           x = -0.85, y = -0.3,     
           label = "Past\nCondition", 
           fill = "white", 
           color = "black", 
           label.size = 0,       
           label.r = unit(0.15, "lines"),  
           size = 3.5) +  
  annotate("label", 
           x = 0.85, y = -0.3,     
           label = "Future\nProjection", 
           fill = "white", 
           color = "black", 
           label.size = 0,       
           label.r = unit(0.15, "lines"),  
           size = 3.5) +
  annotate("label", 
           x = -0.95, y = 0,     
           label = "1a", 
           fill = "white", 
           color = "black", 
           label.size = 0.3,       
           label.r = unit(0.15, "lines"),  
           size = 5,
           fontface = "bold") +
  scale_linetype_manual(name = "Level of Understanding", values = c("Poorly Understood" = "dotted",
                                                                    "Somewhat Understood" = "dashed",
                                                                    "Better Understood" = "solid"),
                        drop = FALSE) +
  guides(
    color = guide_legend(
      order = 1,
      override.aes = list(linewidth = 1.2),
      ncol = 1
    ),
    fill = guide_legend(
      order = 1,
      override.aes = list(alpha = 0.6),
      ncol = 1
    ),
    linetype = guide_legend(
      order = 2,
      override.aes = list(linewidth = 1.2),
      ncol = 1
    )
  ) +
  theme(
    legend.position = "bottom",
    legend.text  = element_text(size = 12),
    legend.title = element_text(size = 14)
  )

mangexpwaterqualGraph_2

mangexp_WQ_t <- tibble(
  Level = c(levels_pos, levels_neg),
  Scenario = c(
    rep("Mangrove Dominance", length(levels_pos)),
    rep("Marsh Dominance", length(levels_neg))
  ),
  mangexp_WQ = c(
    rep(0.05, length(levels_pos)),
    rep(-0.05, length(levels_neg))
  ),
  Understanding = factor(
    "Better Understood",
    levels = c("Poorly Understood", "Somewhat Understood", "Better Understood")
  )
)

mangexp_WQ_t_Graph <- ggplot(mangexp_WQ_t, aes(x = Level, y = mangexp_WQ, color = Scenario, linetype = Understanding)) +
  geom_line(linewidth = 3, show.legend = TRUE) +
  ylim(-2, 2) +
  theme_classic() +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  labs(title = "Technical Knowledge (Literature)",
       x = "Scenario Intensity",
       y = "Relative Change in Water Quality",
       color = NULL,
       linetype = "Level of Understanding") +
  scale_x_continuous(limits = c(0, 1)) +
  scale_color_manual(values = c("Marsh Dominance" = "white",
                                "Mangrove Dominance" = "white")) +
  scale_linetype_manual(values = c("Poorly Understood" = "dotted",
                                   "Somewhat Understood" = "dashed",
                                   "Better Understood" = "solid"),
                        drop = FALSE) +
  annotate("label", 
           x = 0.5, y = 0.5,       
           label = "Context-Specific Response", 
           fill = "white", 
           color = "black", 
           label.size = 0.3,      
           label.r = unit(0.15, "lines"),  
           size = 4) +
  annotate("label", 
           x = 0.025, y = 2,     
           label = "1b", 
           fill = "white", 
           color = "black", 
           label.size = 0.3,       
           label.r = unit(0.15, "lines"),  
           size = 5,
           fontface = "bold") +
  guides(
    color = guide_legend(
      order = 1,
      override.aes = list(linewidth = 1.2),
      ncol = 1
    ),
    fill = guide_legend(
      order = 1,
      override.aes = list(alpha = 0.6),
      ncol = 1
    ),
    linetype = guide_legend(
      order = 2,
      override.aes = list(linewidth = 1.2),
      ncol = 1
    )
  ) +
  theme(
    legend.position = "bottom",
    legend.text  = element_text(size = 12),
    legend.title = element_text(size = 14)
  )

ggarrange(mangexpwaterqualGraph_2, mangexp_WQ_t_Graph, mangexpmarlifeGraph_2, mangexp_ML_t_Graph, mangexpstormproGraph_2, mangexp_SP_t_Graph, 
          ncol = 2, nrow = 3, 
          common.legend = TRUE, legend = "right"
          # labels = c("1a)", "1b)", "2a)", "2b)", "3a)", "3b)")
)

#____________________________________________________________________________________________________________
# Shoreline Armoring

shorearmor <- read_excel(path = "./Sensitivity_SeawallMar_FullRange.xlsx")

shorearmor_ML <- shorearmor %>%
  filter(Label == "Marine Life") %>%
  t() %>%
  as.data.frame() %>%
  rownames_to_column(var = "RowNames") %>%
  left_join(presabs, by = c("RowNames" = "Respondent_ID")) %>%
  {
    colnames(.)[1:(ncol(.) - 20)] <- as.character(.[1, 1:(ncol(.) - 20)]) # replace the column names with the first row, except the last 20 columns that
    # already have the concept names
    . 
  } %>%
  filter(MarineLife == 1 & (SeawallsandHardenedShorelines == 1 | SaltMarshes == 1)) %>%
  mutate(across(-1, as.numeric))

shorearmor_SP <- shorearmor %>%
  filter(Label == "Storm Protection") %>%
  t() %>%
  as.data.frame() %>%
  rownames_to_column(var = "RowNames") %>%
  left_join(presabs, by = c("RowNames" = "Respondent_ID")) %>%
  {
    colnames(.)[1:(ncol(.) - 20)] <- as.character(.[1, 1:(ncol(.) - 20)]) # replace the column names with the first row, except the last 20 columns that
    # already have the concept names
    . 
  } %>%
  filter(StormProtection == 1 & (SeawallsandHardenedShorelines == 1 | SaltMarshes == 1)) %>%
  mutate(across(-1, as.numeric))

shorearmor_WQ <- shorearmor %>%
  filter(Label == "Water Quality") %>%
  t() %>%
  as.data.frame() %>%
  rownames_to_column(var = "RowNames") %>%
  left_join(presabs, by = c("RowNames" = "Respondent_ID")) %>%
  {
    colnames(.)[1:(ncol(.) - 20)] <- as.character(.[1, 1:(ncol(.) - 20)]) # replace the column names with the first row, except the last 20 columns that
    # already have the concept names
    . 
  } %>%
  filter(WaterQuality == 1 & (SeawallsandHardenedShorelines == 1 | SaltMarshes == 1)) %>%
  mutate(across(-1, as.numeric))

shorearmor_dfs <- list(shorearmor_ML = shorearmor_ML, shorearmor_SP = shorearmor_SP,
                       shorearmor_WQ = shorearmor_WQ)

seawall <- data.frame(Level = names(shorearmor_ML)[2:42])

for (i in seq_along(shorearmor_dfs)) {
  
  current_df <- as.data.frame(shorearmor_dfs[i])
  averages <- colMeans(current_df[, 2:42], na.rm = TRUE)
  
  confidence_intervals <- t(sapply(current_df[, 2:42], function(col) {
    t_test_result <- t.test(col, na.rm = TRUE)
    margin_of_error <- qt(0.975, df = length(col) - 1) * t_test_result$stderr
    
    c(t_test_result$conf.int, margin_of_error)
  }))
  
  margin_of_error <- confidence_intervals[, 3]
  
  df_name <- names(shorearmor_dfs)[i]
  
  seawall <- cbind(seawall, setNames(data.frame(averages, margin_of_error), c(df_name, paste0(df_name, "_margin_of_error"))))
}

seawall <- seawall %>%
  mutate(Level_num = as.numeric(Level))

# Making a duplicate zero so both scenarios "meet" at 0
zero_seawall <- seawall %>%
  filter(Level_num == 0) %>%
  mutate(Scenario = "Hardened Dominance",
         Understanding = factor(
           "Better Understood",
           levels = c("Poorly Understood", "Somewhat Understood", "Better Understood")
         )
  )

shorearmormarlifeGraph_2 <- seawall %>%
  mutate(
    Scenario = case_when(
      Level_num <= 0 ~ "Marsh Dominance",
      Level_num > 0  ~ "Hardened Dominance"
    ),
    Understanding = factor(
      "Better Understood",
      levels = c("Poorly Understood", "Somewhat Understood", "Better Understood")
    )
  ) %>%
  bind_rows(zero_seawall) %>%
  ggplot(aes(x = Level_num, y = shorearmor_ML, color = Scenario, fill = Scenario, linetype = Understanding)) +
  geom_line(show.legend = TRUE) +
  geom_ribbon(aes(ymin = shorearmor_ML - shorearmor_ML_margin_of_error,
                  ymax = shorearmor_ML + shorearmor_ML_margin_of_error), alpha = 0.2, color = NA, show.legend = FALSE) +
  geom_vline(xintercept = 0, color = "grey90", linewidth = 0.5, linetype = "solid") +
  theme_classic() +
  theme(axis.line.x = element_line(arrow = grid::arrow(length = unit(0.3, "cm"), 
                                                       ends = "last")),
        axis.title.x = element_text(angle = 0)) +
  labs(x = "Scenario Intensity",
       y = "Relative Change in Marine Life",
       color = NULL, fill = NULL) +
  scale_x_continuous(labels = function(x) abs(x)) +
  scale_y_continuous(limits = c(-0.33, 0.01), breaks = seq(-0.3, 0, by = 0.1)) +
  scale_color_manual(name = "Scenario", values = c("Marsh Dominance" = "#1B9E77", "Hardened Dominance" = "#7570B3")) +
  scale_fill_manual(name = "Scenario", values = c("Marsh Dominance" = "#1B9E77", "Hardened Dominance" = "#7570B3")) +
  annotate("label", 
           x = -0.85, y = -0.3,     
           label = "Past\nCondition", 
           fill = NA, 
           color = "black", 
           label.size = 0,       
           label.r = unit(0.15, "lines"),  
           size = 3.5) +  
  annotate("label", 
           x = 0.85, y = -0.31,     
           label = "Future\nProjection", 
           fill = NA, 
           color = "black", 
           label.size = 0,       
           label.r = unit(0.15, "lines"),  
           size = 3.5) +
  annotate("label", 
           x = -0.95, y = 0.01,     
           label = "2a", 
           fill = "white", 
           color = "black", 
           label.size = 0.3,       
           label.r = unit(0.15, "lines"),  
           size = 5,
           fontface = "bold") +
  scale_linetype_manual(name = "Level of Understanding", values = c("Poorly Understood" = "dotted",
                                                                    "Somewhat Understood" = "dashed",
                                                                    "Better Understood" = "solid"),
                        drop = FALSE) +
  guides(
    color = guide_legend(
      order = 1,
      override.aes = list(linewidth = 1.2),
      ncol = 1
    ),
    fill = guide_legend(
      order = 1,
      override.aes = list(alpha = 0.6),
      ncol = 1
    ),
    linetype = guide_legend(
      order = 2,
      override.aes = list(linewidth = 1.2),
      ncol = 1
    )
  ) +
  theme(
    legend.position = "bottom",
    legend.text  = element_text(size = 12),
    legend.title = element_text(size = 14)
  )

# Use if plotting lines next to each other
shorearmor_ML_t <- tibble(
  Level = c(levels_pos, levels_neg),
  Scenario = c(
    rep("Hardened Dominance", length(levels_pos)),
    rep("Marsh Dominance", length(levels_neg))
  ),
  shorearmor_ML = c(seq(0, -0.3, length.out = length(levels_pos)),
                    seq(0.3, 0, length.out = length(levels_neg))),
  Understanding = factor(
    "Better Understood",
    levels = c("Poorly Understood", "Somewhat Understood", "Better Understood")
  )
)

shorearmor_ML_t_Graph <- ggplot(shorearmor_ML_t, aes(x = Level, y = shorearmor_ML, color = Scenario, linetype = Understanding)) +
  geom_line(linewidth = 3, show.legend = TRUE) +
  ylim(-0.5, 0.5) +
  theme_classic() +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  labs(x = "Scenario Intensity",
       y = "Relative Change in Marine Life",
       color = NULL,
       linetype = "Level of Understanding") +
  scale_x_continuous(limits = c(-1, 1)) +
  scale_color_manual(values = c("Marsh Dominance" = "#1B9E77",
                                "Hardened Dominance" = "#7570B3")) +
  scale_linetype_manual(values = c("Poorly Understood" = "dotted",
                                   "Somewhat Understood" = "dashed",
                                   "Better Understood" = "solid"),
                        drop = FALSE) +
  annotate("label", 
           x = -0.95, y = 0.5,     
           label = "2b", 
           fill = "white", 
           color = "black", 
           label.size = 0.3,       
           label.r = unit(0.15, "lines"),  
           size = 5,
           fontface = "bold") +
  guides(
    color = guide_legend(order = 1),
    fill = guide_legend(order = 1),
    linetype = guide_legend(order = 2)
  )

shorearmorstormproGraph_2 <- seawall %>%
  mutate(
    Scenario = case_when(
      Level_num <= 0 ~ "Marsh Dominance",
      Level_num > 0  ~ "Hardened Dominance"
    ),
    Understanding = factor(
      "Better Understood",
      levels = c("Poorly Understood", "Somewhat Understood", "Better Understood")
    )
  ) %>%
  bind_rows(zero_seawall) %>%
  ggplot(aes(x = Level_num, y = shorearmor_SP, color = Scenario, fill = Scenario, linetype = Understanding)) +
  geom_line(show.legend = TRUE) +
  geom_ribbon(aes(ymin = shorearmor_SP - shorearmor_SP_margin_of_error,
                  ymax = shorearmor_SP + shorearmor_SP_margin_of_error), alpha = 0.2, color = NA, show.legend = FALSE) +
  geom_vline(xintercept = 0, color = "grey90", linewidth = 0.5, linetype = "solid") +
  theme_classic() +
  theme(axis.line.x = element_line(arrow = grid::arrow(length = unit(0.3, "cm"), 
                                                       ends = "last")),
        axis.title.x = element_text(angle = 0)) +
  labs(x = "Scenario Intensity",
       y = "Relative Change in Storm Protection",
       color = NULL, fill = NULL) +
  scale_x_continuous(labels = function(x) abs(x)) +
  scale_y_continuous(limits = c(-0.32, 0), breaks = seq(-0.3, 0, by = 0.1)) +
  scale_color_manual(name = "Scenario", values = c("Marsh Dominance" = "#1B9E77", "Hardened Dominance" = "#7570B3")) +
  scale_fill_manual(name = "Scenario", values = c("Marsh Dominance" = "#1B9E77", "Hardened Dominance" = "#7570B3")) +
  annotate("label", 
           x = -0.85, y = -0.3,     
           label = "Past\nCondition", 
           fill = "white", 
           color = "black", 
           label.size = 0,       
           label.r = unit(0.15, "lines"),  
           size = 3.5) +  
  annotate("label", 
           x = 0.85, y = -0.3,     
           label = "Future\nProjection", 
           fill = "white", 
           color = "black", 
           label.size = 0,       
           label.r = unit(0.15, "lines"),  
           size = 3.5) +
  annotate("label", 
           x = -0.95, y = 0,     
           label = "3a", 
           fill = "white", 
           color = "black", 
           label.size = 0.3,       
           label.r = unit(0.15, "lines"),  
           size = 5,
           fontface = "bold") +
  scale_linetype_manual(name = "Level of Understanding", values = c("Poorly Understood" = "dotted",
                                                                    "Somewhat Understood" = "dashed",
                                                                    "Better Understood" = "solid"),
                        drop = FALSE) +
  guides(
    color = guide_legend(
      order = 1,
      override.aes = list(linewidth = 1.2),
      ncol = 1
    ),
    fill = guide_legend(
      order = 1,
      override.aes = list(alpha = 0.6),
      ncol = 1
    ),
    linetype = guide_legend(
      order = 2,
      override.aes = list(linewidth = 1.2),
      ncol = 1
    )
  ) +
  theme(
    legend.position = "bottom",
    legend.text  = element_text(size = 12),
    legend.title = element_text(size = 14)
  )

shorearmor_SP_t <- tibble(
  Level = rep(levels, times = 2),
  Scenario = rep(c("Shoreline Hardening", "Marsh Restoration"), each = length(levels)),
  shorearmor_SP = c(seq(0, 0.3, length.out = length(levels)),
                    seq(0, -0.3, length.out = length(levels))),
  Understanding = factor("Poorly Understood",
                         levels = c("Poorly Understood", "Somewhat Understood", "Better Understood")))

shorearmor_SP_t_Graph <- ggplot(shorearmor_SP_t, aes(x = Level, y = shorearmor_SP, color = Scenario, linetype = Understanding)) +
  geom_line(linewidth = 3, show.legend = TRUE) +
  theme_classic() +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  labs(x = "Scenario Intensity",
       y = "Relative Change in Storm Protection",
       color = NULL,
       linetype = "Level of Understanding") +
  scale_x_continuous(limits = c(0, 1)) +
  scale_color_manual(values = c("Marsh Restoration" = "white",
                                "Shoreline Hardening" = "white")) +
  scale_linetype_manual(values = c("Poorly Understood" = "dotted",
                                   "Somewhat Understood" = "dashed",
                                   "Better Understood" = "solid"),
                        drop = FALSE) +
  annotate("label", 
           x = 0.5, y = 0.25,       
           label = "Context-Specific Response", 
           fill = "white", 
           color = "black", 
           label.size = 0.3,      
           label.r = unit(0.15, "lines"),  
           size = 4) +
  annotate("label", 
           x = 0.025, y = 0.5,     
           label = "3b", 
           fill = "white", 
           color = "black", 
           label.size = 0.3,       
           label.r = unit(0.15, "lines"),  
           size = 5,
           fontface = "bold") +
  guides(
    color = guide_legend(order = 1),
    fill = guide_legend(order = 1),
    linetype = guide_legend(order = 2)
  )

shorearmorwaterqualGraph_2 <- seawall %>%
  mutate(
    Scenario = case_when(
      Level_num <= 0 ~ "Marsh Dominance",
      Level_num > 0  ~ "Hardened Dominance"
    ),
    Understanding = factor(
      "Better Understood",
      levels = c("Poorly Understood", "Somewhat Understood", "Better Understood")
    )
  ) %>%
  bind_rows(zero_seawall) %>%
  ggplot(aes(x = Level_num, y = shorearmor_WQ, color = Scenario, fill = Scenario, linetype = Understanding)) +
  geom_line(show.legend = TRUE) +
  geom_ribbon(aes(ymin = shorearmor_WQ - shorearmor_WQ_margin_of_error,
                  ymax = shorearmor_WQ + shorearmor_WQ_margin_of_error), alpha = 0.2, color = NA, show.legend = FALSE) +
  geom_vline(xintercept = 0, color = "grey90", linewidth = 0.5, linetype = "solid") +
  theme_classic() +
  theme(axis.line.x = element_line(arrow = grid::arrow(length = unit(0.3, "cm"), 
                                                       ends = "last")),
        axis.title.x = element_text(angle = 0)) +
  labs(title = "Local Knowledge (Mental Models)",
       x = "Scenario Intensity",
       y = "Relative Change in Water Quality",
       color = NULL, fill = NULL) +
  scale_x_continuous(labels = function(x) abs(x)) +
  scale_y_continuous(limits = c(-0.33, 0.01), breaks = seq(-0.3, 0, by = 0.1)) +
  scale_color_manual(name = "Scenario", values = c("Marsh Dominance" = "#1B9E77", "Hardened Dominance" = "#7570B3")) +
  scale_fill_manual(name = "Scenario", values = c("Marsh Dominance" = "#1B9E77", "Hardened Dominance" = "#7570B3")) +
  annotate("label", 
           x = -0.85, y = -0.3,     
           label = "Past\nCondition", 
           fill = "white", 
           color = "black", 
           label.size = 0,       
           label.r = unit(0.15, "lines"),  
           size = 3.5) +  
  annotate("label", 
           x = 0.85, y = -0.3,     
           label = "Future\nProjection", 
           fill = NA, 
           color = "black", 
           label.size = 0,       
           label.r = unit(0.15, "lines"),  
           size = 3.5) +
  annotate("label", 
           x = -0.95, y = 0.01,     
           label = "1a", 
           fill = "white", 
           color = "black", 
           label.size = 0.3,       
           label.r = unit(0.15, "lines"),  
           size = 5,
           fontface = "bold") +
  scale_linetype_manual(name = "Level of Understanding", values = c("Poorly Understood" = "dotted",
                                                                    "Somewhat Understood" = "dashed",
                                                                    "Better Understood" = "solid"),
                        drop = FALSE) +
  guides(
    color = guide_legend(
      order = 1,
      override.aes = list(linewidth = 1.2),
      ncol = 1
    ),
    fill = guide_legend(
      order = 1,
      override.aes = list(alpha = 0.6),
      ncol = 1
    ),
    linetype = guide_legend(
      order = 2,
      override.aes = list(linewidth = 1.2),
      ncol = 1
    )
  ) +
  theme(
    legend.position = "bottom",
    legend.text  = element_text(size = 12),
    legend.title = element_text(size = 14)
  )

shorearmorwaterqualGraph_2

# Use if plotting lines next to each other
shorearmor_WQ_t <- tibble(
  Level = c(levels_pos, levels_neg),
  Scenario = c(
    rep("Hardened Dominance", length(levels_pos)),
    rep("Marsh Dominance", length(levels_neg))
  ),
  shorearmor_WQ = c(seq(0, -0.3, length.out = length(levels_pos)),
                    seq(0.3, 0, length.out = length(levels_neg))),
  Understanding = factor(
    "Better Understood",
    levels = c("Poorly Understood", "Somewhat Understood", "Better Understood")
  )
)

shorearmor_WQ_t_Graph <- ggplot(shorearmor_WQ_t, aes(x = Level, y = shorearmor_WQ, color = Scenario, linetype = Understanding)) +
  geom_line(linewidth = 3, show.legend = TRUE) +
  ylim(-0.5, 0.5) +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  labs(title = "Technical Knowledge (Literature)",
       x = "Scenario Intensity",
       y = "Relative Change in Water Quality",
       color = NULL,
       linetype = "Level of Understanding") +
  scale_x_continuous(limits = c(-1, 1)) +
  scale_color_manual(values = c("Marsh Dominance" = "#1B9E77",
                                "Hardened Dominance" = "#7570B3")) +
  scale_linetype_manual(values = c("Poorly Understood" = "dotted",
                                   "Somewhat Understood" = "dashed",
                                   "Better Understood" = "solid"),
                        drop = FALSE) +
  annotate("label", 
           x = -0.95, y = 0.5,     
           label = "1b", 
           fill = "white", 
           color = "black", 
           label.size = 0.3,       
           label.r = unit(0.15, "lines"),  
           size = 5,
           fontface = "bold") +
  guides(
    color = guide_legend(order = 1),
    fill = guide_legend(order = 1),
    linetype = guide_legend(order = 2)
  )

ggarrange(shorearmorwaterqualGraph_2, shorearmor_WQ_t_Graph, shorearmormarlifeGraph_2, shorearmor_ML_t_Graph, shorearmorstormproGraph_2, shorearmor_SP_t_Graph,   
          ncol = 2, nrow = 3, 
          common.legend = TRUE, legend = "right"
          # labels = c("1a)", "1b)", "2a)", "2b)", "3a)", "3b)")
)

