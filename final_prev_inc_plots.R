# ==========================================================================================================================================
## Script Name: Prevalence/Incidence Plots (Updated)
## Purpose: Creates prevalence vs. incidence plots using cleaned complete household cross-sectional data, in the format this paper uses:
## https://pmc.ncbi.nlm.nih.gov/articles/PMC4569718/ 
## Author: Grace Legris, Research Data Analyst
# ==========================================================================================================================================

source("load_path.R")

# read in cleaned data
kano <- read.csv(file.path(OutputDir, "kano_wet_dry.csv"))
ibadan <- read.csv(file.path(OutputDir, "ibadan_wet_dry.csv"))
combined <- rbind(kano, ibadan)

# filter datasets to each age group
kano_0_5 <- kano %>% dplyr::filter(age >= 0 & age <= 5)
kano_6_15 <- kano %>% dplyr::filter(age >= 6 & age <= 15)
kano_15_plus <- kano %>% dplyr::filter(age > 15)
ibadan_0_5 <- ibadan %>% dplyr::filter(age >= 0 & age <= 5)
ibadan_6_15 <- ibadan %>% dplyr::filter(age >= 6 & age <= 15)
ibadan_15_plus <- ibadan %>% dplyr::filter(age > 15)
combined_0_5 <- combined %>% dplyr::filter(age >= 0 & age <= 5)
combined_6_15 <- combined %>% dplyr::filter(age >= 6 & age <= 15)
combined_15_plus <- combined %>% dplyr::filter(age > 15)

# function to create one prevalence vs. incidence plot per age group
plot_prev_incidence_single <- function(data, location_name, age_label) {
  
  # function to summarize prevalence/incidence by ward
  summarize_by_ward <- function(df) {
    df %>%
      group_by(ward, settlement_type) %>%
      summarise(
        prevalence = mean(test_result == "Positive", na.rm = TRUE),
        incidence = mean(both == "Yes", na.rm = TRUE),
        .groups = "drop"
      )
  }
  
  # color palettes
  kano_palette <- c("#d0e0f3", "#97a4d3")
  ibadan_palette <- c("#eef2c3", "#c9df86")
  combined_palette <- c("#f9efc1", "#eabc8a")
  
  # pick palette
  palette <- switch(location_name,
                    "Kano" = kano_palette,
                    "Ibadan" = ibadan_palette,
                    "Combined" = combined_palette,
                    c("#cccccc", "#999999"))  # fallback
  
  # generate plot
  ggplot(summarize_by_ward(data), aes(x = prevalence, y = incidence)) +
    geom_smooth(method = "loess", formula = y ~ x, se = TRUE, level = 0.95,
                fill = palette[1], color = NA) +
    geom_smooth(method = "loess", formula = y ~ x, se = TRUE, level = 0.68,
                fill = palette[2], color = NA) +
    geom_point(aes(color = settlement_type), alpha = 0.8) +
    labs(
      subtitle = paste0(location_name, " (Age ", age_label, ", n = ", nrow(data), ")"),
      x = "Prevalence",
      y = "Incidence",
      color = "Settlement Type"
    ) +
    theme_manuscript()
}

# kano plots
kano_0_5_plot <- plot_prev_incidence_single(kano_0_5, "Kano", "0–5")
kano_6_15_plot <- plot_prev_incidence_single(kano_6_15, "Kano", "6–15")
kano_15_plot <- plot_prev_incidence_single(kano_15_plus, "Kano", "15+")

# ibadan plots
ibadan_0_5_plot <- plot_prev_incidence_single(ibadan_0_5, "Ibadan", "0–5")
ibadan_6_15_plot <- plot_prev_incidence_single(ibadan_6_15, "Ibadan", "6–15")
ibadan_15_plot <- plot_prev_incidence_single(ibadan_15_plus, "Ibadan", "15+")

# combined plots
combined_0_5_plot <- plot_prev_incidence_single(combined_0_5, "Combined", "0–5")
combined_6_15_plot <- plot_prev_incidence_single(combined_6_15, "Combined", "6–15")
combined_15_plot <- plot_prev_incidence_single(combined_15_plus, "Combined", "15+")

# save the plots
library(gridExtra)
library(cowplot)

shared_legend <- get_legend(combined_0_5_plot + theme(legend.position = "right"))

# remove legends from all plots
plots_nolegend <- list(
  kano_0_5_plot, kano_6_15_plot, kano_15_plot,
  ibadan_0_5_plot, ibadan_6_15_plot, ibadan_15_plot,
  combined_0_5_plot, combined_6_15_plot, combined_15_plot
) %>%
  lapply(function(p) p + theme(legend.position = "none"))

# layout for 3x3 plots
grid_plots <- (
  (plots_nolegend[[1]] / plots_nolegend[[2]] / plots_nolegend[[3]]) |  # Kano
    (plots_nolegend[[4]] / plots_nolegend[[5]] / plots_nolegend[[6]]) |  # Ibadan
    (plots_nolegend[[7]] / plots_nolegend[[8]] / plots_nolegend[[9]])    # Combined
) + plot_layout(guides = "collect") & theme(legend.position = "none")

# add the legend
final_plot <- plot_grid(
  grid_plots,
  shared_legend,
  rel_widths = c(0.9, 0.1),
  nrow = 1
)

ggsave(file.path(FigDir, "updated_prev_inc.png"), final_plot, width = 12, height = 12) 