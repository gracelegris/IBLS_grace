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

# filter out wet and dry season data
combined_wet <- combined %>% dplyr::filter(season == "Wet")
combined_dry <- combined %>% dplyr::filter(season == "Dry")

# filter datasets to each age group
# kano_0_5 <- kano %>% dplyr::filter(age >= 0 & age <= 5)
# kano_6_15 <- kano %>% dplyr::filter(age >= 6 & age <= 15)
# kano_15_plus <- kano %>% dplyr::filter(age > 15)
# ibadan_0_5 <- ibadan %>% dplyr::filter(age >= 0 & age <= 5)
# ibadan_6_15 <- ibadan %>% dplyr::filter(age >= 6 & age <= 15)
# ibadan_15_plus <- ibadan %>% dplyr::filter(age > 15)

combined_wet_0_5 <- combined_wet %>% dplyr::filter(age >= 0 & age <= 5)
combined_wet_6_15 <- combined_wet %>% dplyr::filter(age >= 6 & age <= 15)
combined_wet_15_plus <- combined_wet %>% dplyr::filter(age > 15)
combined_dry_0_5 <- combined_dry %>% dplyr::filter(age >= 0 & age <= 5)
combined_dry_6_15 <- combined_dry %>% dplyr::filter(age >= 6 & age <= 15)
combined_dry_15_plus <- combined_dry %>% dplyr::filter(age > 15)
combined_0_5 <- combined %>% dplyr::filter(age >= 0 & age <= 5)
combined_6_15 <- combined %>% dplyr::filter(age >= 6 & age <= 15)
combined_15_plus <- combined %>% dplyr::filter(age > 15)

# function to create one prevalence vs. incidence plot per age group
plot_prev_incidence_single <- function(data, location_name, age_label) {
  
  # function to summarize prevalence/incidence by ward
  summarize_by_ward_settlement <- function(df) {
    df %>%
      group_by(ward, settlement_type) %>%
      summarise(
        prevalence = mean(test_result == "Positive", na.rm = TRUE),
        incidence = mean(both == "Yes", na.rm = TRUE),
        .groups = "drop"
      )
  }
  # summarize_by_settlement <- function(df) {
  #   df %>%
  #     group_by(settlement_type) %>%
  #     summarise(
  #       prevalence = mean(test_result == "Positive", na.rm = TRUE),
  #       incidence = mean(both == "Yes", na.rm = TRUE),
  #       .groups = "drop"
  #     )
  # }
  
  # color palette for confidence intervals
  ci_palette <- c("#BEBEBE", "#a9a9a9")
  
  data_summarized <- summarize_by_ward_settlement(data)
  
  # for 15+ combined plot, remove outliers (prevalence and incidence > 0.3)
  if(location_name == "Combined" & age_label == "15+") {
    data_summarized <- data_summarized %>% 
      dplyr::filter(prevalence < 0.3)
  }
  
  # generate plot
  if(age_label == "15+") {
    
    if(location_name == "Wet") {
      ggplot(data_summarized, aes(x = prevalence, y = incidence)) +
        geom_smooth(method = "loess", formula = y ~ x, se = TRUE, level = 0.95,
                    fill = ci_palette[1], color = "black", linetype = "solid", linewidth = 0.4) +  # adds 95% CI with trend line
        geom_smooth(method = "loess", formula = y ~ x, se = TRUE, level = 0.68,
                    fill = ci_palette[2], color = NA) +  # 68% CI only shaded
        geom_point(aes(color = settlement_type), alpha = 0.8, size = 3) +
        xlim(0, 1.0) +
        ylim(0, 1.0) +
        scale_color_manual(values = c(
          "Formal" = "#00798c",
          "Informal" = "#d1495b",
          "Slum" = "#edae49"
        )) +
        labs(
          subtitle = paste0(location_name, " (Age ", age_label, ", n = ", nrow(data), ")"),
          x = "Prevalence",
          y = "Incidence",
          color = "Settlement Type"
        ) +
        theme_manuscript()
    } else if (location_name %in% c("Dry", "Combined")) {
      
      # manually calculate CIs so we can remove the negative values
      # Fit loess model
      loess_fit <- loess(incidence ~ prevalence, data_summarized)
      
      # Predict with standard error
      pred <- predict(loess_fit, se = TRUE)
      
      # Create data frame of predictions
      pred_df <- data_summarized %>%
        mutate(
          fit = pred$fit,
          se = pred$se.fit,
          df = pred$df,
          ci_upper_95 = fit + qt(0.975, df) * se,
          ci_lower_95 = pmax(fit - qt(0.975, df) * se, 0),
          ci_upper_68 = fit + qt(0.84, df) * se,
          ci_lower_68 = pmax(fit - qt(0.84, df) * se, 0)
        )
      
      ggplot(pred_df, aes(x = prevalence, y = incidence)) +
        geom_ribbon(aes(ymin = ci_lower_95, ymax = ci_upper_95), fill = ci_palette[1], alpha = 0.5) +
        geom_ribbon(aes(ymin = ci_lower_68, ymax = ci_upper_68), fill = ci_palette[2], alpha = 0.5) +
        geom_line(aes(y = fit), color = "black", linewidth = 0.4) +
        # geom_smooth(method = "loess", formula = y ~ x, se = TRUE, level = 0.95,
        #             fill = ci_palette[1], color = "black", linetype = "solid", linewidth = 0.4) +  # adds 95% CI with trend line
        # geom_smooth(method = "loess", formula = y ~ x, se = TRUE, level = 0.68,
        #             fill = ci_palette[2], color = NA) +  # 68% CI only shaded
        geom_point(aes(color = settlement_type), alpha = 0.8, size = 3) +
        xlim(0, 0.125) +
        ylim(0, 0.125) +
        scale_color_manual(values = c(
          "Formal" = "#00798c",
          "Informal" = "#d1495b",
          "Slum" = "#edae49"
        )) +
        labs(
          subtitle = paste0(location_name, " (Age ", age_label, ", n = ", nrow(data), ")"),
          x = "Prevalence",
          y = "Incidence",
          color = "Settlement Type"
        ) +
        theme_manuscript()
    }
    
  }
  else if(age_label %in% c("0–5", "6–15")) {
    # add linear fit line
    ggplot(data_summarized, aes(x = prevalence, y = incidence)) +
      geom_smooth(method = "lm", se = FALSE, color = "black", size = 0.4) +
      geom_point(aes(color = settlement_type), alpha = 0.8, size = 3) +
      xlim(0, 1.0) +
      ylim(0, 1.0) +
      scale_color_manual(values = c(
        "Formal" = "#00798c",
        "Informal" = "#d1495b",
        "Slum" = "#edae49"
      )) +
      labs(
        subtitle = paste0(location_name, " (Age ", age_label, ", n = ", nrow(data), ")"),
        x = "Prevalence",
        y = "Incidence",
        color = "Settlement Type"
      ) +
      theme_manuscript()
  }
  
}

# kano plots
# kano_0_5_plot <- plot_prev_incidence_single(kano_0_5, "Kano", "0–5")
# kano_6_15_plot <- plot_prev_incidence_single(kano_6_15, "Kano", "6–15")
# kano_15_plot <- plot_prev_incidence_single(kano_15_plus, "Kano", "15+")

# ibadan plots
# ibadan_0_5_plot <- plot_prev_incidence_single(ibadan_0_5, "Ibadan", "0–5")
# ibadan_6_15_plot <- plot_prev_incidence_single(ibadan_6_15, "Ibadan", "6–15")
# ibadan_15_plot <- plot_prev_incidence_single(ibadan_15_plus, "Ibadan", "15+")

# wet plots
wet_0_5_plot <- plot_prev_incidence_single(combined_wet_0_5, "Wet", "0–5")
wet_6_15_plot <- plot_prev_incidence_single(combined_wet_6_15, "Wet", "6–15")
wet_15_plus_plot <- plot_prev_incidence_single(combined_wet_15_plus, "Wet", "15+")

# dry plots
dry_0_5_plot <- plot_prev_incidence_single(combined_dry_0_5, "Dry", "0–5")
dry_6_15_plot <- plot_prev_incidence_single(combined_dry_6_15, "Dry", "6–15")
dry_15_plus_plot <- plot_prev_incidence_single(combined_dry_15_plus, "Dry", "15+")

# combined plots
combined_0_5_plot <- plot_prev_incidence_single(combined_0_5, "Combined", "0–5")
combined_6_15_plot <- plot_prev_incidence_single(combined_6_15, "Combined", "6–15")
combined_15_plus_plot <- plot_prev_incidence_single(combined_15_plus, "Combined", "15+")

# save the plots
library(gridExtra)
library(cowplot)

shared_legend <- get_legend(combined_0_5_plot + theme(legend.position = "right"))

# remove legends from all plots
plots_nolegend <- list(
  wet_0_5_plot, wet_6_15_plot, wet_15_plus_plot,
  dry_0_5_plot, dry_6_15_plot, dry_15_plus_plot,
  combined_0_5_plot, combined_6_15_plot, combined_15_plus_plot
) %>%
  lapply(function(p) p + theme(legend.position = "none"))

# layout for 3x3 plots
grid_plots <- (
    (plots_nolegend[[1]] / plots_nolegend[[2]] / plots_nolegend[[3]]) |  # wet
    (plots_nolegend[[4]] / plots_nolegend[[5]] / plots_nolegend[[6]]) |  # dry
    (plots_nolegend[[7]] / plots_nolegend[[8]] / plots_nolegend[[9]])    # combined
) + plot_layout(guides = "collect") & theme(legend.position = "none")

# add the legend
final_plot <- plot_grid(
  grid_plots,
  shared_legend,
  rel_widths = c(0.9, 0.1),
  nrow = 1
)
final_plot

ggsave(file.path(FigDir, "combined_only_prev_inc.pdf"), final_plot, width = 13, height = 12) 
