# ==========================================================================================================================================
## Script Name: Data Cleaning: Updated Full Kano and Ibadan Datasets
## Purpose: Assigns variable names and does data cleaning of full household data from wet + dry seasons in Ibadan and Kano
## Author: Grace Legris, Research Data Analyst
# ==========================================================================================================================================

kano_dry <- read.dta13(file.path(KidsData, "KN_IP_alldata_dry.dta"))
kano_wet <- read.dta13(file.path(KidsData, "KN_IP_alldata_wet.dta"))
ibadan_dry <- read.dta13(file.path(KidsData, "IB_IP_alldata_dry.dta"))
ibadan_wet <- read.dta13(file.path(KidsData, "IB_IP_alldata_wet.dta"))

clean_kano <- function(data){
  data <- data %>% 
    rename(
      lga = bi1,
      ward = bi2,
      settlement_type = bi3,
      age = hl5,
      dob = hl6,
      two_wk_fever = q401,
      test_result = q302,
    ) %>%
    dplyr::select(lga, ward, settlement_type, age, dob, two_wk_fever, test_result) %>% 
    mutate(
      test_result = case_when(
        test_result == 1 ~ "Positive",
        test_result == 2 ~ "Negative",
        test_result == 3 ~ "Other",
        TRUE ~ NA_character_)
    ) %>% 
    mutate(
      two_wk_fever = case_when(
        two_wk_fever == 1 ~ "Yes",
        two_wk_fever == 2 ~ "No",
        TRUE ~ NA_character_)
    ) %>% 
    mutate(
      settlement_type = case_when(
        settlement_type == 1 ~ "Formal",
        settlement_type == 2 ~ "Informal",
        settlement_type == 3 ~ "Slum",
        TRUE ~ NA_character_)
    ) %>% 
    # clean numeric lga names
    mutate(
      lga_numeric = suppressWarnings(as.numeric(lga)),  # coerce to numeric, will become NA if not numeric
      lga = case_when(
        lga_numeric == 1 ~ "Dala",
        lga_numeric == 2 ~ "Fagge",
        lga_numeric == 3 ~ "Gwale",
        lga_numeric == 4 ~ "Kano Municipal",
        lga_numeric == 5 ~ "Nassarawa",
        TRUE ~ as.character(lga)
      )
    ) %>%
    dplyr::select(-lga_numeric) %>% 
    # clean other lga names
    mutate(
      lga = str_trim(str_to_upper(lga)),
      lga = case_when(
        lga %in% c("DALA") ~ "Dala",
        lga %in% c("FAGGE", "FAGGED2", "FAGGE D2", "FAGGR", "FAGE") ~ "Fagge",
        lga %in% c("GWALE", "GWA", "GAWLE", "Æ\u0093WALE") ~ "Gwale",
        lga %in% c("NASSARAWA", "NASARAWA", "NASSRAWA") ~ "Nassarawa",
        lga %in% c("KANO MUNICIPAL", "K/MUNICIPAL", "MUNICIPAL", "KMC", "Kmc", "KANO  MUNICIPAL") ~ "Kano Municipal",
        lga %in% c("KANO") ~ "Kano",
        lga %in% c("ZANGO", "Zango") ~ "Zango",
        lga %in% c("TARAUNI", "Tarauni") ~ "Tarauni",
        lga %in% c("GIGINYU") ~ "Ginginyu",
        TRUE ~ str_to_title(str_to_lower(lga))
      )
    ) %>% 
    mutate(state = "Kano")
}

clean_ibadan <- function(data){
  data <- data %>% 
    rename(
      lga = bi1,
      ward = bi2,
      settlement_type = settlement,
      age = hl5_x,
      dob = hl6,
      two_wk_fever = q401,
      test_result = q302,
    ) %>%
    dplyr::select(lga, ward, settlement_type, age, dob, two_wk_fever, test_result) %>% 
    mutate(
      test_result = case_when(
        test_result == 1 ~ "Positive",
        test_result == 2 ~ "Negative",
        test_result == 3 ~ "Other",
        TRUE ~ NA_character_)
    ) %>% 
    mutate(
      two_wk_fever = case_when(
        two_wk_fever == 1 ~ "Yes",
        two_wk_fever == 2 ~ "No",
        TRUE ~ NA_character_)
    ) %>% 
    mutate(
      settlement_type = case_when(
        settlement_type == 1 ~ "Formal",
        settlement_type == 2 ~ "Informal",
        settlement_type == 3 ~ "Slum",
        TRUE ~ NA_character_)
    ) %>% 
    # clean ward names
    mutate(
      ward = str_trim(ward),
      ward = if_else(str_detect(ward, "^[0-9]+$"), ward, str_to_upper(ward)),
      ward = case_when(
        ward %in% c("BASHORUN", "BASORUN", "BASHORUN", "Bashorun") ~ "Basorun",
        ward %in% c("CHALLENGE", "CHALLENGE WARD") ~ "Challenge",
        ward == "OLOGUNERU" ~ "Ologuneru",
        ward == "AGUGU" ~ "Agugu",
        ward == "FELELE" ~ "Felele",
        ward == "IBADAN NORTH EAST" ~ "Ibadan Northeast",
        TRUE ~ ward
      )
    ) %>% 
    # clean lga names
    mutate(
      lga = str_trim(str_to_upper(lga)),  # Remove leading/trailing space & standardize to uppercase
      lga = case_when(
        lga %in% c("IBADAN NORTH EAST", "IBADAN NORTH-EAST", "IBADAN  NORTH EAST", "NORTH EAST") ~ "Ibadan Northeast",
        lga %in% c("IBADAN SOUTH EAST", "IBADAN SOUTH-EAST", "IBADAN  SOUTH EAST") ~ "Ibadan Southeast",
        lga %in% c("IBADAN NORTH WEST", "IBADAN NORTHWEST") ~ "Ibadan Northwest",
        lga == "IBADAN NORTH" ~ "Ibadan North",
        lga == "IBADAN SOUTH WEST" ~ "Ibadan Southwest",
        lga == "ONA ARA" ~ "Ona Ara",
        lga %in% c("IDO", "IDO LG") ~ "Ido",
        lga == "LAGELU" ~ "Lagelu",
        lga == "CHALLENGE" ~ "Challenge",
        lga == "AGUGU" ~ "Agugu",
        TRUE ~ str_to_title(lga)
      )
    ) %>% 
    mutate(state = "Ibadan")
}

clean_ibadan_dry <- function(data) {
    data <- data %>% 
      rename(
        lga = bi1,
        ward = bi2,
        settlement_type = bi3,
        age = hl5,
        dob = hl6,
        two_wk_fever = q401,
        test_result = q302,
      ) %>%
      dplyr::select(lga, ward, settlement_type, age, dob, two_wk_fever, test_result) %>% 
      mutate(
        test_result = case_when(
          test_result == 1 ~ "Positive",
          test_result == 2 ~ "Negative",
          test_result == 3 ~ "Other",
          TRUE ~ NA_character_)
      ) %>% 
      mutate(
        two_wk_fever = case_when(
          two_wk_fever == 1 ~ "Yes",
          two_wk_fever == 2 ~ "No",
          TRUE ~ NA_character_)
      ) %>% 
      mutate(
        settlement_type = case_when(
          settlement_type == 1 ~ "Formal",
          settlement_type == 2 ~ "Informal",
          settlement_type == 3 ~ "Slum",
          TRUE ~ NA_character_)
      ) %>% 
      # clean ward names
      mutate(
        ward = case_when(
          ward == "1" ~ "Agugu",
          ward == "2" ~ "Basorun",
          ward == "3" ~ "Challenge",
          ward == "3" ~ "Olopomewa",
          TRUE ~ NA_character_)
      ) %>% 
      # clean lga names
      mutate(
        lga = case_when(
          lga == "1" ~ "Ibadan North",
          lga == "2" ~ "Ibadan Northeast",
          lga == "3" ~ "Ibadan Northwest",
          lga == "3" ~ "Ibadan Southeast",
          TRUE ~ NA_character_)
      ) %>% 
      mutate(state = "Ibadan")
}

kano_dry <- clean_kano(kano_dry)
kano_wet <- clean_kano(kano_wet)
ibadan_dry <- clean_ibadan_dry(ibadan_dry)
ibadan_wet <- clean_ibadan(ibadan_wet)

# clean ward names in kano wet season
kano_wet <- kano_wet %>% 
  dplyr::filter(ward != "3") %>% 
  mutate(
    ward = case_when(
      ward == 1 ~ "Zango",
      ward == 2 ~ "Dorayi",
      ward == 4 ~ "Fagge D2",
      ward == 5 ~ "Gobirawa",
      ward == 6 ~ "Gingiyu",
      TRUE ~ NA_character_)
    )

# clean ward names in kano dry season
kano_dry <- kano_dry %>% 
  mutate(
    ward = case_when(
      ward == 1 ~ "Zango",
      ward == 2 ~ "Dorayi",
      ward == 3 ~ "Gingiyu",
      ward == 4 ~ "Fagge D2",
      ward == 5 ~ "Gobirawa",
      ward == "1" ~ "Zango",
      ward == "2" ~ "Dorayi",
      ward == "3" ~ "Gingiyu",
      ward == "4" ~ "Fagge D2",
      TRUE ~ NA_character_)
  )

# clean ward names in ibadan wet season
ibadan_wet <- ibadan_wet %>% 
  mutate(
    ward = case_when(
      ward == 1 ~ "Agugu",
      ward == 2 ~ "Basorun",
      ward == 3 ~ "Challenge",
      ward == 4 ~ "Olopomewa",
    )
  )

# add season variables
kano_dry <- kano_dry %>% mutate(season = "Dry")
kano_wet <- kano_wet %>% mutate(season = "Wet")
ibadan_dry <- ibadan_dry %>% mutate(season = "Dry")
ibadan_wet <- ibadan_wet %>% mutate(season = "Wet")

all_df <- rbind(kano_dry, kano_wet, ibadan_dry, ibadan_wet)

all_df$both <- ifelse(all_df$two_wk_fever == "Yes" & all_df$test_result == "Positive", "Yes", "No")

## -----------------------------------------------------------------------------------------------------------------------------------------
### Analysis
## -----------------------------------------------------------------------------------------------------------------------------------------

kano_dry <- all_df %>% dplyr::filter(season == "Dry" & state == "Kano")
kano_wet <- all_df %>% dplyr::filter(season == "Wet" & state == "Kano")
ibadan_dry <- all_df %>% dplyr::filter(season == "Dry" & state == "Ibadan")
ibadan_wet <- all_df %>% dplyr::filter(season == "Wet" & state == "Ibadan")
kano <- all_df %>% dplyr::filter(state == "Kano")
ibadan <- all_df %>% dplyr::filter(state == "Ibadan")
kano_u18 <- kano %>% dplyr::filter(age < 18)
ibadan_u18 <- ibadan %>% dplyr::filter(age < 18)
kano_adult <- kano %>% dplyr::filter(age >= 18)
ibadan_adult <- ibadan %>% dplyr::filter(age >= 18)

# save cleaned datasets
write.csv(kano_dry, file = file.path(OutputDir, "kano_dry.csv"), row.names = FALSE)
write.csv(kano_wet, file = file.path(OutputDir, "kano_wet.csv"), row.names = FALSE)
write.csv(ibadan_dry, file = file.path(OutputDir, "ibadan_dry.csv"), row.names = FALSE)
write.csv(ibadan_wet, file = file.path(OutputDir, "ibadan_wet.csv"), row.names = FALSE)
write.csv(kano, file = file.path(OutputDir, "kano_wet_dry.csv"), row.names = FALSE)
write.csv(ibadan, file = file.path(OutputDir, "ibadan_wet_dry.csv"), row.names = FALSE)

# summarize by ward to calculate prevalence and incidence
summarize_by_ward <- function(data) {
  data <- data %>% 
    group_by(ward, season) %>%
    summarise(
      n = n(),
      prevalence = mean(test_result == "Positive", na.rm = TRUE),
      incidence = mean(both == "Yes", na.rm = TRUE),
      .groups = "drop"
    )
}

kano_summary <- summarize_by_ward(kano)
ibadan_summary <- summarize_by_ward(ibadan)
kano_u18_summary <- summarize_by_ward(kano_u18)
ibadan_u18_summary <- summarize_by_ward(ibadan_u18)
kano_adult_summary <- summarize_by_ward(kano_adult)
ibadan_adult_summary <- summarize_by_ward(ibadan_adult)

plot_kano <- function(data) {
  overall_plot_kano <- ggplot(data, aes(x = prevalence, y = incidence, color = season)) +
    geom_point(aes(color = season), size = 3) +
    geom_smooth(se = FALSE, method = "lm") +
    xlim(0, 0.5) +
    ylim(0, 0.5) +
    scale_color_manual(
      values = c("Dry" = "#CBBD93", "Wet" = "#0E87CC"),
      name = "Season"
    ) +
    geom_text(aes(label = ward), hjust = 0, vjust = 1.2, size = 3) +
    labs(
      title = "Kano",
      x = "Prevalence",
      y = "Incidence"
    ) +
    guides(color = "none") +
    theme_manuscript() +
    theme(
      plot.title = element_text(hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5)
    )
}

plot_ibadan <- function(data) {
  overall_plot_ibadan <- ggplot(data, aes(x = prevalence, y = incidence, color = season)) +
    geom_point(aes(color = season), size = 3) +
    geom_smooth(se = FALSE, method = "lm") +
    xlim(0, 0.5) +
    ylim(0, 0.5) +
    scale_color_manual(
      values = c("Dry" = "#CBBD93", "Wet" = "#0E87CC"),
      name = "Season"
    ) +
    geom_text(aes(label = ward), hjust = 0, vjust = 1.2, size = 3) +
    labs(
      title = "Ibadan",
      x = "Prevalence",
      y = "Incidence"
    ) +
    guides(color = "none") +
    theme_manuscript() +
    theme(
      plot.title = element_text(hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5)
    )
}

kano_all_plot <- plot_kano(kano_summary)
ibadan_all_plot <- plot_ibadan(ibadan_summary)
kano_u18_plot <- plot_kano(kano_u18_summary)
ibadan_u18_plot <- plot_ibadan(ibadan_u18_summary)
kano_adult_plot <- plot_kano(kano_adult_summary)
ibadan_adult_plot <- plot_ibadan(ibadan_adult_summary)

kano_ibadan_all_plots <- grid.arrange(
  kano_all_plot,
  ibadan_all_plot,
  ncol = 2,
  top = "All Ages"
)

kano_ibadan_u18 <- grid.arrange(
  kano_u18_plot,
  ibadan_u18_plot,
  ncol = 2,
  top = "Under 18"
)

kano_ibadan_adults <- grid.arrange(
  kano_adult_plot,
  ibadan_adult_plot,
  ncol = 2,
  top = "18 and Over"
)

all_plots <- grid.arrange(
  kano_ibadan_all_plots,
  kano_ibadan_u18,
  kano_ibadan_adults,
  ncol = 1,
  top = "Kano and Ibadan: All Ages, Under 18, Adults"
)

ggsave(paste0(FigDir,"/", "_all_updated_kano_ibadan.pdf"), all_plots, width = 10, height = 12)

