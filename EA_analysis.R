# ==========================================================================================================================================
## Script Name: TPR by EA - cross-sectional and longitudinal
## Purpose: Calculates malaria TPR by enumeration area and merges this with centroids of each EA for Ibadan and Kano.
## Author: Grace Legris, Research Data Analyst
# ==========================================================================================================================================

source("load_path.R")
EADataDir <- file.path(DriveDir, "data/nigeria/kano_ibadan_epi/Combined Working Data")
OutputDir <- file.path(DriveDir, "projects/ChatMRPT/IBLS/tpr_by_ea")
library(readstata13)

# ==========================================================================================================================================
## CROSS-SECTIONAL DATA
# ==========================================================================================================================================

kano_dry <- read.dta13(file.path(EADataDir, "Kano/Dry Season Data/Long Data/long_dryseason_household_membersV00.dta"))
kano_wet <- read.dta13(file.path(EADataDir, "Kano/Wet Season Data/Long Data/kano_wetseason_long_data.dta"))
ibadan_dry <- read.dta13(file.path(EADataDir, "Ibadan/Dry Season Data/Long Data/Ibadan_long_dry_season_household_members_records.dta"))
ibadan_wet <- read.dta13(file.path(EADataDir, "Ibadan/Wet Season Data/Long Data/ibadan_long_wetseason_household_members_with_ind_nets.dta")) %>% dplyr::rename(ea_cluster = enumaration_area)

clean_data <- function(data) {
  data <- data %>% 
    rename(
      age = hl5,
      test_result = q302,
      lat = bi7_lat,
      long = bi7_long,
      household_weight = overall_hh_weight
    ) %>%
    mutate(
      test_result = case_when(
        test_result == 1 ~ "positive",
        test_result == 2 ~ "negative",
        test_result == 3 ~ "other",
        TRUE ~ NA_character_)
    ) %>% 
    dplyr::select(ea_cluster, age, test_result, lat, long, household_weight)
}

# some duplicated variable names so make names unique
names(kano_wet) <- make.names(names(kano_wet), unique = TRUE)

kano_dry <- clean_data(kano_dry)
kano_wet <- clean_data(kano_wet)
ibadan_dry <- clean_data(ibadan_dry)
ibadan_wet <- clean_data(ibadan_wet)

# filter datasets for only children under 5
kano_wet <- kano_wet %>% dplyr::filter(age < 5) %>% mutate(season = "Wet")
kano_dry <- kano_dry %>% dplyr::filter(age < 5) %>% mutate(season = "Dry")
ibadan_wet <- ibadan_wet %>% dplyr::filter(age < 5) %>% mutate(season = "Wet")
ibadan_dry <- ibadan_dry %>% dplyr::filter(age < 5) %>% mutate(season = "Dry")

# combine datasets
kano <- kano_wet %>% rbind(kano_dry)
ibadan <- ibadan_wet %>% rbind(ibadan_dry)

# clean ea_cluster names in kano data
kano <- kano %>%
  mutate(
    ea_cluster = case_when(
      ea_cluster == "DORAYIBABBA/1366" ~ "DORAYI BABBA/1366",
      ea_cluster == "DORAYIBABBA/1376" ~ "DORAYI BABBA/1376",
      ea_cluster == "DORAYIBABBA/1378" ~ "DORAYI BABBA/1378",
      ea_cluster == "DORAYIBABBA/1380" ~ "DORAYI BABBA/1380",
      ea_cluster == "DORAYIBABBA/1382" ~ "DORAYI BABBA/1382",
      ea_cluster == "DORAYIBABBA/1386" ~ "DORAYI BABBA/1386",
      ea_cluster == "DORAYIKARAMA/1398" ~ "DORAYI KARAMA/1398",
      ea_cluster == "DORAYIKARAMA/1400" ~ "DORAYI KARAMA/1400",
      ea_cluster == "DORAYIKARAMA/1402" ~ "DORAYI KARAMA/1402",
      ea_cluster == "DORAYIKARAMA/1404" ~ "DORAYI KARAMA/1404",
      ea_cluster == "UNGUWARJAKADA/1416" ~ "UNGUWAR JAKADA/1416",
      ea_cluster == "UNGUWARJAKADA/1418" ~ "UNGUWAR JAKADA/1418",
      ea_cluster == "UNGUWARJAKADA/1426" ~ "UNGUWAR JAKADA/1426",
      ea_cluster == "UNGUWARJAKADA/1432" ~ "UNGUWAR JAKADA/1432",
      ea_cluster == "UNGUWARWAMBAI/1436" ~ "UNGUWAR WAMBAI/1436",
      ea_cluster == "UNGUWARWAMBAI/1438" ~ "UNGUWAR WAMBAI/1438",
      ea_cluster == "JAENJIGAWA/1422" ~ "JAEN JIGAWA/1422",
      ea_cluster == "KOFARMATADYEPITS/1456" ~ "KOFAR MATA DYE PITS/1456",
      ea_cluster == "MURTALAMUHAMMADSPECIALISTHOSPITAL1/1476" ~ "MURTALA MUHAMMAD SPECIALIST HOSPITAL 1/1476",
      ea_cluster == "MURTALAMUHAMMADSPECIALISTHOSPITAL2/1482" ~ "MURTALA MUHAMMAD SPECIALIST HOSPITAL 2/1482",
      ea_cluster == "BADAWALAYOUT/46" ~ "BADAWA LAYOUT/46",
      ea_cluster == "BADAWALAYOUT/47" ~ "BADAWA LAYOUT/47",
      ea_cluster == "BADAWALAYOUT/48" ~ "BADAWA LAYOUT/48",
      ea_cluster == "BADAWALAYOUT/49" ~ "BADAWA LAYOUT/49",
      ea_cluster == "BADAWALAYOUT/50" ~ "BADAWA LAYOUT/50",
      ea_cluster == "BADAWALAYOUT/51" ~ "BADAWA LAYOUT/51",
      ea_cluster == "BADAWALAYOUT/52" ~ "BADAWA LAYOUT/52",
      ea_cluster == "BADAWALAYOUT/53" ~ "BADAWA LAYOUT/53",
      ea_cluster == "BADAWALAYOUT/55" ~ "BADAWA LAYOUT/55",
      ea_cluster == "GIGINYUB/31" ~ "GIGINYU B/31",
      ea_cluster == "KAWOCIKINGARI/24" ~ "KAWO CIKI GARI/24",
      ea_cluster == "KAWOCIKINGARI/25" ~ "KAWO CIKI GARI/25",
      ea_cluster == "KAWOKUDU/26" ~ "KAWO KUDU/26",
      ea_cluster == "KAWOKUDU/27" ~ "KAWO KUDU/27",
      ea_cluster == "KAWOMAIGARI/28" ~ "KAWO MAIGARI/28",
      ea_cluster == "NASSARAWAGRA/4" ~ "NASSARAWA G.R.A/4",
      ea_cluster == "NASSARAWAGRA/10" ~ "NASSARAWA G.R.A/10",
      ea_cluster == "NASSARAWAGRA/18" ~ "NASSARAWA G.R.A/18",
      ea_cluster == "NASSARAWAGRA/21" ~ "NASSARAWA G.R.A/21",
      ea_cluster == "GOBIRAWAA/2032" ~ "GOBIRAWA A/2032",
      ea_cluster == "GOBIRAWAB/2298" ~ "GOBIRAWA B/2298",
      ea_cluster == "GOBIRAWAA/2460" ~ "GOBIRAWA A/2460",
      ea_cluster == "TRIUMPHPUBLISHINGCOMPANY/348" ~ "TRIUMPH PUBLISHING COMPANY/348",
      ea_cluster == "TRIUMPHPUBLISHINGCOMPANY/358" ~ "TRIUMPH PUBLISHING COMPANY/358",
      ea_cluster == "GOBIRAWAB/2282" ~ "GOBIRAWA B/2282",
      ea_cluster == "GOBIRAWAA/2374" ~ "GOBIRAWA A/2374",
      ea_cluster == "G_CIKINGARI/NA" ~ "G/CIKINGARI/NA",
      ea_cluster == "FAGGED2/354" ~ "FAGGE D2/354",
      ea_cluster == "CHIKALAROAD/356" ~ "CHIKALA ROAD/356",
      ea_cluster == "GOBIRAWAB/2198" ~ "GOBIRAWA B/2198",
      ea_cluster == "WAPAD2/322" ~ "WAPA D2/322",
      ea_cluster == "FILINDURUMI/2166" ~ "FILIN DURIMI/2166",
      TRUE ~ ea_cluster
    )
  )

# clean ea_cluster names in ibadan data
ibadan <- ibadan %>%
  mutate(ea_cluster = case_when(
    ea_cluster %in% c("AGUGU _035/33", "AGUGU_035/033") ~ "AGUGU_035/33",
    ea_cluster %in% c("AGUGU _061/17", "AGUGU_061/017") ~ "AGUGU_061/17",
    ea_cluster %in% c("AGUGU _065/13", "AGUGU_065/013") ~ "AGUGU_065/13",
    ea_cluster %in% c("AGUGU_001/012") ~ "AGUGU_001/12",
    ea_cluster %in% c("AGUGU_003/022") ~ "AGUGU_003/22",
    ea_cluster %in% c("AGUGU_004/035") ~ "AGUGU_004/35",
    ea_cluster %in% c("AGUGU_009/014") ~ "AGUGU_009/14",
    ea_cluster %in% c("AGUGU_012/026") ~ "AGUGU_012/26",
    ea_cluster %in% c("AGUGU_015/004") ~ "AGUGU_015/4",
    ea_cluster %in% c("AGUGU_017/016") ~ "AGUGU_017/16",
    ea_cluster %in% c("AGUGU_018/028") ~ "AGUGU_018/28",
    ea_cluster %in% c("AGUGU_019/027") ~ "AGUGU_019/27",
    ea_cluster %in% c("AGUGU_020/002") ~ "AGUGU_020/2",
    ea_cluster %in% c("AGUGU_022/010") ~ "AGUGU_022/10",
    ea_cluster %in% c("AGUGU_023/018") ~ "AGUGU_023/18",
    ea_cluster %in% c("AGUGU_024/037") ~ "AGUGU_024/37",
    ea_cluster %in% c("AGUGU_026/003") ~ "AGUGU_026/3",
    ea_cluster %in% c("AGUGU_027/001") ~ "AGUGU_027/1",
    ea_cluster %in% c("AGUGU_030/008") ~ "AGUGU_030/8",
    ea_cluster %in% c("AGUGU_031/021") ~ "AGUGU_031/21",
    ea_cluster %in% c("AGUGU_032/020") ~ "AGUGU_032/20",
    ea_cluster %in% c("AGUGU_033/009") ~ "AGUGU_033/9",
    ea_cluster %in% c("AGUGU_034/029") ~ "AGUGU_034/29",
    ea_cluster %in% c("AGUGU_041/005") ~ "AGUGU_041/5",
    ea_cluster %in% c("AGUGU_042/011") ~ "AGUGU_042/11",
    ea_cluster %in% c("AGUGU_045/031") ~ "AGUGU_045/31",
    ea_cluster %in% c("AGUGU_047/025") ~ "AGUGU_047/25",
    ea_cluster %in% c("AGUGU_051/023") ~ "AGUGU_051/23",
    ea_cluster %in% c("AGUGU_053/019") ~ "AGUGU_053/19",
    ea_cluster %in% c("AGUGU_055/006") ~ "AGUGU_055/6",
    ea_cluster %in% c("AGUGU_056/015") ~ "AGUGU_056/15",
    ea_cluster %in% c("AGUGU_057/036") ~ "AGUGU_057/36",
    ea_cluster %in% c("AGUGU_059/030") ~ "AGUGU_059/30",
    ea_cluster %in% c("AGUGU_062/034") ~ "AGUGU_062/34",
    ea_cluster %in% c("BASHORUN _010/10", "BASHORUN_010/010") ~ "BASHORUN_010/10",
    ea_cluster %in% c("BASHORUN _011/13", "BASHORUN_011/013") ~ "BASHORUN_011/13",
    ea_cluster %in% c("BASHORUN _039/3", "BASHORUN_039/003") ~ "BASHORUN_039/3",
    ea_cluster %in% c("BASHORUN _042/18", "BASHORUN_042/018") ~ "BASHORUN_042/18",
    ea_cluster == "BASHORUN _057/5" ~ "BASHORUN_057/5",
    ea_cluster == "BASHORUN _077/25" ~ "BASHORUN_077/25",
    ea_cluster %in% c("BASHORUN _112/36", "BASHORUN_112/036") ~ "BASHORUN_112/36",
    ea_cluster == "BASHORUN_016/027" ~ "BASHORUN_016/27",
    ea_cluster == "BASHORUN_035/022" ~ "BASHORUN_035/22",
    ea_cluster == "BASHORUN_049/015" ~ "BASHORUN_049/15",
    ea_cluster == "BASHORUN_059/023" ~ "BASHORUN_059/23",
    ea_cluster == "BASHORUN_060/011" ~ "BASHORUN_060/11",
    ea_cluster == "BASHORUN_068/002" ~ "BASHORUN_068/2",
    ea_cluster == "BASHORUN_080/001" ~ "BASHORUN_080/1",
    ea_cluster == "BASHORUN_101/030" ~ "BASHORUN_101/30",
    ea_cluster == "BASHORUN_106/012" ~ "BASHORUN_106/12",
    ea_cluster == "BASHORUN_117/033" ~ "BASHORUN_117/33",
    ea_cluster == "BASHORUN_120/004" ~ "BASHORUN_120/4",
    ea_cluster == "BASHORUN_125/035" ~ "BASHORUN_125/35",
    ea_cluster %in% c("CHALLENGE _007/17", "CHALLENGE_007/017") ~ "CHALLENGE_007/17",
    ea_cluster == "CHALLENGE_011/029" ~ "CHALLENGE_011/29",
    ea_cluster == "CHALLENGE_012/014" ~ "CHALLENGE_012/14",
    ea_cluster == "CHALLENGE_013/009" ~ "CHALLENGE_013/9",
    ea_cluster == "CHALLENGE_014/003" ~ "CHALLENGE_014/3",
    ea_cluster == "CHALLENGE_015/030" ~ "CHALLENGE_015/30",
    ea_cluster == "CHALLENGE_017/032" ~ "CHALLENGE_017/32",
    ea_cluster == "CHALLENGE_018/015" ~ "CHALLENGE_018/15",
    ea_cluster == "CHALLENGE_021/018" ~ "CHALLENGE_021/18",
    ea_cluster == "CHALLENGE_023/002" ~ "CHALLENGE_023/2",
    ea_cluster == "CHALLENGE_029/033" ~ "CHALLENGE_029/33",
    ea_cluster == "CHALLENGE_031/025" ~ "CHALLENGE_031/25",
    ea_cluster == "CHALLENGE_035/011" ~ "CHALLENGE_035/11",
    ea_cluster == "CHALLENGE_037/004" ~ "CHALLENGE_037/4",
    ea_cluster == "CHALLENGE_038/019" ~ "CHALLENGE_038/19",
    ea_cluster == "CHALLENGE_039/007" ~ "CHALLENGE_039/7",
    ea_cluster == "CHALLENGE_040/024" ~ "CHALLENGE_040/24",
    ea_cluster == "CHALLENGE_047/035" ~ "CHALLENGE_047/35",
    ea_cluster == "CHALLENGE_048/036" ~ "CHALLENGE_048/36",
    ea_cluster == "CHALLENGE_049/022" ~ "CHALLENGE_049/22",
    ea_cluster == "CHALLENGE_050/005" ~ "CHALLENGE_050/5",
    ea_cluster == "OLOPOMEWA _006/10" ~ "OLOPOMEWA_006/10",
    ea_cluster == "OLOPOMEWA _022/5" ~ "OLOPOMEWA_022/5",
    ea_cluster == "AGUGU 5/32" ~ "AGUGU 005/032",
    ea_cluster == "OLOGUNERU 51/2" ~ "OLOGUNERU 051/002",
    ea_cluster == "CHALLENGE 27/1" ~ "CHALLENGE 027/001",
    TRUE ~ ea_cluster
  )) %>% 
  mutate(
    ea_cluster = str_trim(ea_cluster),
    ea_cluster = str_replace_all(ea_cluster, "_", " "),
    ea_cluster = str_squish(ea_cluster)) # remove any double-spaces

ibadan <- ibadan %>%
  mutate(
    ea_cluster = case_when(
      ea_cluster == "AGUGU 052/024" ~ "AGUGU 052/24",
      TRUE ~ ea_cluster
    )
  )

# re-separate ibadan and kano data into wet and dry seasons
ibadan_dry <- ibadan %>% dplyr::filter(season == "Dry")
ibadan_wet <- ibadan %>% dplyr::filter(season == "Wet")
kano_dry <- kano %>% dplyr::filter(season == "Dry")
kano_wet <- kano %>% dplyr::filter(season == "Wet")

# function to extract centroids using the latitude and longitude from the original datasets
# extract them before summarizing by ea to calculate the tpr
extract_centroids <- function(df) {
  sf_points <- st_as_sf(df, coords = c("long", "lat"), crs = 4326)
  
  multipoints <- sf_points %>%
    group_by(ea_cluster) %>%
    summarise(geometry = st_combine(geometry), .groups = "drop") %>%
    mutate(geometry = st_cast(geometry, "MULTIPOINT"))
  
  centroids <- multipoints %>%
    mutate(geometry = st_centroid(geometry)) %>%
    mutate(
      longitude = st_coordinates(geometry)[, "X"],
      latitude = st_coordinates(geometry)[, "Y"]
    ) %>%
    dplyr::select(ea_cluster, longitude, latitude)
  
  return(centroids)
}

# function to summarize tpr by EA
summarize_tpr <- function(data) {
  data <- data %>%
    filter(!is.na(test_result)) %>%
    group_by(ea_cluster) %>%
    summarise(
      total_tests = n(),  # total number of tests (non-missing after filter)
      total_weight = sum(household_weight, na.rm = TRUE),
      num_positive_tests = sum(test_result == "positive", na.rm = TRUE),
      positive_weight = sum(household_weight[test_result == "positive"], na.rm = TRUE),
      tpr = positive_weight / total_weight, # calculate tpr as the proportion of weighted positives
    ) %>%
    ungroup()
}

# calculate centroids from raw data
kano_centroids <- extract_centroids(kano)
kano_wet_centroids <- extract_centroids(kano_wet)
kano_dry_centroids <- extract_centroids(kano_dry)
ibadan_centroids <- extract_centroids(ibadan)
ibadan_wet_centroids <- extract_centroids(ibadan_wet)
ibadan_dry_centroids <- extract_centroids(ibadan_dry)

# summarize TPR
kano_tpr_by_ea_dry <- summarize_tpr(kano_dry)
kano_tpr_by_ea_wet <- summarize_tpr(kano_wet)
kano_tpr_by_ea_total <- summarize_tpr(kano)
ibadan_tpr_by_ea_dry <- summarize_tpr(ibadan_dry)
ibadan_tpr_by_ea_wet <- summarize_tpr(ibadan_wet)
ibadan_tpr_by_ea_total <- summarize_tpr(ibadan)

# merge centroids into summarized EA-level data
kano_tpr_by_ea_total <- left_join(kano_tpr_by_ea_total, kano_centroids, by = "ea_cluster")
kano_tpr_by_ea_wet <- left_join(kano_tpr_by_ea_wet, kano_wet_centroids, by = "ea_cluster")
kano_tpr_by_ea_dry <- left_join(kano_tpr_by_ea_dry, kano_dry_centroids, by = "ea_cluster")
ibadan_tpr_by_ea_total <- left_join(ibadan_tpr_by_ea_total, ibadan_centroids, by = "ea_cluster")
ibadan_tpr_by_ea_wet <- left_join(ibadan_tpr_by_ea_wet, ibadan_wet_centroids, by = "ea_cluster")
ibadan_tpr_by_ea_dry <- left_join(ibadan_tpr_by_ea_dry, ibadan_dry_centroids, by = "ea_cluster")

# function to clean up table
make_table <- function(data) {
  data %>% 
    dplyr::select(ea_cluster, total_tests, num_positive_tests, tpr, longitude, latitude, geometry)
}

# create the formatted tables
kano_dry_table <- make_table(kano_tpr_by_ea_dry)
kano_wet_table <- make_table(kano_tpr_by_ea_wet)
kano_all_table <- make_table(kano_tpr_by_ea_total)
ibadan_dry_table <- make_table(ibadan_tpr_by_ea_dry)
ibadan_wet_table <- make_table(ibadan_tpr_by_ea_wet)
ibadan_all_table <- make_table(ibadan_tpr_by_ea_total)

# save as CSV files
write.csv(kano_all_table, file = file.path(OutputDir, "kano_all_tpr.csv"), row.names = FALSE)
write.csv(kano_dry_table, file = file.path(OutputDir, "kano_dry_tpr.csv"), row.names = FALSE)
write.csv(kano_wet_table, file = file.path(OutputDir, "kano_wet_tpr.csv"), row.names = FALSE)
write.csv(ibadan_all_table, file = file.path(OutputDir, "ibadan_all_tpr.csv"), row.names = FALSE)
write.csv(ibadan_dry_table, file = file.path(OutputDir, "ibadan_dry_tpr.csv"), row.names = FALSE)
write.csv(ibadan_wet_table, file = file.path(OutputDir, "ibadan_wet_tpr.csv"), row.names = FALSE)


# ==========================================================================================================================================
## LONGITUDINAL DATA
# ==========================================================================================================================================

# load and clean longitudinal data (baseline and follow-up surveys)
source("data_cleaning_long.R")

# combine kano and ibadan data
baseline <- rbind(base_data_ib, base_data_kn)
fup <- rbind(fup_data_ib, fup_data_kn)

# filter baseline data for U5 children only
baseline_u5 <- baseline %>% dplyr::filter(child_age_years < 5)

# merge baseline and follow up data. takes only the u5 children from the longitudinal dataset because it matches by sn
all_df <- baseline_u5 %>%
  left_join(fup, by = "sn")

### calculate TPR per EA per month
# pivot the rdt result columns into long format
fup_long <- all_df %>%
  dplyr::select(ea_cluster, hh_longitude, hh_latitude, starts_with("study_rdt_result_m")) %>%
  pivot_longer(
    cols = starts_with("study_rdt_result_m"),
    names_to = "month",
    values_to = "rdt_result"
  ) %>% 
  dplyr::rename(long = hh_longitude, lat = hh_latitude)

# clean month variable to just keep the month number (e.g., "m1", "m2", ..., "m12")
fup_long <- fup_long %>%
  mutate(month = as.integer(gsub("study_rdt_result_m", "", month)))

# clean up ea_cluster
fup_long <- fup_long %>%
  mutate(ea_cluster = str_trim(ea_cluster),
         ea_cluster = str_replace_all(ea_cluster, "_", " ")) %>%
  mutate(ea_cluster = case_when(
           ea_cluster %in% c("HOTORO /29", "HOTORO/29") ~ "HOTORO/29",
           ea_cluster %in% c("DORAYI KARAMA 1404", "DORAYI KARAMA/1404") ~ "DORAYI KARAMA/1404",
           ea_cluster %in% c("YAMADAWA1384", "YAMADAWA/1384") ~ "YAMADAWA/1384",
           ea_cluster %in% c("YAMADAWA\\1358", "YAMADAWA/1358") ~ "YAMADAWA/1358",
           ea_cluster %in% c("YAMADAWA\\1408", "YAMADAWA/1408") ~ "YAMADAWA/1408",
           ea_cluster %in% c("UNGUWAR JAKADA 1418", "UNGUWAR JAKADA/1418", "UNGUWAR JAKADA/ 1418") ~ "UNGUWAR JAKADA/1418",
           ea_cluster %in% c("UNGUWAR JAKADA\\ 1416", "UNGUWAR JAKADA/1416") ~ "UNGUWAR JAKADA/1416",
           ea_cluster %in% c("UNGUWAR WAMBAI 1436", "UNGUWAR WAMBAI/1436") ~ "UNGUWAR WAMBAI/1436",
           ea_cluster %in% c("BADAWALAYOUT/53", "BADAWA LAYOUT/53") ~ "BADAWA LAYOUT/53",
           ea_cluster %in% c("AGUGU_020/02", "AGUGU_020/2") ~ "AGUGU 020/02",
           ea_cluster %in% c("BASHORUN _011/13", "BASHORUN_011/13") ~ "BASHORUN 011/13",
           ea_cluster %in% c("BASHORUN_112/036", "BASHORUN_112/36") ~ "BASHORUN 112/36",
           ea_cluster %in% c("GIGINYU C/29", "GIGINYUC/29", "GINGIYU C/29") ~ "GIGINYU C/29",
           ea_cluster %in% c("FORESTREE 1392", "FORESTRY/1392") ~ "FORESTRY/1392",
           ea_cluster %in% c("1358YAMADAWA") ~ "YAMADAWA/1358",
           ea_cluster %in% c("AGUGU  042/11") ~ "AGUGU 042/11",
           ea_cluster %in% c("BADAWA LAYOUT/048", "BADAWA/48") ~ "BADAWA/48",
           ea_cluster %in% c("FORRESTRY/1390", "FORESTRY/1390") ~ "FORESTRY/1390",
           ea_cluster %in% c("YAMADAWA 1408", "YAMADAWA/1408") ~ "YAMADAWA/1408",
           ea_cluster %in% c("BASHORUN  011/13", "BASHORUN 011/13") ~ "BASHORUN 011/13",
           ea_cluster %in% c("BASHORUN  016/27") ~ "BASHORUN 016/27",
           ea_cluster %in% c("BASHORUN  112/36") ~ "BASHORUN 112/36",
           ea_cluster %in% c("YAMADAWA 1362") ~ "YAMADAWA/1362",
           ea_cluster %in% c("AGUGU 020/02", "AGUGU 020/2") ~ "AGUGU 020/2",
           ea_cluster %in% c("BASHORUN 068/02", "BASHORUN 068/2") ~ "BASHORUN 068/2",
           ea_cluster %in% c("BASHORUN 112/036", "BASHORUN 112/36") ~ "BASHORUN 112/36",
           ea_cluster %in% c("GINGIYU/37") ~ "GIGINYU/37",
           ea_cluster %in% c("JA'EN JIGAWA 1422", "JAEN JIGAWA/1422") ~ "JAEN JIGAWA/1422",
           ea_cluster %in% c("UNGUWAR JAKADA 1432", "UNGUWAR JAKADA/1432") ~ "UNGUWAR JAKADA/1432",
           ea_cluster %in% c("UNGUWAR WAMBAI 1438", "UNGUWAR WANBAI/1438") ~ "UNGUWAR WAMBAI/1438",
           ea_cluster %in% c("KAWO CIKI GARI/25", "KAWO CIKIN/25", "KAWOCIKI/25", "KAWO CIKI/25") ~ "KAWO CIKI GARI/25",
           ea_cluster %in% c("KAWO CIKI GARI/23", "KAWO CIKI/23", "KAWO/23", "KAWOCIKI/23") ~ "KAWO CIKI GARI/23",
           ea_cluster %in% c("KAWO CIKI GARI/24", "KAWO CIKI/24") ~ "KAWO CIKI GARI/24",
           ea_cluster %in% c("KAWOKUDU/27", "KAWO KUDU/27") ~ "KAWO KUDU/27",
           ea_cluster %in% c("AGUGU 018/28", "AGUGU 18/28") ~ "AGUGU 018/28",
           ea_cluster %in% c("AGUGU 056/15", "AGUGU 56/15") ~ "AGUGU 056/15",
           ea_cluster %in% c("BASHORUN 042/18", "BASHORUN 42/18") ~ "BASHORUN 042/18",
           ea_cluster %in% c("KAWO MAIGARI/28", "KAWOMAIGARI/28") ~ "KAWO MAIGARI/28",
           TRUE ~ ea_cluster
         ))

# longitude coord in "DORAYI BABBA/1376" is missing a decimal
fup_long <- fup_long %>% 
  mutate(long = case_when(
    ea_cluster == "DORAYI BABBA/1376" ~ "8.4634988",
    TRUE ~ long
  ))

# calculate centroids from raw data
longitudinal_centroids <- extract_centroids(fup_long)

# filter non-missing results and calculate TPR by EA
tpr_by_ea_long <- fup_long %>%
  filter(!is.na(rdt_result)) %>%
  group_by(ea_cluster) %>%
  summarise(
    total_tests = n(),
    num_positive_tests = sum(rdt_result == "positive"),
    tpr = num_positive_tests / total_tests,
    .groups = "drop"
  )

# merge centroids into summarized EA-level data
final_longitudinal <- left_join(tpr_by_ea_long, longitudinal_centroids, by = "ea_cluster")

# create formatted table
final_longitudinal <- make_table(final_longitudinal)

write.csv(final_longitudinal, file = file.path(OutputDir, "longitudinal_tpr_by_EA.csv"), row.names = FALSE)



