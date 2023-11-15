suppressMessages(library(dplyr))
library(baseballr)
library(ggplot2)

sc_2023 <- read.csv("C:/Users/15139/Downloads/2023pbp.csv")

for(col_name in names(sc_2023)) {
  if(is.character(sc_2023[[col_name]])) {
    sc_2023[[col_name]][sc_2023[[col_name]] == "NULL"] <- NA
  }
}

sc_2023 <- sc_2023 %>% mutate(bat_team = ifelse(inning_topbot == 'Bot', home_team, away_team))

rel_data <- sc_2023 %>% filter(inning < 10, balls < 4, strikes < 3) %>% 
  dplyr::mutate(on_1b = ifelse(is.na(on_1b), 0, 1),
                on_2b = ifelse(is.na(on_2b), 0 ,1),
                on_3b = ifelse(is.na(on_3b), 0, 1)) %>%
  dplyr::mutate(out_base_count_state = paste0(outs_when_up, '_', on_1b, on_2b, on_3b, '_', balls, strikes),
                game_inning = paste0(game_pk, "_", inning)) %>%
  dplyr::mutate(game_inning_ab = paste0(game_inning, "_", at_bat_number)) %>%
  dplyr::mutate(game_inning_ab_pitch = paste0(game_inning, "_", at_bat_number, "_", pitch_number))

non_outs <- c("double", "field_error", "fielders_choice", "hit_by_pitch", "home_run", "single", "triple", "walk")

rel_data <- rel_data %>% dplyr::mutate(events = ifelse(events %in% non_outs, events, "out"))

rel_data <- rel_data %>%
  dplyr::group_by(game_inning) %>%
  dplyr::mutate(post_inning_runs = max(bat_score)) %>%
  dplyr::ungroup()

rel_data <- rel_data %>% dplyr::filter(!("stealing" %in% events))

rel_data <- rel_data %>% dplyr::mutate(runs_post_ab = post_inning_runs - bat_score,
                                       next_at_bat_number = at_bat_number + 1,
                                       next_game_inning_ab = paste0(game_inning, "_", next_at_bat_number),
                                       runs_from_event = post_bat_score - bat_score)

re_matrix <- rel_data %>%
  dplyr::group_by(out_base_count_state) %>%
  dplyr::summarize(runs_expected = mean(runs_post_ab),
                   occurrences = n())

limited_cols <- rel_data %>% select(game_pk, at_bat_number, pitch_number, pitch_type_condensed, events, description, game_inning,
                                    game_inning_ab, out_base_count_state, balls, strikes, runs_post_ab, runs_from_event)

rvs <- merge(limited_cols, re_matrix %>% select(out_base_count_state, runs_expected), by="out_base_count_state") %>%
  arrange(game_pk, at_bat_number, pitch_number) %>%
  dplyr::mutate(next_out_base_count_state = lead(out_base_count_state),
                next_runs_expected = ifelse(next_out_base_count_state == "0_000_00", 0, lead(runs_expected)),
                delta_re = next_runs_expected - runs_expected + runs_from_event)

pitcher_movement_sum <- rel_data %>% 
  mutate(
    release_speed = as.numeric(release_speed),
    release_pos_x = as.numeric(release_pos_x),
    release_pos_y = as.numeric(release_pos_y),
    release_pos_z = as.numeric(release_pos_z),
    pfx_x = as.numeric(pfx_x),
    pfx_z = as.numeric(pfx_z)
  ) %>%
  filter(pitch_type_condensed %in% c("CB", "CH", "CT", "FF", "SI", "SL", "KN")) %>% 
  group_by(pitcher, pitch_type_condensed) %>%
  summarise(avgVelo = mean(release_speed, na.rm=TRUE),
            avgRPX = mean(release_pos_x, na.rm=TRUE),
            avgRPZ = mean(release_pos_z, na.rm=TRUE),
            avgRPY = mean(release_pos_y, na.rm=TRUE),
            avgHBRK = mean(pfx_x, na.rm=TRUE),
            avgVBRK = mean(pfx_z, na.rm=TRUE),
            sdVelo = sd(release_speed, na.rm=TRUE),
            sdRPX = sd(release_pos_x, na.rm=TRUE),
            sdRPZ = sd(release_pos_z, na.rm=TRUE),
            sdRPY = sd(release_pos_y, na.rm=TRUE),
            sdHBRK = sd(pfx_x, na.rm=TRUE),
            sdVBRK = sd(pfx_z, na.rm=TRUE)
            )

pitcher_usage_sum <- rel_data %>%
  group_by(pitcher) %>%
  mutate(totalPitches = n()) %>%
  ungroup() %>%
  group_by(pitcher, pitch_type_condensed) %>%
  summarise(pitches = n(),
            totalPitches = mean(totalPitches),
            usagePCT = pitches/totalPitches) %>%
  filter(totalPitches > 500 & usagePCT > 0.03)

pitcher_total_sum <- merge(pitcher_usage_sum, pitcher_movement_sum, by=c("pitcher", "pitch_type_condensed"))

generic_pitcher_sum <- rel_data %>%
  mutate(
    release_speed = as.numeric(release_speed),
    release_pos_x = as.numeric(release_pos_x),
    release_pos_y = as.numeric(release_pos_y),
    release_pos_z = as.numeric(release_pos_z),
    pfx_x = as.numeric(pfx_x),
    pfx_z = as.numeric(pfx_z)
  ) %>%
  filter(pitch_type_condensed %in% c("CB", "CH", "CT", "FF", "SI", "SL", "KN")) %>% 
  group_by(p_throws, pitch_type_condensed) %>%
  summarise(avgVelo = mean(release_speed, na.rm=TRUE),
            avgRPX = mean(release_pos_x, na.rm=TRUE),
            avgRPZ = mean(release_pos_z, na.rm=TRUE),
            avgRPY = mean(release_pos_y, na.rm=TRUE),
            avgHBRK = mean(pfx_x, na.rm=TRUE),
            avgVBRK = mean(pfx_z, na.rm=TRUE),
            sdVelo = sd(release_speed, na.rm=TRUE),
            sdRPX = sd(release_pos_x, na.rm=TRUE),
            sdRPZ = sd(release_pos_z, na.rm=TRUE),
            sdRPY = sd(release_pos_y, na.rm=TRUE),
            sdHBRK = sd(pfx_x, na.rm=TRUE),
            sdVBRK = sd(pfx_z, na.rm=TRUE)
  )

hitter_result_sum <- rel_data %>%
  mutate(
    launch_speed = as.numeric(launch_speed),
    launch_angle = as.numeric(launch_angle),
    zone = as.numeric(zone),
    inZone = ifelse(zone < 10, 1, 0),
    swing = ifelse(description %in% c("hit_into_play", "foul", "swinging_strike", "swinging_strike_blocked", "foul_tip", "foul_bunt", "missed_bunt", "bunt_foul_tip"), 1, 0)
  ) %>%
  filter(pitch_type_condensed %in% c("CB", "CH", "CT", "FF", "SI", "SL", "KN")) %>%
  group_by(batter, pitch_type_condensed, p_throws) %>%
  summarise(avgEV = mean(launch_speed, na.rm=TRUE),
            avgLA = mean(launch_angle, na.rm=TRUE),
            zSwingPcT = sum(swing[inZone == 1], na.rm=TRUE) / sum(inZone == 1, na.rm=TRUE),
            oSwingPCT = sum(swing[inZone == 0], na.rm=TRUE) / sum(inZone == 0, na.rm=TRUE)
  )

hitter_count_sum <- rel_data %>%
  group_by(batter) %>%
  mutate(totalPitches = n()) %>% 
  ungroup() %>%
  group_by(batter, pitch_type_condensed, p_throws) %>%
  summarise(pitches = n(),
            totalPitches = mean(totalPitches),
            BIP = sum(description == "hit_into_play")) %>%
  filter(totalPitches > 500, pitches > 50, BIP > 10)

hitter_total_sum <- merge(hitter_count_sum, hitter_result_sum, by=c("batter", "pitch_type_condensed", "p_throws"))

generic_hitter_sum <- rel_data %>%
  mutate(
    launch_speed = as.numeric(launch_speed),
    launch_angle = as.numeric(launch_angle),
    zone = as.numeric(zone),
    inZone = ifelse(zone < 10, 1, 0),
    swing = ifelse(description %in% c("hit_into_play", "foul", "swinging_strike", "swinging_strike_blocked", "foul_tip", "foul_bunt", "missed_bunt", "bunt_foul_tip"), 1, 0)
  ) %>%
  filter(pitch_type_condensed %in% c("CB", "CH", "CT", "FF", "SI", "SL", "KN")) %>%
  group_by(stand, pitch_type_condensed, p_throws) %>%
  summarise(avgEV = mean(launch_speed, na.rm=TRUE),
            avgLA = mean(launch_angle, na.rm=TRUE),
            zSwingPcT = sum(swing[inZone == 1], na.rm=TRUE) / sum(inZone == 1, na.rm=TRUE),
            oSwingPCT = sum(swing[inZone == 0], na.rm=TRUE) / sum(inZone == 0, na.rm=TRUE)
  )

write.csv("C:/Users/15139/Downloads/rvs.csv")


