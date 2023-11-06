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

write.csv("C:/Users/15139/Downloads/rvs.csv")


