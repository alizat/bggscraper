suppressMessages({
    source('common_funcs.R')
})



# get all ranked BGG games ids from 1990 till 2024
ranked_games_ids_by_year <- list()
for (y in 1990:2025) {
    print(glue(''))
    print(glue('Retrieving {y}:'))
    ranked_games_ids_y <- year_ranked_games_ids(y, wait = 5)
    ranked_games_ids_by_year[[as.character(y)]] <- ranked_games_ids_y
    Sys.sleep(5)
}
ranked_games_ids_by_year %>% write_rds('ranked_games_ids_by_year.rds')



# get supplementary info of top 1K BGG games
top_1K_games_ids <- top_k_games_ids(k = 1000)
top_1K_games_ids %>% write_rds('top_1K_games_ids.rds')
top_1K_games_info <- thing(top_1K_games_ids)
top_1K_games_info %>% write_rds('top_1K_games_info.rds')
