suppressMessages({
    source('common_funcs.R')
})



# my collection (owned games + wishlisted games)
# https://boardgamegeek.com/collection/user/alizat
my_collection <- collection('alizat')



# my board game collection (owned games)
my_collection_owned <- my_collection %>% filter(owned == "1")
View(my_collection_owned)



# games from my wishlist that can accommodate 5 or more players
my_collection_wishlist <- 
    my_collection %>% 
    filter(owned == '0', want_to_buy == '1' | want_to_play == '1')
my_collection_wishlist_5_plus_players <- 
    my_collection_wishlist %>% 
    filter(as.numeric(max_players) >= 5) %>% 
    select(item_id, item_name, want_to_play, want_to_buy, min_players, max_players, min_play_time, rating_avg, rating_bayes_avg) %>% 
    arrange(rating_avg) %>%
    print(n = 1000)
View(my_collection_wishlist_5_plus_players)



# supplementary info for my owned games
num_games_owned <- nrow(my_collection_owned)
batch_size <- 10
owned_games_info <- tibble()
for (i in seq(1, num_games_owned, batch_size)) {
    owned_games_info <- rbind(owned_games_info, thing(my_collection_owned$item_id[i:min(i + batch_size - 1, num_games_owned)]))
    Sys.sleep(1)
}



# board game categories/mechanics of my owned games
my_bg_categories <- owned_games_info$category %>% unlist() %>% unique() %>% sort()
my_bg_mechanics  <- owned_games_info$mechanic %>% unlist() %>% unique() %>% sort()
print(my_bg_categories)
print(my_bg_mechanics)



# board game categories/mechanics that do NOT exist in my owned games
all_bg_categories <- categories()$category_name
all_bg_mechanics  <- mechanics()$mechanic_name
remaining_bg_categories <- setdiff(all_bg_categories, my_bg_categories)
remaining_bg_mechanics  <- setdiff(all_bg_mechanics,  my_bg_mechanics)
print(remaining_bg_categories)
print(remaining_bg_mechanics)



# any of 'remaining_bg_categories' or 'remaining_bg_mechanics' in my wishlisted games?
num_games_wishlist <- nrow(my_collection_wishlist)
batch_size <- 10
wishlist_games_info <- tibble()
for (i in seq(1, num_games_wishlist, batch_size)) {
    wishlist_games_info <- rbind(wishlist_games_info, thing(my_collection_wishlist$item_id[i:min(i + batch_size - 1, num_games_wishlist)]))
    Sys.sleep(1)
}
my_wishlist_missing_categories_mechanics <- 
    wishlist_games_info %>% 
    select(name, category, mechanic) %>% 
    filter(map_lgl(category, ~any(remaining_bg_categories %in% .x)) | map_lgl(mechanic, ~any(remaining_bg_mechanics %in% .x))) %>% 
    mutate(
        missing_category = map_chr(category, ~ intersect(.x, remaining_bg_categories) %>% paste(collapse = ', ')), 
        missing_mechanic = map_chr(mechanic, ~ intersect(.x, remaining_bg_mechanics)  %>% paste(collapse = ', ')), 
        # category = category %>% map_chr(~ .x %>% paste(collapse = ', ')),
        # mechanic = mechanic %>% map_chr(~ .x %>% paste(collapse = ', '))
    ) %>% 
    select(name, missing_category, missing_mechanic)  # , category, mechanic
View(my_wishlist_missing_categories_mechanics)



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



# get current top 1K games at BGG & observe the years they were published
ranked_games_ids_by_year <- read_rds('ranked_games_ids_by_year.rds')
top_1K_games_ids <- top_k_games_ids(k = 1000)
top_1K_games_years_published <- 
    tibble(
    game_id = top_1K_games_ids, 
    year_published = 
        map_chr(
            top_1K_games_ids, 
            function(my_game) {
                year_published <- '< 1990'
                year_indx <- which(map_lgl(ranked_games_ids_by_year, ~ my_game %in% .x))
                if (length(year_indx)) {
                    year_published <- names(ranked_games_ids_by_year)[year_indx[[1]]]
                }
                year_published
            }
        )
)
p <- 
    top_1K_games_years_published %>% 
    count(year_published) %>%
    ggplot(aes(year_published, n)) +
    geom_bar(stat = 'identity')
print(p)



# 20 most occurring families in the current top 1K games
batch_size <- 10
top_1K_games_info <- tibble()
for (i in seq(1, 1000, batch_size)) {
    top_1K_games_info <- rbind(top_1K_games_info, thing(top_1K_games_ids[i:min(i + batch_size - 1, 1000)]))
    Sys.sleep(1)
}
top_1K_games_info$family %>% unlist() %>% table() %>% sort(decreasing = TRUE) %>% head(20)
