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
my_collection %>% 
    filter(as.numeric(max_players) >= 5, owned == '0', want_to_buy == '1' | want_to_play == '1') %>% 
    select(item_id, item_name, want_to_play, want_to_buy, min_players, max_players, min_play_time, rating_avg, rating_bayes_avg) %>% 
    arrange(rating_avg) %>%
    print(n = 1000)



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
