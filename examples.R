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
owned_games_info <- thing(my_collection_owned$item_id)



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
wishlist_games_info <- thing(my_collection_wishlist$item_id)
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



# get current top 1K games at BGG & observe the years they were published
top_1K_games_info <- read_rds('top_1K_games_info.rds')
top_1K_games_years_published <- 
    top_1K_games_info %>% 
    select(year_published) %>% 
    mutate(year_published = if_else(year_published < "1990", "< 1990", year_published))
p <- 
    top_1K_games_years_published %>% 
    count(year_published) %>%
    ggplot(aes(year_published, n)) +
    geom_bar(stat = 'identity')
print(p)



# 20 most occurring families in the current top 1K games
top_1K_games_info$family %>% unlist() %>% table() %>% sort(decreasing = TRUE) %>% head(20)
