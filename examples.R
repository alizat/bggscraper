suppressMessages({
    source('common_funcs.R')
})


SELECTED_USERNAME <- 'alizat'  # my BGG username
print(glue('Analyzing BGG collection of user: {SELECTED_USERNAME}'))
print(glue(""))
print(glue(""))




# my collection (owned games + wishlisted games)
# https://boardgamegeek.com/collection/user/alizat
my_collection <- collection(SELECTED_USERNAME)



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
    arrange(rating_avg)
View(my_collection_wishlist_5_plus_players)



# supplementary info for my owned games
print(glue("*** OWNED GAMES INFO ***"))
print(glue(""))
print(glue("Getting supplementary info for my owned games..."))
owned_games_info <- thing(my_collection_owned$item_id)
print(glue(""))
print(owned_games_info)
print(glue(""))
print(glue(""))



# board game categories/mechanics of my owned games
print(glue("*** OWNED GAMES CATEGORIES/MECHANICS ***"))
my_bg_categories <- owned_games_info$category %>% unlist() %>% unique() %>% sort()
print(glue(""))
print(glue("Categories:"))
print(my_bg_categories)
print(glue(""))
print(glue("Mechanics:"))
my_bg_mechanics  <- owned_games_info$mechanic %>% unlist() %>% unique() %>% sort()
print(my_bg_mechanics)
print(glue(""))
print(glue(""))



# board game categories/mechanics that do NOT exist in my owned games
print(glue("*** REMAINING BOARD GAME CATEGORIES/MECHANICS ***"))
all_bg_categories <- categories()$category_name
all_bg_mechanics  <- mechanics()$mechanic_name
remaining_bg_categories <- setdiff(all_bg_categories, my_bg_categories)
remaining_bg_mechanics  <- setdiff(all_bg_mechanics,  my_bg_mechanics)
print(glue(""))
print(glue("Remaining Categories:"))
print(remaining_bg_categories)
print(glue(""))
print(glue("Remaining Mechanics:"))
print(remaining_bg_mechanics)
print(glue(""))
print(glue(""))



# any of 'remaining_bg_categories' or 'remaining_bg_mechanics' in my wishlisted games?
print(glue("*** WISHLISTED GAMES WITH MISSING CATEGORIES/MECHANICS ***"))
print(glue(""))
print(glue("Getting supplementary info for my wishlisted games..."))
wishlist_games_info <- thing(my_collection_wishlist$item_id)
my_wishlist_missing_categories_mechanics <-
    wishlist_games_info %>%
    inner_join(my_collection_wishlist %>% select(item_name, want_to_buy, want_to_play), by = c('name' = 'item_name')) %>%
    filter(map_lgl(category, ~any(remaining_bg_categories %in% .x)) | map_lgl(mechanic, ~any(remaining_bg_mechanics %in% .x))) %>%
    mutate(
        missing_categories     = map_chr(category, ~ intersect(.x, remaining_bg_categories) %>% paste(collapse = ', ')),
        num_missing_categories = map_int(missing_categories, ~ str_count(.x, ', ') + 1),
        num_missing_categories = if_else(missing_categories == '', 0L, num_missing_categories),
        all_categories         = category %>% map_chr(~ .x %>% paste(collapse = ', ')),

        missing_mechanics      = map_chr(mechanic, ~ intersect(.x, remaining_bg_mechanics)  %>% paste(collapse = ', ')),
        num_missing_mechanics  = map_int(missing_mechanics,  ~ str_count(.x, ', ') + 1),
        num_missing_mechanics  = if_else(missing_mechanics == '', 0L, num_missing_mechanics),
        all_mechanics          = mechanic %>% map_chr(~ .x %>% paste(collapse = ', '))
    ) %>%
    select(
        name, min_players, max_players, playing_time, rating_avg, rating_bayes_avg,
        want_to_buy, want_to_play,
        missing_categories, num_missing_categories, #all_categories,
        missing_mechanics, num_missing_mechanics, #all_mechanics
    ) %>%
    mutate(across(c(min_players:rating_bayes_avg), as.numeric)) %>%
    mutate_if(is.numeric, as.numeric) %>%
    arrange(desc(num_missing_categories), desc(num_missing_mechanics), desc(rating_bayes_avg))
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
    geom_bar(stat = 'identity') +
    geom_text(aes(x = year_published, y = n + 2, label = n)) +
    labs(
        title = "Year Distribution of the Top 1K Games",
        x = "Year Published",
        y = "Number of Games"
    )
print(p)



# 20 most occurring ... in the current top 1K games
print(glue("*** TOP 1K GAMES ***"))
print(glue(""))
print(glue("20 most occurring families in the top 1K games:"))
top_1K_games_info$family    %>% unlist()           %>% table() %>% sort(decreasing = TRUE) %>% head(20) %>% print()
print(glue(""))
print(glue("20 most occurring designers in the top 1K games:"))
top_1K_games_info$designer  %>% unlist()           %>% table() %>% sort(decreasing = TRUE) %>% head(20) %>% print()
print(glue(""))
print(glue("20 most occurring mechanics in the top 1K games:"))
top_1K_games_info$mechanic  %>% unlist()           %>% table() %>% sort(decreasing = TRUE) %>% head(20) %>% print()
print(glue(""))
print(glue("20 most occurring categories in the top 1K games:"))
top_1K_games_info$category  %>% unlist()           %>% table() %>% sort(decreasing = TRUE) %>% head(20) %>% print()
print(glue(""))
print(glue("20 most occurring publishers in the top 1K games:"))
top_1K_games_info$publisher %>% map_chr(~ .x[[1]]) %>% table() %>% sort(decreasing = TRUE) %>% head(20) %>% print()
print(glue(""))
print(glue(""))



# Distribution of average game weight in the current top 1K games
top_1K_games_info$avg_weight %>%
    as.numeric() %>%
    hist(breaks = seq(1, 5, 0.25),
         main = 'Average weights of top 1K games',
         xlab = 'Average weight',
         ylab = 'Frequency')



# TODO: Categories/Mechanics that are NOT in my collection or wishlisted games + Examples of them from the top 1K games
# TODO: Below, get reviews for a random board game from your collection



# Get text reviews of a game / game expansion
print(glue("*** REVIEWS FOR AN EXAMPLE BOARD GAME ***"))
game_id <- 40849  # Pandemic: On the Brink (expansion)
game_forumlist <- forumlist(40849)  # get list of forums for this game
# from these forums, we are particularly interested in the "Reviews" forum
game_reviews_forumlist_id <-
    game_forumlist %>%
    filter(title == 'Reviews') %>%
    pull(forum_id)
# get reviews
game_reviews_list <- forum(game_reviews_forumlist_id)
game_reviews_threads <- game_reviews_list$thread_id %>% map(thread)
game_reviews <-
    game_reviews_threads %>%
    map_dfr(~ .x[1,])  # first entry in each thread is the actual review
# clean up the HTML-like tags in the reviews
review_body_cleaner <- function(text)  {
    text %>%
        str_squish() %>%
        str_replace_all('<b>|</b>', '**') %>%
        str_replace_all('<i>|</i>', '_') %>%
        str_remove_all('<center>|</center>|\\[|\\]') %>%
        str_replace_all('<br/>', '\n')
}
game_reviews$body <- review_body_cleaner(game_reviews$body)
# display a couple of reviews
print(glue(''))
print(glue('>>>>> Review 1 <<<<<'))
print(glue(game_reviews$body[[2]]))
print(glue(''))
print(glue('>>>>> Review 2 <<<<<'))
print(glue(game_reviews$body[[4]]))
print(glue(''))
print(glue(''))



# suggest a bunch of random mechanics & categories
print(glue("*** BOARDGAMIZER: RANDOMLY PICK A FEW CATEGORIES/MECHANICS ***"))
boardgamizer <- function(num_categories, num_mechanics) {
    random_categories <- dplyr::sample_n(categories(), num_categories)
    random_mechanics  <- dplyr::sample_n(mechanics(),  num_mechanics)

    return_me <-
        rbind(
            tibble::tibble(type = 'category', id = random_categories[[1]], name = random_categories[[2]]),
            tibble::tibble(type = 'mechanic', id = random_mechanics[[1]],  name = random_mechanics[[2]])
        )

    return_me
}
print(boardgamizer(num_categories = 3, num_mechanics = 5))
