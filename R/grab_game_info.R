grab_game_info <- function(game_link) {
    # game id
    game_id <- stringr::str_extract(game_link, '/[:digit:]+/')
    game_id <- stringr::str_replace_all(game_id, '/', '')

    # game details
    game_xml_link <- paste0('https://www.boardgamegeek.com/xmlapi2/thing?id=', game_id, '&stats=1')
    game_details <- rvest::read_html(game_xml_link)

    # features to extract
    features <-
        list(
            'name[type=primary]' = '',
            'yearpublished' = '',
            'description' = '',
            'minplayers' = '',
            'maxplayers' = '',
            'playingtime' = '',
            'minplaytime' = '',
            'maxplaytime' = '',
            'minage' = '',
            'link[type=boardgamecategory]' = '',
            'link[type=boardgamemechanic]' = '',
            'link[type=boardgamefamily]' = '',
            'link[type=boardgamedesigner]' = '',
            'link[type=boardgameartist]' = '',
            'link[type=boardgamepublisher]' = '',
            'ratings > usersrated' = '',
            'ratings > average' = '',
            'ratings > bayesaverage' = '',
            'ratings > ranks > rank[name="boardgame"]' = '',
            'ratings > owned' = '',
            'ratings > trading' = '',
            'ratings > wanting' = '',
            'ratings > wishing' = '',
            'ratings > numcomments' = '',
            'ratings > numweights' = '',
            'ratings > averageweight' = ''
        )

    # extract features
    for (feat in names(features)) {
        features[[feat]] <- rvest::html_elements(game_details, feat)
        if (feat == 'description') {
            features[[feat]] <- rvest::html_text(features[[feat]])

        } else {
            features[[feat]] <- as.character(rvest::html_attr(features[[feat]], "value"))
        }
    }

    # adjust features names
    names(features)[names(features) == 'name[type=primary]']            <- 'name'
    names(features)[names(features) == 'yearpublished']                 <- 'yearpublished'
    names(features)[names(features) == 'minplayers']                    <- 'min_players'
    names(features)[names(features) == 'maxplayers']                    <- 'max_players'
    names(features)[names(features) == 'playingtime']                   <- 'playing_time'
    names(features)[names(features) == 'minplaytime']                   <- 'min_playtime'
    names(features)[names(features) == 'maxplaytime']                   <- 'max_playtime'
    names(features)[names(features) == 'minage']                        <- 'min_age'
    names(features)[names(features) == 'link[type=boardgamecategory]']  <- 'category'
    names(features)[names(features) == 'link[type=boardgamemechanic]']  <- 'mechanic'
    names(features)[names(features) == 'link[type=boardgamefamily]']    <- 'family'
    names(features)[names(features) == 'link[type=boardgamedesigner]']  <- 'designer'
    names(features)[names(features) == 'link[type=boardgameartist]']    <- 'artist'
    names(features)[names(features) == 'link[type=boardgamepublisher]'] <- 'publisher'
    names(features)[names(features) == 'ratings > usersrated']          <- 'users_rated'
    names(features)[names(features) == 'ratings > average']             <- 'avg_rating'
    names(features)[names(features) == 'ratings > bayesaverage']        <- 'bayes_avg_rating'
    names(features)[names(features) == 'ratings > ranks > rank[name="boardgame"]'] <- 'rank'
    names(features)[names(features) == 'ratings > owned']               <- 'owned'
    names(features)[names(features) == 'ratings > trading']             <- 'trading'
    names(features)[names(features) == 'ratings > wanting']             <- 'wanting'
    names(features)[names(features) == 'ratings > wishing']             <- 'wishing'
    names(features)[names(features) == 'ratings > numcomments']         <- 'num_comments'
    names(features)[names(features) == 'ratings > numweights']          <- 'num_weights'
    names(features)[names(features) == 'ratings > averageweight']       <- 'avg_weight'


    # convert to data frame
    features <- purrr::map2_dfr(names(features), features, ~ tibble::tibble(feature = .x, value = .y))
    features <- dplyr::mutate(features, game_id = game_id)
    features <- dplyr::select(features, game_id, feature, value)


    # return features
    features
}

# References
#   https://stackoverflow.com/questions/61053827/web-scraping-boardgamegeek-with-rvest
#   https://boardgamegeek.com/wiki/page/BGG_XML_API2
