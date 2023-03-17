#' Thing Info
#'
#' @description \code{thing()} retrieves the available information of the
#'   specified items (i.e. board games and board game expansions).
#'
#' @param ids IDs of the items that you wish retrieve the information for.
#'
#' @return
#' A data frame containing the details of the items for which the IDs were
#' supplied.
#'
#' @examples
#' games_details <- thing(c(158600, 194607, 40849))
#' games_details
thing <- function(ids) {
    # merge ids
    ids <- paste(ids, collapse = ',')

    # game details
    link <- paste0('https://www.boardgamegeek.com/xmlapi2/thing?id=', ids, '&stats=1')
    items <- rvest::html_elements(rvest::read_html(link), 'item')

    # features to extract
    features_to_extract <-
        c(
            'name[type=primary]',
            'yearpublished',
            'description',
            'minplayers',
            'maxplayers',
            'minplaytime',
            'maxplaytime',
            'playingtime',
            'minage',
            'link[type=boardgamecategory]',
            'link[type=boardgamemechanic]',
            'link[type=boardgamefamily]',
            'link[type=boardgamedesigner]',
            'link[type=boardgameartist]',
            'link[type=boardgamepublisher]',
            'ratings > usersrated',
            'ratings > average',
            'ratings > bayesaverage',
            'ratings > ranks > rank[name="boardgame"]',
            'ratings > owned',
            'ratings > trading',
            'ratings > wanting',
            'ratings > wishing',
            'ratings > numcomments',
            'ratings > numweights',
            'ratings > averageweight'
        )

    # loop over items
    items_details <- list()
    for (i in 1:length(items)) {
        # extract features
        features <- list()
        for (feat in features_to_extract) {
            features[[feat]] <- rvest::html_elements(items[[i]], feat)
            if (feat == 'description') {
                features[[feat]] <- rvest::html_text(features[[feat]])

            } else {
                features[[feat]] <- as.character(rvest::html_attr(features[[feat]], "value"))
            }
        }

        # adjust features names
        names(features)[names(features) == 'name[type=primary]']            <- 'name'
        names(features)[names(features) == 'yearpublished']                 <- 'year_published'
        names(features)[names(features) == 'minplayers']                    <- 'min_players'
        names(features)[names(features) == 'maxplayers']                    <- 'max_players'
        names(features)[names(features) == 'minplaytime']                   <- 'min_playtime'
        names(features)[names(features) == 'maxplaytime']                   <- 'max_playtime'
        names(features)[names(features) == 'playingtime']                   <- 'playing_time'
        names(features)[names(features) == 'minage']                        <- 'min_age'
        names(features)[names(features) == 'link[type=boardgamecategory]']  <- 'category'
        names(features)[names(features) == 'link[type=boardgamemechanic]']  <- 'mechanic'
        names(features)[names(features) == 'link[type=boardgamefamily]']    <- 'family'
        names(features)[names(features) == 'link[type=boardgamedesigner]']  <- 'designer'
        names(features)[names(features) == 'link[type=boardgameartist]']    <- 'artist'
        names(features)[names(features) == 'link[type=boardgamepublisher]'] <- 'publisher'
        names(features)[names(features) == 'ratings > usersrated']          <- 'rating_users_rated'
        names(features)[names(features) == 'ratings > average']             <- 'rating_avg'
        names(features)[names(features) == 'ratings > bayesaverage']        <- 'rating_bayes_avg'
        names(features)[names(features) == 'ratings > ranks > rank[name="boardgame"]'] <- 'rank'
        names(features)[names(features) == 'ratings > owned']               <- 'owned'
        names(features)[names(features) == 'ratings > trading']             <- 'trading'
        names(features)[names(features) == 'ratings > wanting']             <- 'wanting'
        names(features)[names(features) == 'ratings > wishing']             <- 'wishing'
        names(features)[names(features) == 'ratings > numcomments']         <- 'num_comments'
        names(features)[names(features) == 'ratings > numweights']          <- 'num_weights'
        names(features)[names(features) == 'ratings > averageweight']       <- 'avg_weight'

        # add item type
        features[['type']] <- rvest::html_attr(items[[i]], 'type')

        # convert to data frame
        features <- purrr::map2_dfr(names(features), features, ~ tibble::tibble(feature = .x, value = .y))
        features[['id']]   <- rvest::html_attr(items[[i]], 'id')
        features <- dplyr::select(features, dplyr::all_of(c('id', 'feature', 'value')))

        # save features
        items_details[[i]] <- features
    }

    # return
    dplyr::bind_rows(items_details)
}

# References
#   https://stackoverflow.com/questions/61053827/web-scraping-boardgamegeek-with-rvest
#   https://boardgamegeek.com/wiki/page/BGG_XML_API2
