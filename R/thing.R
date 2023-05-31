#' Thing Info
#'
#' @description \code{thing()} retrieves the available information of the
#'   specified items (i.e. board games and board game expansions).
#'
#' @param ids IDs of the items that you wish retrieve the information for.
#'
#' @return
#' Data frame containing the details of the items for which the IDs were
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
            name = 'name[type=primary]::value',
            year_published = 'yearpublished::value',
            description = 'description',
            min_players = 'minplayers::value',
            max_players = 'maxplayers::value',
            min_playtime = 'minplaytime::value',
            max_playtime = 'maxplaytime::value',
            playing_time = 'playingtime::value',
            min_age = 'minage::value',
            category = 'link[type=boardgamecategory]::value',
            mechanic = 'link[type=boardgamemechanic]::value',
            family = 'link[type=boardgamefamily]::value',
            designer = 'link[type=boardgamedesigner]::value',
            artist = 'link[type=boardgameartist]::value',
            publisher = 'link[type=boardgamepublisher]::value',
            rating_users_rated = 'ratings > usersrated::value',
            rating_avg = 'ratings > average::value',
            rating_bayes_avg = 'ratings > bayesaverage::value',
            rank = 'ratings > ranks > rank[name="boardgame"]::value',
            owned = 'ratings > owned::value',
            trading = 'ratings > trading::value',
            wanting = 'ratings > wanting::value',
            wishing = 'ratings > wishing::value',
            num_comments = 'ratings > numcomments::value',
            num_weights = 'ratings > numweights::value',
            avg_weight = 'ratings > averageweight::value'
        )

    # extract features
    items_details <- features_extractor(items, features_to_extract)

    # loop over items
    items_details_dfs <- vector(mode = 'list', length = length(items_details))
    for (i in 1:length(items_details)) {
        # current item details
        items_details_i <- items_details[[i]]

        # convert to data frame
        df_i <- purrr::map2_dfr(names(items_details_i), items_details_i, ~ tibble::tibble(feature = .x, value = .y))
        df_i[['id']]   <- rvest::html_attr(items[[i]], 'id')
        df_i <- dplyr::select(df_i, dplyr::all_of(c('id', 'feature', 'value')))
        df_i <- rbind(df_i, dplyr::tibble(id = df_i[['id']], feature = 'type', value = 'boardgame'))

        # save features
        items_details_dfs[[i]] <- df_i
    }

    # return
    return_me <- dplyr::bind_rows(items_details_dfs)
    return_me <- dplyr::distinct(return_me)
    return_me <- tidyr::pivot_wider(return_me, names_from = feature, values_from = value, values_fn = list)
    modify_these_columns <-
        setdiff(colnames(return_me),
                c('category', 'mechanic', 'family', 'designer', 'artist', 'publisher'))
    return_me <- dplyr::mutate(return_me, dplyr::across(dplyr::all_of(modify_these_columns), unlist))
    return_me
}

# References
#   https://stackoverflow.com/questions/61053827/web-scraping-boardgamegeek-with-rvest
#   https://boardgamegeek.com/wiki/page/BGG_XML_API2
