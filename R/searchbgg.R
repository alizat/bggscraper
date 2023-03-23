#' BGG Search
#'
#' @param query keywords of item that you are looking for
#' @param type type of item. Possible values are \code{"rpgitem"},
#'   \code{"videogame"}, \code{"boardgame"}, \code{"boardgameaccessory"} and
#'   \code{"boardgameexpansion"}. Specifying multiple types separated by commas
#'   is allowed (see examples below).
#' @param exact whether the returned result should match the search query
#'   perfectly (default is \code{0})
#'
#' @return
#' Data frame containing search results
#'
#' @examples
#' searchbgg(query = 'shipshape')
#' searchbgg(query = 'shipshape', type = 'boardgame')
#' searchbgg(query = 'dune imperium')
#' searchbgg(query = 'water', type = 'rpgitem')
searchbgg <- function(query,
                      type = NULL,
                      exact = 0) {
    # adjust query (if necessary)
    query <- stringr::str_squish(query)
    query <- stringr::str_replace_all(query, ' ', '+')

    # features of interest
    my_features <-
        list(
            item_id = '::id',
            item_type = 'name::type',
            item_name = 'name::value',
            item_yearpublished = 'yearpublished::value'
        )

    # if type is NULL, it will be taken to mean BOTH board games and expansions
    if (is.null(type) || type == 'boardgame') {
        # both board games and expansions
        link <- glue::glue('https://boardgamegeek.com/xmlapi2/search?query={query}&type=boardgame')
        page <- rvest::read_html(link)
        all_items <- rvest::html_elements(page, 'item')
        query_result <- features_extractor(all_items, my_features)

        # just expansions
        link <- glue::glue('https://boardgamegeek.com/xmlapi2/search?query={query}&type=boardgameexpansion')
        page <- rvest::read_html(link)
        expansions     <- rvest::html_elements(page, 'item')
        expansions_ids <- rvest::html_attr(expansions, 'id')

        # if 'type' was specified as 'boardgame'...
        if (!is.null(type) && type == 'boardgame') {
            # ... exclude expansions
            query_result <- dplyr::filter(query_result, !('item_id' %in% expansions_ids))
        } else {
            # otherwise, keep all items and adjust type in 'all_items_types'
            query_result$item_type[query_result$item_id %in% expansions_ids] <- 'boardgameexpansion'
        }

    } else {
        # type was specified as something other than 'boardgame'
        link <- glue::glue('https://boardgamegeek.com/xmlapi2/search?query={query}&type={type}')
        page <- rvest::read_html(link)
        all_items       <- rvest::html_elements(page, 'item')
        query_result <- features_extractor(all_items, my_features)
    }

    # adjust item type in query results
    if (is.null(type)) {
        type <- 'boardgame'
    }
    query_result$item_type[query_result$item_type == 'primary'] <- type

    # return
    query_result
}
