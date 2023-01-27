searchbgg <- function(query,
                      type = NULL,
                      exact = 0) {
    # if type is NULL, it will be taken to mean BOTH board games and expansions
    if (is.null(type) || type == 'boardgame') {
        # both board games and expansions
        link <- glue::glue('https://boardgamegeek.com/xmlapi2/search?query={query}&type=boardgame')
        page <- rvest::read_html(link)
        all_items       <- rvest::html_elements(page, 'item')
        all_items_ids   <- rvest::html_attr(all_items, 'id')
        all_items_types <- rvest::html_attr(rvest::html_elements(all_items, 'name'), 'type')
        all_items_names <- rvest::html_attr(rvest::html_elements(all_items, 'name'), 'value')
        all_items_year  <- rvest::html_attr(rvest::html_elements(all_items, 'yearpublished'), 'value')

        # just expansions
        link <- glue::glue('https://boardgamegeek.com/xmlapi2/search?query={query}&type=boardgameexpansion')
        page <- rvest::read_html(link)
        expansions     <- rvest::html_elements(page, 'item')
        expansions_ids <- rvest::html_attr(expansions, 'id')

        # if 'type' was specified as 'boardgame'...
        if (!is.null(type) && type == 'boardgame') {
            # ... exclude expansions
            all_items_types <- all_items_types[!(all_items_ids %in% expansions_ids)]
            all_items_names <- all_items_names[!(all_items_ids %in% expansions_ids)]
            all_items_year  <- all_items_year[!(all_items_ids %in% expansions_ids)]
            all_items_ids   <- all_items_ids[!(all_items_ids %in% expansions_ids)]
        } else {
            # otherwise, keep all items and adjust type in 'all_items_types'
            all_items_types[all_items_ids %in% expansions_ids] <- 'boardgameexpansion'
        }

    } else {
        # both board games and expansions
        link <- glue::glue('https://boardgamegeek.com/xmlapi2/search?query={query}&type={type}')
        page <- rvest::read_html(link)
        all_items       <- rvest::html_elements(page, 'item')
        all_items_ids   <- rvest::html_attr(all_items, 'id')
        all_items_types <- rvest::html_attr(rvest::html_elements(all_items, 'name'), 'type')
        all_items_names <- rvest::html_attr(rvest::html_elements(all_items, 'name'), 'value')
        all_items_year  <- rvest::html_attr(rvest::html_elements(all_items, 'yearpublished'), 'value')
    }

    # query result
    query_result <-
        dplyr::tibble(
        id = all_items_ids,
        type = all_items_types,
        name = all_items_names,
        yearpublished = all_items_year
    )
    if (is.null(type)) {
        query_result$type <- dplyr::if_else(query_result$type == 'primary', 'boardgame', query_result$type)
    } else {
        query_result$type <- dplyr::if_else(query_result$type == 'primary', type, query_result$type)
    }

    # return
    query_result
}
