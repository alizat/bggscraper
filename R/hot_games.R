hot_games <- function(type = 'boardgame') {
    # link
    link <- 'https://boardgamegeek.com/xmlapi2/hot?type={type}'

    # html page
    page <- rvest::read_html(link)

    # hot items
    items                <- rvest::html_elements(page, 'item')
    items_ids            <- rvest::html_attr(items, 'id')
    items_names          <- rvest::html_attr(rvest::html_elements(items, 'name'), 'value')
    items_year_published <- character(0)
    for (i in 1:length(items)) {
        val <- rvest::html_attr(rvest::html_elements(items[[i]], 'yearpublished'), 'value')
        if (length(val) == 0)
            val <- NA
        items_year_published <- c(items_year_published, val)
    }

    # return
    dplyr::tibble(item_id = items_ids, item_name = items_names, year_published = items_year_published)
}
