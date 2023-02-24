hot <- function(type = 'boardgame') {
    # link
    link <- glue::glue('https://boardgamegeek.com/xmlapi2/hot?type={type}')

    # html page
    page <- rvest::read_html(link)

    # hot items
    items <- rvest::html_elements(page, 'item')
    items <-
        purrr::map_dfr(
            items,
            function(item) {
                item_id        <- empty_to_na(rvest::html_attr(item, 'id'))
                item_name      <- empty_to_na(rvest::html_attr(rvest::html_elements(item, 'name'), 'value'))
                year_published <- empty_to_na(rvest::html_attr(rvest::html_elements(item, 'yearpublished'), 'value'))
                dplyr::tibble(item_id, item_name, year_published)
            }
        )

    # return
    items
}
