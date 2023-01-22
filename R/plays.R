plays <- function(game_id, wait = 5) {
    # initialize
    games_ids_all <- c()

    # number of pages
    page <- rvest::read_html(glue::glue('https://boardgamegeek.com/xmlapi2/plays?id={game_id}'))
    num_pages <- rvest::html_attr(rvest::html_elements(page, 'plays'), 'total')
    num_pages <- as.numeric(num_pages)

    # plays
    plays <- list()
    for (i in 1:num_pages) {
        # retrieve page i
        page_i <- glue::glue('https://boardgamegeek.com/xmlapi2/plays?id={game_id}&page={i}')
        page_i <- rvest::read_html(page_i)

        # get plays of page i
        plays_i <-
            dplyr::tibble(
                id         = rvest::html_attr(rvest::html_elements(page_i, 'play'), 'id'),
                userid     = rvest::html_attr(rvest::html_elements(page_i, 'play'), 'userid'),
                date       = rvest::html_attr(rvest::html_elements(page_i, 'play'), 'date'),
                quantity   = rvest::html_attr(rvest::html_elements(page_i, 'play'), 'quantity'),
                length     = rvest::html_attr(rvest::html_elements(page_i, 'play'), 'length'),
                incomplete = rvest::html_attr(rvest::html_elements(page_i, 'play'), 'incomplete'),
                nowinstats = rvest::html_attr(rvest::html_elements(page_i, 'play'), 'nowinstats'),
                location   = rvest::html_attr(rvest::html_elements(page_i, 'play'), 'location')
            )

        # append
        plays[[i]] <- plays_i

        # duration to sleep so BGG website would not block us
        Sys.sleep(wait)
    }

    # return
    dplyr::bind_rows(plays)
}
