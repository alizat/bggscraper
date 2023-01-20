mechanics <- function() {
    # mechanics link
    link <- 'https://boardgamegeek.com/browse/boardgamemechanic'

    # obtain html page
    page <- rvest::read_html(link)

    # retrieve mechanics
    mechanics <- rvest::html_text(rvest::html_elements(page, '.forum_table tr > td > a'))

    # return
    mechanics
}
