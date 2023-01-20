categories <- function() {
    # categories link
    link <- 'https://boardgamegeek.com/browse/boardgamecategory'

    # obtain html page
    page <- rvest::read_html(link)

    # retrieve categories
    categories <- rvest::html_text(rvest::html_elements(page, '.forum_table tr > td > a'))

    # return
    categories
}
