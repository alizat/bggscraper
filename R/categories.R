#' Categories of board games
#'
#' @return a data frame containing all categories of board games.
#'
#' @seealso [mechanics()]
#'
#' @examples
#' bg_categories <- categories()
#' bg_categories
categories <- function() {
    # categories link
    link <- 'https://boardgamegeek.com/browse/boardgamecategory'

    # obtain html page
    page <- rvest::read_html(link)

    # retrieve categories
    categories     <- rvest::html_text(rvest::html_elements(page, '.forum_table tr > td > a'))
    categories_ids <- rvest::html_attr(rvest::html_elements(page, '.forum_table tr > td > a'), 'href')
    categories_ids <- stringr::str_extract(categories_ids, '[:digit:]+')

    # return
    dplyr::tibble(category_id = categories_ids, category_name = categories)
}
