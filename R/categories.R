#' Categories of Board Games
#'
#' @description \code{categories()} retrieves the board game categories that are
#'   present at \href{http://boardgamegeek.com}{Board Game Geek}.
#'
#' @return
#' Data frame containing all mechanics of board games (as per BGG).
#'
#' @seealso \code{\link{mechanics}} \code{\link{designers}}
#'   \code{\link{families}}
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
