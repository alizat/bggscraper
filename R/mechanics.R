#' Mechanics of Board Games
#'
#' @description \code{mechanics()} retrieves the board game mechanics that are
#'   present at \href{http://boardgamegeek.com}{Board Game Geek}.
#'
#' @return
#' Data frame containing all mechanics of board games (as per BGG)
#'
#' @seealso \code{\link{categories}} \code{\link{designers}}
#'   \code{\link{families}}
#'
#' @examples
#' bg_mechanics <- mechanics()
#' bg_mechanics
mechanics <- function() {
    # mechanics link
    link <- 'https://boardgamegeek.com/browse/boardgamemechanic'

    # obtain html page
    page <- rvest::read_html(link)

    # retrieve mechanics
    mechanics     <- rvest::html_text(rvest::html_elements(page, '.forum_table tr > td > a'))
    mechanics_ids <- rvest::html_attr(rvest::html_elements(page, '.forum_table tr > td > a'), 'href')
    mechanics_ids <- stringr::str_extract(mechanics_ids, '[:digit:]+')

    # return
    dplyr::tibble(mechanic_id = mechanics_ids, mechanic_name = mechanics)
}
