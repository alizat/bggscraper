#' Specific Year's Games IDs
#'
#' @description \code{year_games_ids()} retrieves the ids of the games that came
#'   out in the specified year.
#'
#' @param y year to get games ids for
#' @param wait number of seconds to wait between HTML pages as they are scraped
#'
#' @return
#' Games IDs for the select year, \code{y}.
#'
#' @seealso \code{\link{top_k_games_ids}}
#'
#' @examples
#' year_1995_game_ids <- year_games_ids(y = 1995)
#' year_1995_game_ids
year_games_ids <- function(y, wait = 10) {
    # initialize
    games_ids_all <- c()

    # xml link
    link_base <- 'https://boardgamegeek.com/search/boardgame'

    # params
    params <- glue::glue('advsearch=1&q=&sort=rank&sortdir=asc&range[yearpublished][min]={y}&range[yearpublished][max]={y}&range[numvoters][min]=1')

    # get last page index
    last_page <- last_page_index(glue::glue('{link_base}?{params}'))

    # loop on pages
    for (i in 1:last_page) {
        # retrieve page i
        page_i <- glue::glue('{link_base}/page/{i}?{params}')
        page_i <- rvest::read_html(page_i)
        page_i <- rvest::html_elements(page_i, xpath = '//*[@id="collectionitems"]')

        # grab game ids of page i
        games_ids <- rvest::html_attr(rvest::html_elements(page_i, 'tr > td > div > a'), 'href')
        if (length(games_ids) == 0)
            break
        games_ids <- stringr::str_extract(games_ids, '/[:digit:]+/')
        games_ids <- stringr::str_replace_all(games_ids, '/', '')

        # append
        games_ids_all <- c(games_ids_all, games_ids)

        # duration to sleep so BGG website would not block us
        Sys.sleep(wait)
    }

    # return
    games_ids_all
}
