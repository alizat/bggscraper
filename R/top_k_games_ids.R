#' Top K Games IDs
#'
#' @description \code{top_k_games_ids()} retrieves the ids of the top \code{k}
#'   games.
#'
#' @param k the number of top games that you want IDs for
#' @param wait number of seconds to wait between HTML pages as they are scraped
#'
#' @return
#' Games IDs for the top k games.
#'
#' @seealso \code{\link{year_games_ids}}
#'
#' @examples
#' top_500_game_ids <- top_k_games_ids(k = 500)
#' top_500_game_ids
top_k_games_ids <- function(k = 100, wait = 10) {
    # initialize
    games_ids_all <- c()

    # lower bound for k is 1
    if (k < 1) {
        message('Lower limit for K is 1')
        message('Setting K = 1 ...')
        k <- 1
    }

    # upper bound for k is 5000
    if (k > 5000) {
        message('Upper limit for K is 5000')
        message('Setting K = 5000 ...')
        k <- 5000
    }

    # loop on pages
    i <- 1
    while(length(games_ids_all) < k) {
        # retrieve page i
        page_i <- glue::glue('https://boardgamegeek.com/search/boardgame/page/{i}?advsearch=1&q=&sort=rank&sortdir=asc')
        page_i <- rvest::read_html(page_i)
        page_i <- rvest::html_elements(page_i, xpath = '//*[@id="collectionitems"]')

        # grab game ids of page i
        games_ids <- rvest::html_attr(rvest::html_elements(page_i, 'tr > td > div > a'), 'href')
        games_ids <- stringr::str_extract(games_ids, '/[:digit:]+/')
        games_ids <- stringr::str_replace_all(games_ids, '/', '')

        # precaution: break in case of empty page
        if (length(games_ids) == 0)
            break

        # append
        games_ids_all <- c(games_ids_all, games_ids)

        # increment i
        i <- i + 1

        # duration to sleep so BGG website would not block us
        Sys.sleep(wait)
    }

    # keep k game ids
    if (length(games_ids_all) > 0) {
        # precaution: length(games_ids_all) may be lower than k
        k <- min(k, length(games_ids_all))

        # keep number of games requested
        games_ids_all <- games_ids_all[1:k]
    } else {
        message("That's odd... No games retrieved?")
    }

    # return
    games_ids_all
}
