top_k_games_ids <- function(k = 5000) {
    # initialize
    games_ids_all <- c()

    # upper bound for k is 5000
    if (k > 5000) {
        message('Upper limit for K is 5000')
        message('Setting K = 5000 ...')
        k <- 5000
    }

    # loop on pages
    for (i in 1:50) {
        # retrieve page i
        page_i <- glue::glue('https://boardgamegeek.com/search/boardgame/page/{i}?advsearch=1&q=&sort=rank&sortdir=asc')
        page_i <- rvest::read_html(page_i)
        page_i <- rvest::html_elements(page_i, xpath = '//*[@id="collectionitems"]')

        # grab game ids of page i
        games_ids <- rvest::html_attr(rvest::html_elements(page_i, 'tr > td > div > a'), 'href')
        games_ids <- stringr::str_extract(games_ids, '/[:digit:]+/')
        games_ids <- stringr::str_replace_all(games_ids, '/', '')

        # append
        games_ids_all <- c(games_ids_all, games_ids)

        # 10-second sleep so BGG website would not block us
        Sys.sleep(10)
    }

    #return game ids
    games_ids_all
}
