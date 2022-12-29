top_5k_games <- function() {
    # initialize
    game_ids_all <- c()

    # loop on pages
    for (i in 1:50) {
        # retrieve page i
        page_i <- glue::glue('https://boardgamegeek.com/search/boardgame/page/{i}?advsearch=1&q=&sort=rank&sortdir=asc')
        page_i <- rvest::read_html(page_i)
        page_i <- rvest::html_elements(page_i, xpath = '//*[@id="collectionitems"]')

        # grab game ids of page i
        game_ids <- rvest::html_attr(rvest::html_elements(page_i, 'tr > td > div > a'), 'href')
        game_ids <- stringr::str_extract(game_ids, '/[:digit:]+/')
        game_ids <- stringr::str_replace_all(game_ids, '/', '')

        # append
        game_ids_all <- c(game_ids_all, game_ids)

        # 10-second sleep so BGG website would not block us
        Sys.sleep(10)
    }

    #return game ids
    game_ids_all
}
