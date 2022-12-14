year_games_ids <- function(y) {
    # initialize
    games_ids_all <- c()

    # loop on pages
    for (i in 1:50) {
        # retrieve page i
        page_i <- glue::glue('https://boardgamegeek.com/search/boardgame/page/{i}?advsearch=1&q=&sort=rank&sortdir=asc&range[yearpublished][min]={y}&range[yearpublished][max]={y}&range[numvoters][min]=1')
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

        # 5-second sleep so BGG website would not block us
        Sys.sleep(5)
    }

    #return game ids
    games_ids_all
}
