hot_games_ids <- function() {
    # initialize
    games_ids_all <- c()

    # link
    link <- 'https://boardgamegeek.com/xmlapi2/hot?type=boardgame'

    # html page
    page <- rvest::read_html(link)

    # hot items
    games_ids_all <- rvest::html_elements(page, 'item')
    games_ids_all <- rvest::html_attr(games_ids_all, 'id')

    #return game ids
    games_ids_all
}
