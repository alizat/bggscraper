hot_games_ids <- function(type = 'boardgame') {
    # initialize
    games_ids_all <- c()

    # link
    link <- 'https://boardgamegeek.com/xmlapi2/hot?type={type}'

    # html page
    page <- rvest::read_html(link)

    # hot items
    games_ids_all <- rvest::html_elements(page, 'item')
    games_ids_all <- rvest::html_attr(games_ids_all, 'id')

    # return
    games_ids_all
}
