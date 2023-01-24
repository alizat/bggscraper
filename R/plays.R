plays <- function(game_id = NULL,
                  user = NULL,
                  wait = 5) {
    # check that game id and/or user was supplied
    assertthat::assert_that(!is.null(game_id) | !is.null(user))

    # xml link
    game_id <- if (is.null(game_id)) '' else game_id
    user    <- if (is.null(user))    '' else user
    link_base <- glue::glue('https://boardgamegeek.com/xmlapi2/plays?id={game_id}&username={user}')

    # number of pages
    page <- rvest::read_html(link_base)
    num_plays <- rvest::html_attr(rvest::html_elements(page, 'plays'), 'total')
    num_plays <- as.numeric(num_plays)
    num_pages <- ceiling(num_plays / 100)

    # plays
    plays <- list()
    for (i in 1:num_pages) {
        # retrieve page i
        page_i <- glue::glue('{link_base}&page={i}')
        page_i <- rvest::read_html(page_i)

        # get plays of page i
        plays_i <- rvest::html_elements(page_i, 'play')
        items_i <- rvest::html_elements(page_i, 'play item')

        # precaution: break in case of empty page
        if (length(plays_i) == 0)
            break

        # parse
        plays_i <-
            dplyr::tibble(
                id         = rvest::html_attr(plays_i, 'id'),
                user       = if (user == '')    rvest::html_attr(plays_i, 'userid')   else user,
                game_id    = rvest::html_attr(items_i, 'objectid'),
                game_name  = rvest::html_attr(items_i, 'name'),
                date       = rvest::html_attr(plays_i, 'date'),
                quantity   = rvest::html_attr(plays_i, 'quantity'),
                length     = rvest::html_attr(plays_i, 'length'),
                incomplete = rvest::html_attr(plays_i, 'incomplete'),
                nowinstats = rvest::html_attr(plays_i, 'nowinstats'),
                location   = rvest::html_attr(plays_i, 'location')
            )

        # append
        plays[[i]] <- plays_i

        # duration to sleep so BGG website would not block us
        Sys.sleep(wait)
    }

    # return
    dplyr::bind_rows(plays)
}
