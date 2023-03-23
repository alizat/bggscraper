#' Plays of a Specific Item and/or User
#'
#' @description \code{plays()} retrieves info on either plays of a specific
#'   item, plays by a specific user or plays of a specific item by a specific
#'   user.
#'
#' @param item_id id of item to retrieve plays information on. At least one of
#'   \code{item_id} and \code{username} needs to be supplied.
#' @param username username of user to retrieve plays for. At least one of
#'   \code{item_id} and \code{username} needs to be supplied.
#' @param type type of the item you want to request play information for. Valid
#'   values are \code{"thing"} and \code{"family"}.
#' @param mindate if supplied, returns only plays of specified date or later.
#' @param maxdate if supplied, returns only plays of specified date or previous.
#' @param subtype filters plays by supplied subtype. Valid values are
#'   \code{"boardgame"} (default), \code{"boardgameexpansion"},
#'   \code{"boardgameaccessory"}, \code{"boardgameintegration"},
#'   \code{"boardgamecompilation"}, \code{"boardgameimplementation"},
#'   \code{"rpg"}, \code{"rpgitem"} and \code{"videogame"}
#' @param wait number of seconds to wait between pages while scraping to avoid
#'   being blocked by BGG (default is 10 seconds)
#'
#' @return
#' Plays info of supplied item and/or username
#'
#' @examples
#' plays_3_wishes <- plays(198836)
#' plays_3_wishes
plays <- function(item_id = NULL,
                  username = NULL,
                  type = NULL,
                  mindate = NULL,
                  maxdate = NULL,
                  subtype = NULL,
                  wait = 10) {
    # check that item id and/or user name was supplied
    assertthat::assert_that(!is.null(item_id) | !is.null(username))

    # xml link
    item_id  <- if (is.null(item_id))  '' else item_id
    username <- if (is.null(username)) '' else username
    link_base <- glue::glue('https://boardgamegeek.com/xmlapi2/plays?id={item_id}&username={username}')
    if (!is.null(type))
        link_base <- paste0(link_base, '&type=', type)
    if (!is.null(mindate))
        link_base <- paste0(link_base, '&mindate=', lubridate::as_date(mindate))
    if (!is.null(maxdate))
        link_base <- paste0(link_base, '&maxdate=', lubridate::as_date(maxdate))
    if (!is.null(subtype))
        link_base <- paste0(link_base, '&subtype=', subtype)

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
        page_i <- rvest::html_elements(rvest::read_html(page_i), 'plays')

        # get plays of page i
        plays_i <- rvest::html_elements(page_i, 'play')
        items_i <- rvest::html_elements(page_i, 'play item')

        # precaution: break in case of empty page
        if (length(plays_i) == 0)
            break

        # parse
        plays_i <-
            dplyr::tibble(
                play_id    = rvest::html_attr(plays_i, 'id'),
                user_id    = if (username == '') rvest::html_attr(plays_i, 'userid') else rvest::html_attr(page_i, 'userid'),
                item_id    = rvest::html_attr(items_i, 'objectid'),
                item_name  = rvest::html_attr(items_i, 'name'),
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
