#' Designers of Board Games
#'
#' @description \code{designers()} retrieves the board game designers that are
#'   present at \href{http://boardgamegeek.com}{Board Game Geek}.
#'
#' @param wait number of seconds to wait between pages while scraping to avoid
#'   being blocked by BGG (default is 10 seconds)
#' @param verbose whether to print supplementary text (shows intermediate
#'   progress)
#'
#' @return
#' Data frame containing list of designers from BGG website
#'
#' @seealso \code{\link{categories}} \code{\link{mechanics}}
#'   \code{\link{families}}
#'
#' @examples
#' designers(verbose = TRUE)
designers <- function(wait = 10, verbose = FALSE) {
    # designers link
    link <- 'https://boardgamegeek.com/browse/boardgamedesigner'

    # get last page index
    last_page <- last_page_index(link)
    if (verbose)
        message(paste0('Number of HTML pages to scrape: ', last_page))

    # loop on pages
    designers <- dplyr::tibble()
    for (i in 1:last_page) {
        # log
        if (verbose)
            message(paste0('Currently scraping HTML page no. ', i))

        # retrieve page i
        page_i <- glue::glue('{link}/page/{i}')
        page_i <- rvest::read_html(page_i)

        # designers of page i
        designers_i <- rvest::html_elements(page_i, 'table > tr > td > a')

        # precaution: break in case of empty page
        if (length(designers_i) == 0) {
            # log
            if (verbose)
                message(paste0('HTML page no. ', i, ' is empty! No data found.'))

            break
        }

        # parse
        designers_i <-
            purrr::map_dfr(
                designers_i,
                function(item) {
                    designer_id   <- empty_to_na(rvest::html_attr(designers_i, 'href'))
                    designer_id   <- stringr::str_extract(designer_id, '[:digit:]+')
                    designer_name <- empty_to_na(rvest::html_text(designers_i))
                    dplyr::tibble(designer_id, designer_name)
                }
            )
        designers_i <- dplyr::distinct(designers_i)

        # append
        designers <- rbind(designers, designers_i)

        # duration to sleep so BGG website would not block us
        Sys.sleep(wait)
    }

    # return
    designers
}

# TODO: add note that the above code fails to retrieve pages beyond page 20
#     `rvest::read_html(page_i)` returns empty page when i > 20
