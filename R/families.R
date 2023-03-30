#' Families of Board Games
#'
#' @description \code{families()} retrieves the board game families that are
#'   present at \href{http://boardgamegeek.com}{Board Game Geek}.
#'
#' @param wait number of seconds to wait between pages while scraping to avoid
#'   being blocked by BGG (default is 10 seconds)
#' @param verbose whether to print supplementary text (shows intermediate
#'   progress)
#'
#' @return
#' Data frame containing list of families from BGG website.
#'
#' @seealso \code{\link{categories}} \code{\link{mechanics}}
#'   \code{\link{designers}}
#'
#' @examples
#' families(verbose = TRUE)
families <- function(wait = 10, verbose = FALSE) {
    # families link
    link <- 'https://boardgamegeek.com/browse/boardgamefamily'

    # get last page index
    last_page <- last_page_index(link)
    if (verbose)
        message(paste0('Number of HTML pages to scrape: ', last_page))

    # loop on pages
    families <- dplyr::tibble()
    for (i in 1:last_page) {
        # log
        if (verbose)
            message(paste0('Currently scraping HTML page no. ', i))

        # retrieve page i
        page_i <- glue::glue('{link}/page/{i}')
        page_i <- rvest::read_html(page_i)

        # families of page i
        families_i <- rvest::html_elements(page_i, 'table > tr > td > a')

        # precaution: break in case of empty page
        if (length(families_i) == 0) {
            # log
            if (verbose)
                message(paste0('HTML page no. ', i, ' is empty! No data found.'))

            break
        }

        # parse
        families_i <-
            purrr::map_dfr(
                families_i,
                function(item) {
                    family_id   <- empty_to_na(rvest::html_attr(families_i, 'href'))
                    family_id   <- stringr::str_extract(family_id, '[:digit:]+')
                    family_name <- empty_to_na(rvest::html_text(families_i))
                    dplyr::tibble(family_id, family_name)
                }
            )
        families_i <- dplyr::distinct(families_i)

        # append
        families <- rbind(families, families_i)

        # duration to sleep so BGG website would not block us
        Sys.sleep(wait)
    }

    # return
    families
}

# TODO: add note that the above code fails to retrieve pages beyond page 20
#     `rvest::read_html(page_i)` returns empty page when i > 20
