families <- function(wait = 5, verbose = FALSE) {
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

        # grab families of page i
        families_i     <- rvest::html_text(rvest::html_elements(page_i, 'table > tr > td > a'))
        families_i_ids <- rvest::html_attr(rvest::html_elements(page_i, 'table > tr > td > a'), 'href')
        families_i_ids <- stringr::str_extract(families_i_ids, '[:digit:]+')

        # precaution: break in case of empty page
        if (length(families_i) == 0) {
            # log
            if (verbose)
                message(paste0('HTML page no. ', i, ' is empty! No data found.'))

            break
        }

        # append
        families <- rbind(families, dplyr::tibble(family_id = families_i_ids, family_name = families_i))

        # duration to sleep so BGG website would not block us
        Sys.sleep(wait)
    }

    # return
    families
}

# TODO: add note that the above code fails to retrieve pages beyond page 20
#     `rvest::read_html(page_i)` returns empty page when i > 20
