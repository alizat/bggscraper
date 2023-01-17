families <- function(wait = 5) {
    # initialize
    families <- c()

    # get last page index
    paginator <- rvest::read_html('https://boardgamegeek.com/browse/boardgamefamily')
    paginator <- rvest::html_elements(paginator, '.fr a')
    last_page_element <- paginator[rvest::html_attr(paginator, 'title') == 'last page']
    last_page <- rvest::html_text(last_page_element)
    last_page <- stringr::str_extract(last_page, '[:digit:]+')
    last_page <- as.numeric(last_page)

    # loop on pages
    for (i in 1:last_page) {
        print(i)

        # retrieve page i
        page_i <- glue::glue('https://boardgamegeek.com/browse/boardgamefamily/page/{i}')
        page_i <- rvest::read_html(page_i)

        # grab families of page i
        families_i <- rvest::html_text(rvest::html_elements(page_i, 'table > tr > td > a'))
        if (length(families_i) == 0)
            break

        # append
        families <- c(families, families_i)

        # duration to sleep so BGG website would not block us
        Sys.sleep(wait)
    }

    #return families
    families
}

# TODO: add note that the above code fails to retrieve pages beyond page 20
#     `rvest::read_html(page_i)` returns empty page when i > 20
