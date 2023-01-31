designers <- function(wait = 5) {
    # initialize
    designers <- c()

    # xml link
    link_base <- 'https://boardgamegeek.com/browse/boardgamedesigner'

    # get last page index
    last_page <- last_page_index(link_base)

    # loop on pages
    for (i in 1:last_page) {
        # retrieve page i
        page_i <- glue::glue('{link_base}/page/{i}')
        page_i <- rvest::read_html(page_i)

        # grab designers of page i
        designers_i <- rvest::html_text(rvest::html_elements(page_i, 'table > tr > td > a'))
        if (length(designers_i) == 0)
            break

        # append
        designers <- c(designers, designers_i)

        # duration to sleep so BGG website would not block us
        Sys.sleep(wait)
    }

    # return
    designers
}

# TODO: add note that the above code fails to retrieve pages beyond page 20
#     `rvest::read_html(page_i)` returns empty page when i > 20
