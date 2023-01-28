forumlist <- function(id, type = 'thing') {
    # forums
    link <- paste0('https://www.boardgamegeek.com/xmlapi2/forumlist?id=', id, '&type=', type)
    forums <- rvest::html_elements(rvest::read_html(link), 'forum')

    # parse
    forums <-
        dplyr::tibble(
            type         = type,
            id           = rvest::html_attr(forums, 'id'),
            groupid      = rvest::html_attr(forums, 'groupid'),
            title        = rvest::html_attr(forums, 'title'),
            noposting    = rvest::html_attr(forums, 'noposting'),
            description  = rvest::html_attr(forums, 'description'),
            numthreads   = rvest::html_attr(forums, 'numthreads'),
            numposts     = rvest::html_attr(forums, 'numposts'),
            lastpostdate = rvest::html_attr(forums, 'lastpostdate')
        )

    # return
    forums
}
