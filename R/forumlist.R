#' Forum Lists for a Specified Item
#'
#' @description \code{forumlist()} retrieves the lists of forums available for
#'   the specified item.
#'
#' @param id of the items that you wish retrieve the forum lists for.
#' @param type type of the specified item
#'
#' @return
#' Data frame containing list of available forums for the specified item
#'
#' @examples
#' forumlist_pandemic_on_the_brink <- forumlist(40849)
#' forumlist_pandemic_on_the_brink
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
