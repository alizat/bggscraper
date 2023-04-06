#' List of Forums for a Specified Item
#'
#' @description \code{forumlist()} retrieves the lists of forums available for
#'   the specified item.
#'
#' @param forumlist_id id of the item that you wish retrieve forum lists for.
#' @param type type of the specified item. Valid values are \code{"thing"} and
#'   \code{"family"}.
#'
#' @return
#' Data frame containing list of available forums for the specified item.
#'
#' @seealso \code{\link{forum}} \code{\link{thread}}
#'
#' @examples
#' forumlist_pandemic_on_the_brink <- forumlist(40849)
#' forumlist_pandemic_on_the_brink
forumlist <- function(forumlist_id, type = 'thing') {
    # forums
    link <- paste0('https://www.boardgamegeek.com/xmlapi2/forumlist?id=', forumlist_id, '&type=', type)
    forums <- rvest::html_elements(rvest::read_html(link), 'forum')

    # parse
    features_to_extract <-
        list(
            forum_id     = '::id',
            groupid      = '::groupid',
            title        = '::title',
            noposting    = '::noposting',
            description  = '::description',
            numthreads   = '::numthreads',
            numposts     = '::numposts',
            lastpostdate = '::lastpostdate'
        )
    forums_info <- features_extractor(forums, features_to_extract)
    forums_info$forumlist_id <- forumlist_id
    forums_info$type <- type
    forums_info <- dplyr::select(forums_info, forumlist_id, type, dplyr::everything())

    # return
    forums_info
}
