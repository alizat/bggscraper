#' List of Threads in a Forum
#'
#' @description \code{forum()} retrieves the list of threads for the specified
#'   forum.
#'
#' @param id id of the desired forum.
#'
#' @return
#' Data frame containing list of threads for the specified forum.
#'
#' @seealso \code{\link{forumlist}} \code{\link{thread}}
#'
#' @examples
#' forum(2418)
forum <- function(forum_id) {
    # forum threads
    link <- paste0('https://www.boardgamegeek.com/xmlapi2/forum?id=', forum_id)
    threads <- rvest::html_elements(rvest::read_html(link), 'thread')

    # parse
    features_to_extract <-
        list(
            thread_id    = '::id',
            subject      = '::subject',
            author       = '::author',
            numarticles  = '::numarticles',
            postdate     = '::postdate',
            lastpostdate = '::lastpostdate'
        )
    threads_info <- features_extractor(threads, features_to_extract)
    threads_info$forum_id <- forum_id
    threads_info <- dplyr::select(threads_info, forum_id, dplyr::everything())

    # return
    threads_info
}
