#' Thread Details
#'
#' @description \code{thread()} retrieves the details for the specified thread.
#'
#' @param id id of the desired forum.
#'
#' @return
#' Data frame containing the details of the specified thread.
#'
#' @seealso \code{\link{forumlist}} \code{\link{forum}}
#'
#' @examples
#' pandemic_on_the_brink_review <- thread(650169)
#' pandemic_on_the_brink_review
thread <- function(thread_id) {
    # forum threads
    link <- paste0('https://www.boardgamegeek.com/xmlapi2/thread?id=', thread_id)
    thread_details <- rvest::read_html(link)
    thread_subject <- rvest::html_text(rvest::html_elements(thread_details, 'thread > subject'))
    articles <- rvest::html_elements(thread_details, 'article')

    # parse
    features_to_extract <-
        list(
            article_id = '::id',
            username   = '::username',
            link       = '::link',
            postdate   = '::postdate',
            editdate   = '::editdate',
            numedits   = '::numedits',
            subject    = 'subject',
            body       = 'body'
        )
    articles_info <- features_extractor(articles, features_to_extract)
    articles_info$thread_id <- thread_id
    articles_info$thread_subject <- thread_subject
    articles_info <- dplyr::select(articles_info, thread_id, thread_subject, dplyr::everything())
    articles_info <- dplyr::mutate(articles_info, body = stringr::str_remove(body, paste0('^', subject)))

    # return
    articles_info
}
