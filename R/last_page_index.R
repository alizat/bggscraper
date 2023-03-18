#' Last Page Index
#'
#' @description \code{last_page_index()} gets the index of the last page on a
#'   given BGG link
#'
#' @param link BGG link for which to get the index of the last page
#'
#' @return
#' Last page index for the given BGG link
#'
#' @examples
#' link <- 'https://boardgamegeek.com/browse/boardgamedesigner'
#' indx <- last_page_index(link)
#' indx
last_page_index <- function(link) {
    # paginator
    paginator <- rvest::read_html(link)
    paginator <- rvest::html_elements(paginator, '.fr a')

    # html element corresponding to last page
    last_page_element <- paginator[rvest::html_attr(paginator, 'title') == 'last page']

    # index of last page
    last_page <- rvest::html_text(last_page_element)
    last_page <- stringr::str_extract(last_page, '[:digit:]+')
    last_page <- as.numeric(last_page)

    # return
    last_page
}
