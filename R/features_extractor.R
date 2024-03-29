#' Extract Features from HTML Elements
#'
#' @description \code{features_extractor()} obtains, for a list of HTML elements,
#'   a set of features that are given as input.
#'
#' @param elements a list of HTML elements
#' @param features a list of features to obtain for each of the HTML elements
#'   that are given as input
#'
#' @return
#' Data frame containing the retrieved features for the HTML elements that are
#' given as input.
#'
#' @examples
#' my_html <- rvest::read_html('https://boardgamegeek.com/xmlapi2/collection?username=alizat')
#' my_elements <- rvest::html_elements(my_html, 'item')
#' my_features <- c('name', 'status::own', 'status::wanttoplay', 'status::wanttobuy')
#' features_extractor(my_elements, my_features)
features_extractor <- function(elements, features) {
    properties <- vector(mode = 'list', length = length(elements))
    return_as_data_frame <- TRUE
    for (i in 1:length(elements)) {
        properties_i <- list()
        for (j in 1:length(features)) {
            # is it an attribute?
            is_attribute <- stringr::str_detect(features[[j]], '::')

            # is it the body?
            is_body <- features[[j]] %in% c('', 'body')

            if (is_attribute) {
                components <- unlist(stringr::str_split(features[[j]], '::'))
                if (components[[1]] == '') {
                    value <- rvest::html_attr(elements[[i]], components[[2]])
                } else {
                    value <- rvest::html_attr(rvest::html_elements(elements[[i]], components[[1]]), components[[2]])
                }
            } else if (is_body) {
                value <- rvest::html_text(elements[[i]])
            } else {
                value <- rvest::html_text(rvest::html_elements(elements[[i]], features[[j]]))
            }

            # add feature value
            properties_i[[features[[j]]]] <- empty_to_na(value)
        }

        # adjust feature names
        if (is.null(names(features))) {
            names(properties_i) <- stringr::str_remove(features, '^::')
        } else {
            names(properties_i) <- names(features)
        }

        # append
        properties[[i]] <- properties_i

        # can still return as data frame?
        if (any(purrr::map_dbl(properties_i, length) > 1)) {
            return_as_data_frame <- FALSE
        }
    }

    # return as
    if (return_as_data_frame) {
        properties <- purrr::map(properties, ~ data.frame(.x))
        properties <- purrr::map(properties, ~ dplyr::mutate_all(.x, as.character))
        properties <- dplyr::bind_rows(properties)
        properties <- dplyr::as_tibble(properties)
    }

    # return
    properties
}
