features_extractor <- function(elements, features) {
    properties <- dplyr::tibble()
    for (i in 1:length(elements)) {
        properties_i <- list()
        for (j in 1:length(features)) {
            # is it an attribute?
            is_attribute <- stringr::str_detect(features[[j]], '::')

            if (is_attribute) {
                components <- unlist(stringr::str_split(features[[j]], '::'))
                if (components[[1]] == '') {
                    value <- rvest::html_attr(elements[[i]], components[[2]])
                } else {
                    value <- rvest::html_attr(rvest::html_elements(elements[[i]], components[[1]]), components[[2]])
                }
            } else {
                value <- rvest::html_text(rvest::html_elements(elements[[i]], features[[j]]))
            }

            # add feature value
            properties_i[[features[[j]]]] <- empty_to_na(value)
        }

        # adjust feature names
        names(properties_i) <- names(features)
        properties <- rbind(properties, dplyr::mutate_all(data.frame(properties_i), as.character))
    }

    # return
    properties
}
