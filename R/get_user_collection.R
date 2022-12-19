get_user_collection <- function(user) {
    # user collection link
    link <- paste0('https://boardgamegeek.com/collection/user/', user)

    # obtain html page
    html_page <- rvest::read_html(link)

    # grab user collection table
    collectionitems <- rvest::html_table(rvest::html_elements(html_page, xpath = '//*[@id="collectionitems"]'))
    collectionitems <- collectionitems[[1]]
    collectionitems <- dplyr::mutate_if(collectionitems, is.character, stringr::str_squish)
    collectionitems <- dplyr::select(collectionitems, Title, UserRating, Status)

    # user rating column (separate rating from date that rating was made)
    collectionitems$UserRatingDate <- purrr::map_chr(collectionitems$UserRating, ~ paste(unlist(stringr::str_split(.x, ' '))[-1], collapse = ' ') )
    collectionitems$UserRating     <- purrr::map_chr(collectionitems$UserRating, ~       unlist(stringr::str_split(.x, ' '))[[1]] )

    # missing rating to be NA instead of ''
    collectionitems$UserRating[collectionitems$UserRating == 'N/A']      <- NA
    collectionitems$UserRatingDate[collectionitems$UserRatingDate == ''] <- NA

    # status
    statuses <- c('Owned', 'Want To Buy', 'Want To Play')
    for (stts in statuses) {
        collectionitems[[stts]] <- stringr::str_detect(collectionitems$Status, stts)
    }
    collectionitems <- dplyr::select(collectionitems, -Status)

    # return
    collectionitems
}
