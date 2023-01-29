user_collection <- function(user) {
    # user collection link
    link <- paste0('https://boardgamegeek.com/xmlapi2/collection?username=', user, '&stats=1')

    # obtain html page
    html_page <- rvest::read_html(link)

    # features to extract
    features <-
        list(
            'item::objecttype' = '',
            'item::objectid' = '',
            'item::subtype' = '',
            'name' = '',
            'yearpublished' = '',
            'stats::minplayers' = '',
            'stats::maxplayers' = '',
            'stats::minplaytime' = '',
            'stats::maxplaytime' = '',
            'stats::playingtime' = '',
            'stats::numowned' = '',
            'rating::value' = '',
            'rating > usersrated::value' = '',
            'rating > average::value' = '',
            'rating > bayesaverage::value' = '',
            'rating > stddev::value' = '',
            'rating > median::value' = '',
            'status::own' = '',
            'status::prevowned' = '',
            'status::fortrade' = '',
            'status::want' = '',
            'status::wanttoplay' = '',
            'status::wanttobuy' = '',
            'status::wishlist' = '',
            'status::preordered' = '',
            'status::lastmodified' = '',
            'numplays' = '',
            'comment' = ''
        )

    # extract features
    for (feat in names(features)) {
        if (stringr::str_detect(feat, '::')) {
            components <- unlist(stringr::str_split(feat, '::'))
            tag <- components[[1]]
            attribute <- components[[2]]
            features[[feat]] <- rvest::html_elements(html_page, tag)
            features[[feat]] <- as.character(rvest::html_attr(features[[feat]], attribute))
        } else if (feat == 'comment') {
            features[[feat]] <-
                purrr::map_chr(
                    rvest::html_elements(html_page, 'item'),
                    function(x) {
                        y <- rvest::html_elements(x, 'comment')
                        y <- rvest::html_text(y)
                        if(is_empty(y))
                            y <- ''
                        y
                    }
                )
        } else {
            features[[feat]] <- rvest::html_elements(html_page, feat)
            features[[feat]] <- rvest::html_text(features[[feat]])
        }
    }

    # adjust features names
    names(features)[names(features) == 'item::objecttype']             <- 'object_type'
    names(features)[names(features) == 'item::objectid']               <- 'object_id'
    names(features)[names(features) == 'item::subtype']                <- 'sub_type'
    names(features)[names(features) == 'yearpublished']                <- 'year_published'
    names(features)[names(features) == 'stats::minplayers']            <- 'min_players'
    names(features)[names(features) == 'stats::maxplayers']            <- 'max_players'
    names(features)[names(features) == 'stats::minplaytime']           <- 'min_play_time'
    names(features)[names(features) == 'stats::maxplaytime']           <- 'max_play_time'
    names(features)[names(features) == 'stats::playingtime']           <- 'playing_time'
    names(features)[names(features) == 'stats::numowned']              <- 'num_owned'
    names(features)[names(features) == 'rating::value']                <- 'usr_rating'
    names(features)[names(features) == 'rating > usersrated::value']   <- 'rating_users_rated'
    names(features)[names(features) == 'rating > average::value']      <- 'rating_avg'
    names(features)[names(features) == 'rating > bayesaverage::value'] <- 'rating_bayes_avg'
    names(features)[names(features) == 'rating > stddev::value']       <- 'rating_stddev'
    names(features)[names(features) == 'rating > median::value']       <- 'rating_median'
    names(features)[names(features) == 'status::own']                  <- 'owned'
    names(features)[names(features) == 'status::prevowned']            <- 'prev_owned'
    names(features)[names(features) == 'status::fortrade']             <- 'for_trade'
    names(features)[names(features) == 'status::want']                 <- 'want'
    names(features)[names(features) == 'status::wanttoplay']           <- 'want_to_play'
    names(features)[names(features) == 'status::wanttobuy']            <- 'want_to_buy'
    names(features)[names(features) == 'status::wishlist']             <- 'wishlist'
    names(features)[names(features) == 'status::preordered']           <- 'preordered'
    names(features)[names(features) == 'status::lastmodified']         <- 'last_modified'
    names(features)[names(features) == 'numplays']                     <- 'num_plays'

    # convert to data frame
    collectionitems <- dplyr::bind_cols(features)
    collectionitems <- dplyr::mutate(collectionitems, user = user)
    collectionitems <- dplyr::select(collectionitems, user, dplyr::everything())

    # return
    collectionitems
}
