#' User's Collection
#'
#' @param username name of the user to retrieve their collection
#'
#' @return Collection of games for the specified user, include those that they
#' own or want to buy/play.
#'
#' @examples
#' my_collection <- collection('alizat')
#' my_collection
collection <- function(username) {
    # user collection link
    link <- paste0('https://boardgamegeek.com/xmlapi2/collection?username=', username, '&stats=1')

    # obtain html page
    html_page <- rvest::read_html(link)

    # elements to parse features
    items <- rvest::html_elements(html_page, 'item')

    # features to extract
    features <-
        list(
            item_type = '::objecttype',
            item_id = '::objectid',
            sub_type = '::subtype',
            item_name = 'name',
            year_published = 'yearpublished',
            min_players = 'stats::minplayers',
            max_players = 'stats::maxplayers',
            min_play_time = 'stats::minplaytime',
            max_play_time = 'stats::maxplaytime',
            playing_time = 'stats::playingtime',
            num_owned = 'stats::numowned',
            usr_rating = 'rating::value',
            rating_users_rated = 'rating > usersrated::value',
            rating_avg = 'rating > average::value',
            rating_bayes_avg = 'rating > bayesaverage::value',
            rating_stddev = 'rating > stddev::value',
            rating_median = 'rating > median::value',
            owned = 'status::own',
            prev_owned = 'status::prevowned',
            for_trade = 'status::fortrade',
            want = 'status::want',
            want_to_play = 'status::wanttoplay',
            want_to_buy = 'status::wanttobuy',
            wishlist = 'status::wishlist',
            preordered = 'status::preordered',
            last_modified = 'status::lastmodified',
            num_plays = 'numplays',
            comment = 'comment'
        )

    # convert to data frame
    collectionitems <- features_extractor(items, features)

    # return
    collectionitems
}
