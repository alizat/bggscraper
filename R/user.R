#' User Info
#'
#' @description \code{user()} retrieves the details for a specified user from
#'   \href{http://boardgamegeek.com}{Board Game Geek}.
#'
#' @param username name of the user to retrieve their details
#' @param buddies result to include user's buddies?
#' @param guilds result to include user's guilds?
#' @param hot result to include user's hot items?
#' @param top result to include user's top items?
#'
#' @return
#' List containing the specified user's info.
#'
#' @seealso \code{\link{collection}}
#'
#' @examples
#' user_info <- user('alizat')
#' user_info
user <- function(username, buddies = 0, guilds = 0, hot = 0, top = 0) {
    # user collection link
    link <- paste0('https://boardgamegeek.com/xmlapi2/user?name=', username)
    link <- paste0(link, '&buddies=', buddies)
    link <- paste0(link, '&guilds=', guilds)
    link <- paste0(link, '&hot=', hot)
    link <- paste0(link, '&top=', top)

    # obtain html page
    html_page <- rvest::read_html(link)

    # elements to parse features
    buddies <- if (buddies) rvest::html_elements(html_page, 'buddies') else NULL
    guilds  <- if (guilds)  rvest::html_elements(html_page, 'guilds')  else NULL
    hott     <- if (hot)     rvest::html_elements(html_page, 'hot')     else NULL
    topp     <- if (top)     rvest::html_elements(html_page, 'top')     else NULL
    hots <- if (!is.null(hott)) rvest::html_elements(hott, 'item')
    tops <- if (!is.null(topp)) rvest::html_elements(topp, 'item')

    # extract user details
    features <-
        list(
            firstname = 'firstname::value',
            lastname = 'lastname::value',
            avatarlink = 'avatarlink::value',
            yearregistered = 'yearregistered::value',
            lastlogin = 'lastlogin::value',
            stateorprovince = 'stateorprovince::value',
            country = 'country::value',
            webaddress = 'webaddress::value',
            xboxaccount = 'xboxaccount::value',
            wiiaccount = 'wiiaccount::value',
            psnaccount = 'psnaccount::value',
            battlenetaccount = 'battlenetaccount::value',
            steamaccount = 'steamaccount::value',
            traderating = 'traderating::value'
        )
    user_details <- features_extractor(list(html_page), features)

    # extract hots/tops features
    features <-
        list(
            item_rank = '::rank',
            item_type = '::type',
            item_id   = '::id',
            item_name = '::name'
        )
    if (!is.null(hots) && length(hots) > 0)
        hots_info <- features_extractor(hots, features)
    if (!is.null(tops) && length(tops) > 0)
        tops_info <- features_extractor(tops, features)

    # combine into list
    user_info <- list()
    user_info[['user_details']] <- user_details
    if (hot != 0 && exists('hots_info'))
        user_info[['hots_info']] <- hots_info
    if (top != 0 && exists('tops_info'))
        user_info[['tops_info']] <- tops_info

    # return
    user_info
}
