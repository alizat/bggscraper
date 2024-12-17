# References
#   https://stackoverflow.com/questions/61053827/web-scraping-boardgamegeek-with-rvest
#   https://boardgamegeek.com/wiki/page/BGG_XML_API2



suppressMessages({
    library(tidyverse)
    library(lubridate)
    library(plotly)
    library(tictoc)
    library(glue)
    library(rvest)
    
    options(dplyr.summarise.inform = FALSE)
})



#' Convert Array to NA if Empty
#'
#' @description \code{empty_to_na()} converts the received array to single value
#'   of \code{NA} if it is an empty array. Otherwise, the received array is
#'   returned as is.
#'
#' @param x an array
#'
#' @return
#' \code{NA} if x was empty. Otherwise, x is returned as is.
#'
#' @examples
#' empty_to_na(character(0))
#' empty_to_na(c())
#' empty_to_na(c(1,2,3))
#' empty_to_na(c('a','b','c'))
empty_to_na <- function(x) {
    if (length(x) == 0) NA else x
}



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
#' my_html <- read_html('https://boardgamegeek.com/xmlapi2/collection?username=alizat')
#' my_elements <- html_elements(my_html, 'item')
#' my_features <- c(name = 'name', own = 'status::own', wanttoplay = 'status::wanttoplay', wanttobuy = 'status::wanttobuy')
#' features_extractor(my_elements, my_features)
features_extractor <- function(elements, features) {
    # if features is not a named list...
    if (is.null(names(features))) {
        # turn it into a named list
        names(features) = features %>% str_remove_all('\\s') %>% str_replace_all('\\W+', '_') %>% str_remove('_value$')
    }
    
    # extract features
    for (j in 1:length(features)) {
        # is it an attribute?
        is_attribute <- str_detect(features[[j]], '::')
        
        # is it the body?
        is_body <- features[[j]] %in% c('', 'body')
        
        if (is_attribute) {
            components <- unlist(str_split(features[[j]], '::'))
            if (components[[1]] == '') {
                value <- elements %>% map_chr(~ .x %>% html_attr(components[[2]]))
            } else {
                value <- elements %>% map(~ .x %>% html_elements(components[[1]]) %>% html_attr(components[[2]]))
            }
        } else if (is_body) {
            value <- elements %>% map(~ .x %>% html_text())
        } else {
            value <- elements %>% map(~ .x %>% html_elements(features[[j]]) %>% html_text())
        }
        
        # add feature value
        if (j == 1) {
            properties <- tibble(dummy = 1:length(value))
        }
        properties[[names(features)[[j]]]] <- value
    }
    
    # convert list columns to character columns wherever possible
    properties <-
        properties %>%
        select(-dummy) %>%
        
        # for each column that is a list...
        mutate_if(is.list, function(x) {
            # if any item in this list column has multiple elements...
            if (any(map_dbl(x, length) > 1)) {
                # not possible to convert to character column  -->  return as is
                return(x)
            }
            
            # otherwise, convert to character column as follows...
            # for each item, return the single element within (or '' if nothing inside)
            map_chr(x, ~ if (length(.x) == 0) { '' } else { .x[[1]] })
        })
    
    # return
    properties
}



#' Thing Info
#'
#' @description \code{thing()} retrieves the available information of the
#'   specified items (i.e. board games and board game expansions).
#'
#' @param ids IDs of the items that you wish retrieve the information for.
#'
#' @return
#' Data frame containing the details of the items for which the IDs were
#' supplied.
#'
#' @examples
#' games_details <- thing(c(158600, 194607, 40849))
#' games_details
thing <- function(ids) {
    # merge ids
    ids <- paste(ids, collapse = ',')
    
    # game details
    link <- paste0('https://www.boardgamegeek.com/xmlapi2/thing?id=', ids, '&stats=1')
    items <- html_elements(read_html(link), 'item')
    
    # features to extract
    features_to_extract <-
        c(
            name = 'name[type=primary]::value',
            year_published = 'yearpublished::value',
            description = 'description',
            min_players = 'minplayers::value',
            max_players = 'maxplayers::value',
            min_playtime = 'minplaytime::value',
            max_playtime = 'maxplaytime::value',
            playing_time = 'playingtime::value',
            min_age = 'minage::value',
            category = 'link[type=boardgamecategory]::value',
            mechanic = 'link[type=boardgamemechanic]::value',
            family = 'link[type=boardgamefamily]::value',
            designer = 'link[type=boardgamedesigner]::value',
            artist = 'link[type=boardgameartist]::value',
            publisher = 'link[type=boardgamepublisher]::value',
            rating_users_rated = 'ratings > usersrated::value',
            rating_avg = 'ratings > average::value',
            rating_bayes_avg = 'ratings > bayesaverage::value',
            rank = 'ratings > ranks > rank[name="boardgame"]::value',
            owned = 'ratings > owned::value',
            trading = 'ratings > trading::value',
            wanting = 'ratings > wanting::value',
            wishing = 'ratings > wishing::value',
            num_comments = 'ratings > numcomments::value',
            num_weights = 'ratings > numweights::value',
            avg_weight = 'ratings > averageweight::value'
        )
    
    # extract features
    items_details <- features_extractor(items, features_to_extract)
    items_details
}



#' User's Collection
#'
#' @description \code{collection()} retrieves the board game collection for a
#'   specified user from \href{http://boardgamegeek.com}{Board Game Geek}.
#'
#' @param username name of the user to retrieve their collection
#'
#' @return
#' Data frame containing the specified user's collection, including those that
#' they own or want to buy/play.
#'
#' @examples
#' my_collection <- collection('alizat')
#' my_collection
collection <- function(username) {
    # user collection link
    link <- paste0('https://boardgamegeek.com/xmlapi2/collection?username=', username, '&stats=1')
    
    # obtain html page
    html_page <- read_html(link)
    
    # elements to parse features
    items <- html_elements(html_page, 'item')
    
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



#' Categories of Board Games
#'
#' @description \code{categories()} retrieves the board game categories that are
#'   present at \href{http://boardgamegeek.com}{Board Game Geek}.
#'
#' @return
#' Data frame containing all mechanics of board games (as per BGG).
#'
#' @examples
#' bg_categories <- categories()
#' bg_categories
categories <- function() {
    # categories link
    link <- 'https://boardgamegeek.com/browse/boardgamecategory'
    
    # obtain html page
    page <- read_html(link)
    
    # retrieve categories
    categories     <- html_text(html_elements(page, '.forum_table tr > td > a'))
    categories_ids <- html_attr(html_elements(page, '.forum_table tr > td > a'), 'href')
    categories_ids <- str_extract(categories_ids, '[:digit:]+')
    
    # return
    tibble(category_id = categories_ids, category_name = categories)
}



#' Mechanics of Board Games
#'
#' @description \code{mechanics()} retrieves the board game mechanics that are
#'   present at \href{http://boardgamegeek.com}{Board Game Geek}.
#'
#' @return
#' Data frame containing all mechanics of board games (as per BGG).
#'
#' @examples
#' bg_mechanics <- mechanics()
#' bg_mechanics
mechanics <- function() {
    # mechanics link
    link <- 'https://boardgamegeek.com/browse/boardgamemechanic'
    
    # obtain html page
    page <- read_html(link)
    
    # retrieve mechanics
    mechanics     <- html_text(html_elements(page, '.forum_table tr > td > a'))
    mechanics_ids <- html_attr(html_elements(page, '.forum_table tr > td > a'), 'href')
    mechanics_ids <- str_extract(mechanics_ids, '[:digit:]+')
    
    # return
    tibble(mechanic_id = mechanics_ids, mechanic_name = mechanics)
}



#' Last Page Index
#'
#' @description \code{last_page_index()} gets the index of the last page on a
#'   given BGG link
#'
#' @param link BGG link for which to get the index of the last page
#'
#' @return
#' Last page index for the given BGG link.
#'
#' @examples
#' link <- 'https://boardgamegeek.com/browse/boardgamedesigner'
#' indx <- last_page_index(link)
#' indx
last_page_index <- function(link) {
    # paginator
    paginator <- read_html(link)
    paginator <- html_elements(paginator, '.fr a')
    
    # html element corresponding to last page
    last_page_element <- paginator[html_attr(paginator, 'title') == 'last page']
    
    # index of last page
    last_page <- html_text(last_page_element)
    last_page <- str_extract(last_page, '[:digit:]+')
    last_page <- as.numeric(last_page)
    
    # return
    last_page
}



#' Designers of Board Games
#'
#' @description \code{designers()} retrieves the board game designers that are
#'   present at \href{http://boardgamegeek.com}{Board Game Geek}.
#'
#' @param wait number of seconds to wait between pages while scraping to avoid
#'   being blocked by BGG (default is 10 seconds)
#' @param verbose whether to print supplementary text (shows intermediate
#'   progress)
#'
#' @return
#' Data frame containing list of designers from BGG website.
#'
#' @examples
#' designers(verbose = TRUE)
designers <- function(wait = 10, verbose = FALSE) {
    # designers link
    link <- 'https://boardgamegeek.com/browse/boardgamedesigner'
    
    # get last page index
    last_page <- last_page_index(link)
    if (verbose)
        message(paste0('Number of HTML pages to scrape: ', last_page))
    
    # loop on pages
    designers <- tibble()
    for (i in 1:last_page) {
        # log
        if (verbose)
            message(paste0('Currently scraping HTML page no. ', i))
        
        # retrieve page i
        page_i <- glue('{link}/page/{i}')
        page_i <- read_html(page_i)
        
        # designers of page i
        designers_i <- html_elements(page_i, 'table > tr > td > a')
        
        # precaution: break in case of empty page
        if (length(designers_i) == 0) {
            # log
            if (verbose)
                message(paste0('HTML page no. ', i, ' is empty! No data found.'))
            
            break
        }
        
        # parse
        designers_i <-
            map_dfr(
                designers_i,
                function(item) {
                    designer_id   <- empty_to_na(html_attr(designers_i, 'href'))
                    designer_id   <- str_extract(designer_id, '[:digit:]+')
                    designer_name <- empty_to_na(html_text(designers_i))
                    tibble(designer_id, designer_name)
                }
            )
        designers_i <- distinct(designers_i)
        
        # append
        designers <- rbind(designers, designers_i)
        
        # duration to sleep so BGG website would not block us
        if (i != last_page) {
            Sys.sleep(wait)
        }
    }
    
    # return
    designers
}

# TODO: add note that the above code fails to retrieve pages beyond page 20
#     `read_html(page_i)` returns empty page when i > 20



#' Families of Board Games
#'
#' @description \code{families()} retrieves the board game families that are
#'   present at \href{http://boardgamegeek.com}{Board Game Geek}.
#'
#' @param wait number of seconds to wait between pages while scraping to avoid
#'   being blocked by BGG (default is 10 seconds)
#' @param verbose whether to print supplementary text (shows intermediate
#'   progress)
#'
#' @return
#' Data frame containing list of families from BGG website.
#'
#' @examples
#' families(verbose = TRUE)
families <- function(wait = 10, verbose = FALSE) {
    # families link
    link <- 'https://boardgamegeek.com/browse/boardgamefamily'
    
    # get last page index
    last_page <- last_page_index(link)
    if (verbose)
        message(paste0('Number of HTML pages to scrape: ', last_page))
    
    # loop on pages
    families <- tibble()
    for (i in 1:last_page) {
        # log
        if (verbose)
            message(paste0('Currently scraping HTML page no. ', i))
        
        # retrieve page i
        page_i <- glue('{link}/page/{i}')
        page_i <- read_html(page_i)
        
        # families of page i
        families_i <- html_elements(page_i, 'table > tr > td > a')
        
        # precaution: break in case of empty page
        if (length(families_i) == 0) {
            # log
            if (verbose)
                message(paste0('HTML page no. ', i, ' is empty! No data found.'))
            
            break
        }
        
        # parse
        families_i <-
            map_dfr(
                families_i,
                function(item) {
                    family_id   <- empty_to_na(html_attr(families_i, 'href'))
                    family_id   <- str_extract(family_id, '[:digit:]+')
                    family_name <- empty_to_na(html_text(families_i))
                    tibble(family_id, family_name)
                }
            )
        families_i <- distinct(families_i)
        
        # append
        families <- rbind(families, families_i)
        
        # duration to sleep so BGG website would not block us
        if (i != last_page) {
            Sys.sleep(wait)
        }
    }
    
    # return
    families
}

# TODO: add note that the above code fails to retrieve pages beyond page 20
#     `read_html(page_i)` returns empty page when i > 20



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
#' @examples
#' forumlist_pandemic_on_the_brink <- forumlist(40849)
#' forumlist_pandemic_on_the_brink
forumlist <- function(forumlist_id, type = 'thing') {
    # forums
    link <- paste0('https://www.boardgamegeek.com/xmlapi2/forumlist?id=', forumlist_id, '&type=', type)
    forums <- html_elements(read_html(link), 'forum')
    
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
    forums_info <- select(forums_info, forumlist_id, type, everything())
    
    # return
    forums_info
}



#' List of Threads in a Forum
#'
#' @description \code{forum()} retrieves the list of threads for the specified
#'   forum.
#'
#' @param forum_id id of the desired forum.
#'
#' @return
#' Data frame containing list of threads for the specified forum.
#'
#' @examples
#' forum(2418)
forum <- function(forum_id) {
    # forum threads
    link <- paste0('https://www.boardgamegeek.com/xmlapi2/forum?id=', forum_id)
    threads <- html_elements(read_html(link), 'thread')
    
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
    threads_info <- select(threads_info, forum_id, everything())
    
    # return
    threads_info
}



#' Thread Details
#'
#' @description \code{thread()} retrieves the details for the specified thread.
#'
#' @param thread_id id of the desired forum.
#'
#' @return
#' Data frame containing the details of the specified thread.
#'
#' @examples
#' pandemic_on_the_brink_review <- thread(650169)
#' pandemic_on_the_brink_review
thread <- function(thread_id) {
    # forum threads
    link           <- paste0('https://www.boardgamegeek.com/xmlapi2/thread?id=', thread_id)
    thread_details <- read_html(link)
    thread_subject <- html_text(html_elements(thread_details, 'thread > subject'))
    articles       <- html_elements(thread_details, 'article')
    
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
    articles_info <- select(articles_info, thread_id, thread_subject, everything())
    articles_info$body <- str_remove(articles_info$body, paste0('^', articles_info$subject))
    
    # return
    articles_info
}



#' Guild Info
#'
#' @description \code{guild()} retrieves the info for the specified guild
#'   (details and members list).
#'
#' @param guild_id id of the guild you wish to retrieve
#' @param members include member roster in the results? (default: \code{TRUE})
#' @param sort_by how to sort the members list. Valid values are
#'   \code{"username"} (default) and \code{"date"}.
#'
#' @return
#' List containing two data frames: \code{"guild_details"} and
#' \code{"members_info"}.
#'
#' @examples
#' guild(1299)
guild <- function(guild_id, members = 1, sort_by = 'username') {
    # link
    link <- glue('https://boardgamegeek.com/xmlapi2/guild?id={guild_id}&members={members}&sort={sort_by}')
    
    # html page
    page <- read_html(link)
    
    # guild info
    guild_details <- html_elements(page, 'guild')
    features <-
        list(
            guild_id = '::id',
            guild_name = '::name',
            creation_date = '::created',
            category = 'category',
            website = 'website',
            manager = 'manager',
            description = 'description',
            addr1 = 'location::addr1',
            addr2 = 'location::addr2',
            city = 'location::city',
            stateorprovince = 'location::stateorprovince',
            postalcode = 'location::postalcode',
            country = 'location::country'
        )
    guild_details <- features_extractor(guild_details, features)
    
    # members info
    num_members  <- as.numeric(html_attr(html_elements(page, 'members'), 'count'))
    num_pages    <- ceiling(num_members / 25)
    members_info <- tibble()
    for (page_no in 1:num_pages) {
        # page link
        page_link <- paste0(link, '&page=', page_no)
        
        # page itself
        page <- read_html(page_link)
        
        # members
        members_list <- html_elements(page, 'member')
        members_list <- features_extractor(elements = members_list, features = list(member_name = '::name', member_join_date = '::date'))
        members_info <- rbind(members_info, members_list)
    }
    
    # return
    list(
        guild_details = guild_details,
        members_info  = members_info
    )
}



#' Hot items nowadays
#'
#' @description \code{hot()} retrieves the items that are "hot" nowadays.
#'
#' @param type type of hot items to return. Possible values are
#'   \code{"boardgame"} (default), \code{"rpg"}, \code{"videogame"},
#'   \code{"boardgameperson"}, \code{"rpgperson"}, \code{"boardgamecompany"},
#'   \code{"rpgcompany"} and \code{"videogamecompany"}.
#'
#' @return A data frame containing nowadays' hot items.
#'
#' @examples
#' hot_items_df <- hot()
#' hot_items_df
hot <- function(type = 'boardgame') {
    # link
    link <- glue('https://boardgamegeek.com/xmlapi2/hot?type={type}')
    
    # html page
    page <- read_html(link)
    
    # hot items
    items <- html_elements(page, 'item')
    items <-
        map_dfr(
            items,
            function(item) {
                item_id        <- empty_to_na(html_attr(item, 'id'))
                item_name      <- empty_to_na(html_attr(html_elements(item, 'name'), 'value'))
                year_published <- empty_to_na(html_attr(html_elements(item, 'yearpublished'), 'value'))
                tibble(item_id, item_name, year_published)
            }
        )
    
    # return
    items
}



#' Plays of a Specific Item and/or User
#'
#' @description \code{plays()} retrieves info on either plays of a specific
#'   item, plays by a specific user or plays of a specific item by a specific
#'   user.
#'
#' @param item_id id of item to retrieve plays information on. At least one of
#'   \code{item_id} and \code{username} needs to be supplied.
#' @param username username of user to retrieve plays for. At least one of
#'   \code{item_id} and \code{username} needs to be supplied.
#' @param type type of the item you want to request play information for. Valid
#'   values are \code{"thing"} and \code{"family"}.
#' @param mindate if supplied, returns only plays of specified date or later.
#' @param maxdate if supplied, returns only plays of specified date or previous.
#' @param subtype filters plays by supplied subtype. Valid values are
#'   \code{"boardgame"} (default), \code{"boardgameexpansion"},
#'   \code{"boardgameaccessory"}, \code{"boardgameintegration"},
#'   \code{"boardgamecompilation"}, \code{"boardgameimplementation"},
#'   \code{"rpg"}, \code{"rpgitem"} and \code{"videogame"}
#' @param wait number of seconds to wait between pages while scraping to avoid
#'   being blocked by BGG (default is 10 seconds)
#'
#' @return
#' Plays info of supplied item and/or username.
#'
#' @examples
#' plays_3_wishes <- plays(198836)
#' plays_3_wishes
plays <- function(item_id = NULL,
                  username = NULL,
                  type = NULL,
                  mindate = NULL,
                  maxdate = NULL,
                  subtype = NULL,
                  wait = 10) {
    # check that item id and/or user name was supplied
    assertthat::assert_that(!is.null(item_id) | !is.null(username))
    
    # xml link
    item_id   <- if (is.null(item_id))  '' else item_id
    username  <- if (is.null(username)) '' else username
    link_base <- glue('https://boardgamegeek.com/xmlapi2/plays?id={item_id}&username={username}')
    if (!is.null(type))
        link_base <- paste0(link_base, '&type=', type)
    if (!is.null(mindate))
        link_base <- paste0(link_base, '&mindate=', lubridate::as_date(mindate))
    if (!is.null(maxdate))
        link_base <- paste0(link_base, '&maxdate=', lubridate::as_date(maxdate))
    if (!is.null(subtype))
        link_base <- paste0(link_base, '&subtype=', subtype)
    
    # number of pages
    page      <- read_html(link_base)
    num_plays <- html_attr(html_elements(page, 'plays'), 'total')
    num_plays <- as.numeric(num_plays)
    num_pages <- ceiling(num_plays / 100)
    
    # plays
    plays <- list()
    for (i in 1:num_pages) {
        # retrieve page i
        page_i <- glue('{link_base}&page={i}')
        page_i <- html_elements(read_html(page_i), 'plays')
        
        # get plays of page i
        plays_i <- html_elements(page_i, 'play')
        items_i <- html_elements(page_i, 'play item')
        
        # precaution: break in case of empty page
        if (length(plays_i) == 0)
            break
        
        # parse
        plays_i <-
            tibble(
                play_id    = html_attr(plays_i, 'id'),
                user_id    = if (username == '') html_attr(plays_i, 'userid') else html_attr(page_i, 'userid'),
                item_id    = html_attr(items_i, 'objectid'),
                item_name  = html_attr(items_i, 'name'),
                date       = html_attr(plays_i, 'date'),
                quantity   = html_attr(plays_i, 'quantity'),
                length     = html_attr(plays_i, 'length'),
                incomplete = html_attr(plays_i, 'incomplete'),
                nowinstats = html_attr(plays_i, 'nowinstats'),
                location   = html_attr(plays_i, 'location')
            )
        
        # append
        plays[[i]] <- plays_i
        
        # duration to sleep so BGG website would not block us
        if (i != num_pages) {
            Sys.sleep(wait)
        }
    }
    
    # return
    bind_rows(plays)
}



#' BGG Search
#'
#' @description \code{searchbgg()} gets search results for supplied query.
#'
#' @param query keywords of item that you are looking for
#' @param type type of item. Possible values are \code{"rpgitem"},
#'   \code{"videogame"}, \code{"boardgame"}, \code{"boardgameaccessory"} and
#'   \code{"boardgameexpansion"}. Specifying multiple types separated by commas
#'   is allowed (see examples below).
#' @param exact whether the returned result should match the search query
#'   perfectly (default is \code{0})
#'
#' @return
#' Data frame containing search results.
#'
#' @examples
#' searchbgg(query = 'shipshape')
#' searchbgg(query = 'shipshape', type = 'boardgame')
#' searchbgg(query = 'dune imperium')
#' searchbgg(query = 'water', type = 'rpgitem')
searchbgg <- function(query,
                      type = NULL,
                      exact = 0) {
    # adjust query (if necessary)
    query <- str_squish(query)
    query <- str_replace_all(query, ' ', '+')
    
    # features of interest
    my_features <-
        list(
            item_id            = '::id',
            item_type          = 'name::type',
            item_name          = 'name::value',
            item_yearpublished = 'yearpublished::value'
        )
    
    # if type is NULL, it will be taken to mean BOTH board games and expansions
    if (is.null(type) || type == 'boardgame') {
        # both board games and expansions
        link         <- glue('https://boardgamegeek.com/xmlapi2/search?query={query}&type=boardgame')
        page         <- read_html(link)
        all_items    <- html_elements(page, 'item')
        query_result <- features_extractor(all_items, my_features)
        
        # just expansions
        link           <- glue('https://boardgamegeek.com/xmlapi2/search?query={query}&type=boardgameexpansion')
        page           <- read_html(link)
        expansions     <- html_elements(page, 'item')
        expansions_ids <- html_attr(expansions, 'id')
        
        # if 'type' was specified as 'boardgame'...
        if (!is.null(type) && type == 'boardgame') {
            # ... exclude expansions
            query_result <- filter(query_result, !('item_id' %in% expansions_ids))
        } else {
            # otherwise, keep all items and adjust type in 'all_items_types'
            query_result$item_type[query_result$item_id %in% expansions_ids] <- 'boardgameexpansion'
        }
        
    } else {
        # type was specified as something other than 'boardgame'
        link         <- glue('https://boardgamegeek.com/xmlapi2/search?query={query}&type={type}')
        page         <- read_html(link)
        all_items    <- html_elements(page, 'item')
        query_result <- features_extractor(all_items, my_features)
    }
    
    # adjust item type in query results
    if (is.null(type)) {
        type <- 'boardgame'
    }
    query_result$item_type[query_result$item_type == 'primary'] <- type
    
    # return
    query_result
}



#' Top K Games IDs
#'
#' @description \code{top_k_games_ids()} retrieves the ids of the top \code{k}
#'   games.
#'
#' @param k the number of top games that you want IDs for
#' @param wait number of seconds to wait between HTML pages as they are scraped
#'
#' @return
#' Games IDs for the top k games.
#'
#' @examples
#' top_500_game_ids <- top_k_games_ids(k = 500)
#' top_500_game_ids
top_k_games_ids <- function(k = 100, wait = 10) {
    # initialize
    games_ids_all <- c()
    
    # lower bound for k is 1
    if (k < 1) {
        message('Lower limit for K is 1')
        message('Setting K = 1 ...')
        k <- 1
    }
    
    # upper bound for k is 5000
    if (k > 5000) {
        message('Upper limit for K is 5000')
        message('Setting K = 5000 ...')
        k <- 5000
    }
    
    # loop on pages
    i <- 1
    while (length(games_ids_all) < k) {
        # retrieve page i
        page_i <- glue('https://boardgamegeek.com/search/boardgame/page/{i}?advsearch=1&q=&sort=rank&sortdir=asc')
        page_i <- read_html(page_i)
        page_i <- html_elements(page_i, xpath = '//*[@id="collectionitems"]')
        
        # grab game ids of page i
        games_ids <- html_attr(html_elements(page_i, 'tr > td > div > a'), 'href')
        games_ids <- str_extract(games_ids, '/[:digit:]+/')
        games_ids <- str_replace_all(games_ids, '/', '')
        
        # precaution: break in case of empty page
        if (length(games_ids) == 0)
            break
        
        # append
        games_ids_all <- c(games_ids_all, games_ids)
        
        # increment i
        i <- i + 1
        
        # duration to sleep so BGG website would not block us
        Sys.sleep(wait)
    }
    
    # keep k game ids
    if (length(games_ids_all) > 0) {
        # precaution: length(games_ids_all) may be lower than k
        k <- min(k, length(games_ids_all))
    
        # keep number of games requested
        games_ids_all <- games_ids_all[1:k]
    } else {
        message("That's odd... No games retrieved?")
    }
    
    # return
    games_ids_all
}



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
#' @examples
#' user_info <- user('alizat')
#' user_info
user <- function(username, buddies = 0, guilds = 0, hot = 0, top = 0) {
    # user collection link
    link <- paste0('https://boardgamegeek.com/xmlapi2/user?name=', username)
    link <- paste0(link, '&buddies=', buddies)
    link <- paste0(link, '&guilds=',  guilds)
    link <- paste0(link, '&hot=',     hot)
    link <- paste0(link, '&top=',     top)
    
    # obtain html page
    html_page <- read_html(link)
    
    # elements to parse features
    buddies <- if ( buddies        ) html_elements(html_page, 'buddies') else NULL
    guilds  <- if ( guilds         ) html_elements(html_page, 'guilds')  else NULL
    hott    <- if ( hot            ) html_elements(html_page, 'hot')     else NULL
    topp    <- if ( top            ) html_elements(html_page, 'top')     else NULL
    hots    <- if ( !is.null(hott) ) html_elements(hott,      'item')    else NULL
    tops    <- if ( !is.null(topp) ) html_elements(topp,      'item')    else NULL
    
    # extract user details
    features <-
        list(
            firstname        = 'firstname::value',
            lastname         = 'lastname::value',
            avatarlink       = 'avatarlink::value',
            yearregistered   = 'yearregistered::value',
            lastlogin        = 'lastlogin::value',
            stateorprovince  = 'stateorprovince::value',
            country          = 'country::value',
            webaddress       = 'webaddress::value',
            xboxaccount      = 'xboxaccount::value',
            wiiaccount       = 'wiiaccount::value',
            psnaccount       = 'psnaccount::value',
            battlenetaccount = 'battlenetaccount::value',
            steamaccount     = 'steamaccount::value',
            traderating      = 'traderating::value'
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



#' Specific Year's Games IDs
#'
#' @description \code{year_games_ids()} retrieves the ids of the games that came
#'   out in the specified year.
#'
#' @param y year to get games ids for
#' @param wait number of seconds to wait between HTML pages as they are scraped
#'
#' @return
#' Games IDs for the selected year, \code{y}.
#'
#' @examples
#' year_1995_game_ids <- year_games_ids(y = 1995)
#' year_1995_game_ids
year_games_ids <- function(y, wait = 10) {
    # initialize
    games_ids_all <- c()
    
    # xml link
    link_base <- 'https://boardgamegeek.com/search/boardgame'
    
    # params
    params <- glue('advsearch=1&q=&sort=rank&sortdir=asc&range[yearpublished][min]={y}&range[yearpublished][max]={y}&range[numvoters][min]=1')
    
    # get last page index
    last_page <- last_page_index(glue('{link_base}?{params}'))
    
    # loop on pages
    for (i in 1:last_page) {
        # retrieve page i
        page_i <- glue('{link_base}/page/{i}?{params}')
        page_i <- read_html(page_i)
        page_i <- html_elements(page_i, xpath = '//*[@id="collectionitems"]')
        
        # grab game ids of page i
        games_ids <- html_attr(html_elements(page_i, 'tr > td > div > a'), 'href')
        if (length(games_ids) == 0)
            break
        games_ids <- str_extract(games_ids, '/[:digit:]+/')
        games_ids <- str_replace_all(games_ids, '/', '')
        
        # append
        games_ids_all <- c(games_ids_all, games_ids)
        print(glue('{length(games_ids_all)} game ids collected so far'))
        
        # duration to sleep so BGG website would not block us
        if (i != last_page) {
            Sys.sleep(wait)
        }
    }
    
    # return
    games_ids_all
}
