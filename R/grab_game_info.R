grab_game_info <- function(game_link) {
    # game id
    game_id <- stringr::str_extract(game_link, '/[:digit:]+/')
    game_id <- stringr::str_replace_all(game_id, '/', '')

    # game details
    game_xml_link <- paste0('https://www.boardgamegeek.com/xmlapi2/thing?id=', game_id, '&stats=1')
    game_details <- rvest::read_html(game_xml_link)

    # features to extract
    features <-
        list(
            'name[type=primary]' = '',
            'yearpublished' = '',
            'description' = '',
            'minplayers' = '',
            'maxplayers' = '',
            'playingtime' = '',
            'minplaytime' = '',
            'maxplaytime' = '',
            'minage' = '',
            'link[type=boardgamecategory]' = '',
            'link[type=boardgamemechanic]' = '',
            'link[type=boardgamefamily]' = '',
            'link[type=boardgamedesigner]' = '',
            'link[type=boardgameartist]' = '',
            'link[type=boardgamepublisher]' = '',
            'ratings > usersrated' = '',
            'ratings > average' = '',
            'ratings > bayesaverage' = '',
            'ratings > ranks > rank[name="boardgame"]' = '',
            'ratings > owned' = '',
            'ratings > trading' = '',
            'ratings > wanting' = '',
            'ratings > wishing' = '',
            'ratings > numcomments' = '',
            'ratings > numweights' = '',
            'ratings > averageweight' = ''
        )

    # extract features
    for (feat in names(features)) {
        if (feat == 'description') {
            features[[feat]] <-
                game_details %>%
                html_elements("description") %>%
                html_text()
        } else {
            features[[feat]] <-
                game_details %>%
                html_elements(feat) %>%
                html_attr("value") %>%
                as.character()
        }
    }

    # adjust features names
    # TODO: modify names to be nicer
    #     e.g. 'ratings > averageweight' to be 'avg_weight'
    #     e.g. 'link[type=boardgamepublisher]' to be 'publisher'

    # return features
    features
}

# References
#   https://stackoverflow.com/questions/61053827/web-scraping-boardgamegeek-with-rvest
#   https://boardgamegeek.com/wiki/page/BGG_XML_API2
