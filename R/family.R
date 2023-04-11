#' Family details
#'
#' @description \code{family()} retrieves the details for the specified family.
#'
#' @param family_id id of the desired family.
#' @param family_type filter results by the specified family type (if any).
#'   Valid values are \code{"boardgamefamily"} (default), \code{"rpg"} and
#'   \code{"rpgperiodical"}.
#'
#' @return
#' Data frame containing the details for the specified family.
#'
#' @examples
#' family(78462)
#' family(43631)
family <- function(family_id, family_type = 'boardgamefamily') {
    # family details
    link <- paste0('https://www.boardgamegeek.com/xmlapi2/family?id=', family_id)
    if (!is.null(family_type)) {
        link <- paste0(link, '&type=', family_type)
    }
    family_details <- rvest::read_html(link)
    item_details <-
        features_extractor(
            rvest::html_elements(family_details, 'item'),
            list(
                item_type      = '::type',
                item_id        = '::id',
                item_thumbnail = 'thumbnail',
                item_image     = 'image',
                item_name      = 'name::value',  # 'name[type="primary"]::value'
                description    = 'description'
            )
        )
    item_links <-
        features_extractor(
            rvest::html_elements(family_details, 'item > link'),
            list(
                link_type    = '::type',
                link_id      = '::id',
                link_value   = '::value',
                link_inbound = '::inbound'
            )
        )
    item_details$links <- item_links

    # return
    item_details
}
