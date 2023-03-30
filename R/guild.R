#' Guild Info
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
    link <- glue::glue('https://boardgamegeek.com/xmlapi2/guild?id={guild_id}&members={members}&sort={sort_by}')

    # html page
    page <- rvest::read_html(link)

    # guild info
    guild_details <- rvest::html_elements(page, 'guild')
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
    num_members <- as.numeric(rvest::html_attr(rvest::html_elements(page, 'members'), 'count'))
    num_pages <- ceiling(num_members / 25)
    members_info <- dplyr::tibble()
    for (page_no in 1:num_pages) {
        # page link
        page_link <- paste0(link, '&page=', page_no)

        # page itself
        page <- rvest::read_html(page_link)

        # members
        members_list <- rvest::html_elements(page, 'member')
        members_list <- features_extractor(elements = members_list, features = c('::name', '::date'))
        members_info <- rbind(members_info, members_list)
    }

    # return
    list(
        guild_details = guild_details,
        members_info  = members_info
    )
}
