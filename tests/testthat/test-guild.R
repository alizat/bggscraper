test_that("guild info retrieval (Toowoomba Boardgamers)", {
    info <- guild(1299)
    assertthat::assert_that(length(info) == 2)
    assertthat::assert_that(identical(names(info), c('guild_details', 'members_info')))
    assertthat::assert_that(info$guild_details$guild_name == 'Toowoomba Boardgamers')
})
