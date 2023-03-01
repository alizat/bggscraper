test_that("multiplication works", {
    info <- guild(1299)
    assertthat::assert_that(length(info) == 2)
    assertthat::assert_that(identical(names(info), c('guild_info', 'members_info')))
    assertthat::assert_that(info$guild_info$guild_name == 'Toowoomba Boardgamers')
})
