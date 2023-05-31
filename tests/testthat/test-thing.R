test_that("retrieval of BG info for 'Dominion (Second Edition)'", {
    info <- thing(209418)
    assertthat::assert_that(info$name           == 'Dominion (Second Edition)')
    assertthat::assert_that(info$year_published == '2016')
    assertthat::assert_that(info$'designer'     == 'Donald X. Vaccarino')
})
