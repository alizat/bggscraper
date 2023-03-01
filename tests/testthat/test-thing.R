test_that("retrieval of BG info for 'Nemesis'", {
    info <- thing("209418")
    assertthat::assert_that(info$value[info$feature == 'name']           == 'Dominion (Second Edition)')
    assertthat::assert_that(info$value[info$feature == 'year_published'] == '2016')
    assertthat::assert_that(info$value[info$feature == 'designer']       == 'Donald X. Vaccarino')
})
