options(warnPartialMatchArgs = FALSE)
test_that("setup_parallel warns", {
    expect_warning(
        setup_parallel("not_a_number"),
        "`ncores` must be `NA` or a number, but you provided"
    )
    if (check_package_installed("doFuture")) {
        expect_warning(
            pc <- setup_parallel(9999999),
            "You specified 9999999 cores, but only"
        )
    } else {
        expect_warning(
            pc <- setup_parallel(9999999),
            "The packages `future`, `doFuture`, and `foreach` are required for using multiple cores"
        )
    }
    expect_false(pc)
})
test_that("setup_parallel works", {
    expect_false(setup_parallel(NA))
    if (check_package_installed("doFuture")) {
        expect_message(pc <- setup_parallel(2), "Using 2 cores for parallel processing.")
        expect_true(pc)
    } else {
        expect_warning(
            pc <- setup_parallel(2),
            "The packages `future`, `doFuture`, and `foreach` are required for using multiple cores"
        )
        expect_false(pc)
    }
})
