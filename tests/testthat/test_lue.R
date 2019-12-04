context("LUE")
data("ec_dd")


test_that("LUE", {
        
        lue_val <- lue(ec_dd$GPP, ec_dd$PPFD * 0.8, ts=FALSE)
        
        expect_equal(round(min(lue_val), 6), -1e-06)
        
        
})


test_that("LUE", {
        
        lue_val <- lue(ec_dd$GPP, ec_dd$PPFD * 0.8)

        
})