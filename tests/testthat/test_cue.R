context("CUE")
data("ec_dd")


# NEE = NEP?
# Rb = Reco?


test_that("ecosystem", {
        
        cue_val <- cue(ec_dd$GPP, ec_dd$NEE, '', '', method='ecosystem', ec_dd$date,
                      aggregation.time = NULL)
        
        expect_equal(round(min(cue_val), 2), -484.96)
        expect_equal(round(max(cue_val), 2), 175.82)
        
        cue_mean <- cue(ec_dd$GPP, ec_dd$NEE, '', '', method='ecosystem', ec_dd$date,
                        aggregation.time = 'year', aggregation.metric = 'mean')
        
        expect_true(all(round(cue_mean, 2) == c(-0.20, -0.76, 1.08, -0.05)))
        
})


test_that("apparent", {
        
        cue_val <- cue(ec_dd$GPP, '', '', ec_dd$Reco, method='apparent', ec_dd$date,
                       aggregation.time = NULL)
        
        expect_equal(round(min(cue_val), 2), -175.82)
        expect_equal(round(max(cue_val), 2), 484.96)
        
        cue_mean <- cue(ec_dd$GPP, '', '', ec_dd$Reco, method='apparent', ec_dd$date,
                        aggregation.time = 'year', aggregation.metric = 'mean')
        
        expect_true(all(round(cue_mean, 2) == c(0.20, 0.76, -1.08, 0.05)))
        
})


test_that("unknown_method", {
        
        expect_error(cue(ec_dd$GPP, '', '', ec_dd$Reco, method='unknown_method', ec_dd$date,
                         aggregation.time = NULL))
        
})


