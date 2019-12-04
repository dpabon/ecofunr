context("WUE")
data("ec_dd")

ET <- bigleaf::LE.to.ET(ec_dd$LE, ec_dd$Tair)


test_that("LE.to.ET works", {
        
        expect_equal(round(min(ET), 6), -1e-06)
        expect_equal(round(max(ET), 6), 5.1e-5)


})


test_that("wue", {
        
        wue_val <- wue(ec_dd$GPP, ec_dd$VPD, ET, '', method = 'wue', ec_dd$date, 
                       aggregation.time=NULL)
        expect_equal(round(min(wue_val), 2), -225.41)
        expect_equal(round(max(wue_val), 2), 2959.28)
        
        
        wue_mean <- wue(ec_dd$GPP, ec_dd$VPD, ET, '', method = 'wue', ec_dd$date, 
                        aggregation.time='year', aggregation.metric = 'mean')
        expect_true(all(round(wue_mean, 2) == c(24.03, 3.43, 2.66, 79.90)))
        
        
        wue_med <- wue(ec_dd$GPP, ec_dd$VPD, ET, '', method = 'wue', ec_dd$date, 
                       aggregation.time='year', aggregation.metric = 'median')
        expect_true(all(round(wue_med, 2) == c(9.63, 2.46, 2.02, 3.67)))
        
        
        wue_quant <- wue(ec_dd$GPP, ec_dd$VPD, ET, '', method = 'wue', ec_dd$date, 
                        aggregation.time='year', aggregation.metric = 'quantile', probs = 0.5)
        expect_true(all(round(wue_quant, 2) == c(9.63, 2.46, 2.02, 3.67)))
        
})

test_that("Iwue", {
        
        wue_val <- wue(ec_dd$GPP, ec_dd$VPD, ET, '', method = 'Iwue', ec_dd$date, 
                       aggregation.time=NULL)
        expect_equal(round(min(wue_val), 2), -41.46)
        expect_equal(round(max(wue_val), 2), 755.08)
        
        wue_mean <- wue(ec_dd$GPP, ec_dd$VPD, ET, '', method = 'Iwue', ec_dd$date, 
                        aggregation.time='year', aggregation.metric = 'mean')
        expect_true(all(round(wue_mean, 2) == c(4.04, 2.20, 2.25, 17.40)))
        
        
        wue_med <- wue(ec_dd$GPP, ec_dd$VPD, ET, '', method = 'Iwue', ec_dd$date, 
                       aggregation.time='year', aggregation.metric = 'median')
        expect_true(all(round(wue_med, 2) == c(2.08, 1.62, 1.99, 1.26)))
        
        
        wue_quant <- wue(ec_dd$GPP, ec_dd$VPD, ET, '', method = 'Iwue', ec_dd$date, 
                         aggregation.time='year', aggregation.metric = 'quantile', probs = 0.5)
        expect_true(all(round(wue_quant, 2) == c(2.08, 1.62, 1.99, 1.26)))
        
})


test_that("uwue", {
        
        wue_val <- wue(ec_dd$GPP, ec_dd$VPD, ET, '', method = 'uwue', ec_dd$date, 
                       aggregation.time=NULL)
        expect_equal(round(min(wue_val), 2), -96.68)
        expect_equal(round(max(wue_val), 2), 1494.82)
        
        wue_mean <- wue(ec_dd$GPP, ec_dd$VPD, ET, '', method = 'uwue', ec_dd$date, 
                        aggregation.time='year', aggregation.metric = 'mean')
        expect_true(all(round(wue_mean, 2) == c(9.61, 2.27, 2.15, 35.93)))
        
})


test_that("all", {
        
        wue_val <- wue(ec_dd$GPP, ec_dd$VPD, ET, 1, method = 'all', ec_dd$date, 
                       aggregation.time=NULL)
        expect_named(wue_val)
        expect_true(all(round(colMeans(wue_val), 1) == c(9.4, 0.0, 3.4, 4.9)))
        
        wue_mean <- wue(ec_dd$GPP, ec_dd$VPD, ET, 1, method = 'all', ec_dd$date, 
                        aggregation.time='year', aggregation.metric = 'mean')
        expect_true(all(round(wue_mean[1, ], 2) == c(24.03, 0, 4.04, 9.61)))
        
})

test_that("unknown_method", {
        
        expect_error(wue(ec_dd$GPP, ec_dd$VPD, ET, '', method = 'unknown_method', ec_dd$date, 
                         aggregation.time=NULL))
        
})

