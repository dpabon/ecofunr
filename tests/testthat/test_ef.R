context("EF")
data("ec_dd")

test_that("EF works", {
        
        ef_val <- ef(ec_dd$LE, ec_dd$H, ec_dd$date, aggregation.time = NULL)
        
        expect_equal(round(min(ef_val), 2), -11.48)
        expect_equal(round(max(ef_val), 2), 44.28)
        
        ef_mean <- ef(ec_dd$LE, ec_dd$H, ec_dd$date, aggregation.time = 'year', aggregation.metric = 'mean')
        
        expect_true(all(round(ef_mean, 2) == c(0.78, 0.63, 0.60, 0.40)))
        
})