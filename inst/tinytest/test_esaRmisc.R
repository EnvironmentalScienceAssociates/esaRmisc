
# wy_yday
expect_equal(wy_yday(c("2016-10-01", "2017-03-01",
                       "2016-03-01", "2017-09-30",
                       "2016-09-30")),
             c(1, 152, 153, 365, 366))
expect_error(wy_yday("02/19/78"))
expect_error(wy_yday(100))

# wy_week
expect_equal(wy_week(seq(as.Date("2022-10-01"), as.Date("2023-09-30"), by = "1 week")),
             1:53)
expect_error(wy_week("02/19/78"))
expect_error(wy_week(100))

# wy_date
expect_equal(wy_date(c(1, 152, 365), 2017),
             as.Date(c("2016-10-01", "2017-03-01", "2017-09-30")))
expect_equal(wy_date(c(153, 366), 2016),
             as.Date(c("2016-03-01", "2016-09-30")))

# water_year
expect_equal(water_year(c("2022-10-01", "2022-12-31", "2023-01-01", "2023-09-30")),
             rep(2023, 4))
