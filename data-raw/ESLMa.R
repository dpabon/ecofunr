# half hourly dataset

ec_hh <- read.table('data-raw/ESLMa_MainTower.txt', header = TRUE)

doy2date <- function(doy, year) {
        start = ISOdate(year, 1, 1, 0, 0, 0)
        dif = as.difftime(doy - 1, units = "days")
        date = start + dif
        return(date)
        
}

ec_hh$date <- doy2date(ec_hh$doy, ec_hh$year) + as.difftime(ec_hh$hour, units='hours')

usethis::use_data(ec_hh)

# daily dataset

ec_hh <- subset(ec_hh, select=-c(date,hour))
ec_dd <- aggregate(ec_hh, by=list(ec_hh$year, ec_hh$doy), mean, na.rm=TRUE)

ec_dd <- subset(ec_dd, select=-c(Group.1, Group.2))
ec_dd$date <- doy2date(ec_dd$doy, ec_dd$year)
ec_dd <- ec_dd[order(ec_dd$date), ]
        
usethis::use_data(ec_dd)
