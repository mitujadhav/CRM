
date_strings = c( "1.06.2012","19.09.2014")
datetimes = strptime(date_strings, format = "%d.%m.%Y")

diff_in_days = difftime(datetimes[2], datetimes[1], units = "days") # days
diff_in_days

