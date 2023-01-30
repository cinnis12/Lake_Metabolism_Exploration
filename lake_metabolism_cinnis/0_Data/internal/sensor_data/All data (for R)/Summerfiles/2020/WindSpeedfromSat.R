rm(list = ls())

###First for Holland Basin
##
#
wndsat <- read.csv(file = "/Users/josep/Desktop/Summerfiles/2020/Holland_RawSatWind.csv",
                   header = T)

#formats data and corrects for timezone differences
wndsat$datetime <- strptime(wndsat$time, format = "%Y-%m-%dT%H:%M:%SZ") - 21600 

wndsat$wnd_10 <- sqrt(wndsat$vgrd10m^2 + wndsat$ugrd10m^2)

wnd_cor <- wndsat[wndsat$latitude == 47.5,]
wnd_cor <- wnd_cor[,6:7]

final_wnd <- wnd_cor[rep(seq_len(nrow(wnd_cor)), each = 6), ]

final_wnd$datetime <- seq.POSIXt(from = wnd_cor$datetime[1],
                                 by = "30 min",
                                 length.out = nrow(final_wnd))

write.csv(final_wnd, file = "/Users/josep/Desktop/Summerfiles/2020/Holland_wndsat.csv")


###
### Now McDonald
rm(list=ls())
###

wndsat <- read.csv(file = "/Users/josep/Desktop/Summerfiles/2020/McDonald_RawSatWind.csv",
                   header = T)

#formats data and corrects for timezone differences
wndsat$datetime <- strptime(wndsat$time, format = "%Y-%m-%dT%H:%M:%SZ") - 21600 

wndsat$wnd_10 <- sqrt(wndsat$vgrd10m^2 + wndsat$ugrd10m^2)

wnd_cor <- wndsat[wndsat$latitude == 47.5,]
wnd_cor <- wnd_cor[,6:7]

final_wnd <- wnd_cor[rep(seq_len(nrow(wnd_cor)), each = 6), ]

final_wnd$datetime <- seq.POSIXt(from = wnd_cor$datetime[1],
                                 by = "30 min",
                                 length.out = nrow(final_wnd))

write.csv(final_wnd, file = "/Users/josep/Desktop/Summerfiles/2020/McDonald_wndsat.csv")

