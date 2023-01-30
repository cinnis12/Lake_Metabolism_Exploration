#fixes columns (within load.data)
coln <- function(df) {
  colnames(df) <- c("sens_add", "date", "time", "windSpeed", 
                    "windDirection", "gustWindSpeed", "airTemp",
                    "xOrient", "yOrient", "null", "northWindSpeed", 
                    "eastWindSpeed")
  colnames(df)
}

# fixes times (within load.data)
# must externally define sensorStartDateTime
timefix <- function(df,ssdt) {
  sensorOriginDate = as.Date(x = "2000-01-01 00:00:00")
  df$date <- as.Date(df$date, format = "%d/%m/%y")
  df$senstime <- parse_date_time(paste(df$date, df$time), 
                                 "ymd_HMS")
  difftime(df$senstime, sensorOriginDate) + 
    ssdt
  
}

#loads data and cleans it up
#requires above packages
load.data <- function(file){
  wnd <- read.csv(file = file, header = T)
  
  colnames(wnd) <- coln(wnd)
  wnd$datetime <- timefix(wnd, ssdt = sensorStartDateTime)
  
  wnd
}

#plots windspeed with another variable
#must externally define location (Hol, Uph, or Sapp)
serplot <- function(df, location, var1 = "windSpeed", var2) {
  par(mar = c(2.2,4,2,4))
  v1 <- which(colnames(df) == var1)
  v2 <- which(colnames(df) == var2)
  yeet <- pretty(c(0,ceiling(range(df[[v1]])[2]))) #defines common axis tics
  yeet2 <- pretty(c(floor(range(df[[v2]])[1]), 
                    ceiling(range(df[[v2]])[2])))
  dateyeet <- pretty(range(df$datetime))
  
  plot(x=df$datetime, y = df[[v1]], col = "blue", ylim = range(yeet),
       xlab = "date and time", type = 'l', lwd = 0.75,
       main = location, xaxt = 'n', yaxt = 'n',
       ylab = "")
  axis(2, col = "blue", at = yeet, labels = yeet)
  mtext(2, text = "wind speed (m/s)", line = 2)
  axis(1, at = dateyeet, labels = dateyeet)
  grid(ny = length(dateyeet))
  
  par(new=T)
  plot(x=df$datetime, y=df[[v2]], col ="red",ylim = range(yeet2),
       axes=F, xlab ="", type = 'l', lwd = 0.5, main = "",
       xaxt = 'n', yaxt = 'n', ylab ="")
  axis(4, col = "red", at = yeet2, labels = yeet2, line = 0)
  mtext(side = 4, text = var2, 
        line = 2.5)
}

#writes just date/time and windspeed to output csv 
#calls external floc
wsfile <- function(wnd) {
  wnds <- dplyr::select(wnd, datetime, windSpeed)
  write.csv(wnds, file = paste("Research/Summer 2019/WindSpeed/",floc, "/MetabFiles/", 
                               sensorStartDate, ".txt", sep = ""))
}

##taken from github - loads minidot data hella
read_minidot <- function(fname, skip = 3, ...)
{
  files <- list.files(fname, full.names=TRUE)
  if(requireNamespace('data.table', quietly=TRUE))
  {
    dat <- data.table::rbindlist(lapply(files, data.table::fread, skip = skip, ...))
  } else {
    dat <- do.call(rbind, lapply(files, read.csv, skip = skip, ...))
  }
  
  # drop battery column
  if(ncol(dat) == 5) 
    dat <- dat[,-2]
  
  colnames(dat) <- c('time_sec', 'temperature', 'DO', 'q')
  dat$timestamp <- as.POSIXct(dat[[1]], origin="1970-01-01")
  #dat$timestamp <- strptime(df$timestamp)
  dat
}

##loads hobo pendant data from a folder with multiple depths and puts them in same dataframe
#make sure input csvs are labelled as depth.csv (single number depth)
load.hobo <- function(pat, do) {
  
  setwd(pat)
  df <- ""
  file.names <- dir(pat, pattern =".csv")
  sort(file.names)
  
  for(i in 1:length(file.names)){
    file <- read.csv(file.names[i],header=T, skip = 1)
    if (ncol(file) == 3) {
      file$irr = NA
      colnames(file) <- c(paste("index", "_", str_remove(file.names[i], ".csv"),sep =""),
                          "datetime",
                          paste("wtr", "_", str_remove(file.names[i], ".csv"),sep =""),
                          paste("irr", "_", str_remove(file.names[i], ".csv"),sep =""))
    }
    else {
      colnames(file) <- c(paste("index", "_", str_remove(file.names[i], ".csv"),sep =""),
                          "datetime",
                          paste("wtr", "_", str_remove(file.names[i], ".csv"),sep =""),
                          paste("irr", "_", str_remove(file.names[i], ".csv"),sep =""))
    }
    
    if (i == 1) {
      df <- cbind(df, file[2:4])
    } else {
      df <- merge(df, file[2:4], by = 'datetime')
    }
  }
  
  df$datetime <- strptime(df$datetime, format = '%m/%d/%y %I:%M:%S %p') # fixes time before final merge
  
  dooby <- do[,c(6,2)]  ## adds temp data from Minidot at 1 m
  names(dooby) <- c("datetime", "wtr_1")
  
  df$datetime <- as.POSIXct(df$datetime)
  
  df <- merge(df, dooby, by = 'datetime')
  
  # time is in GMT format, so we need to fix it to MDT timezone
  # df$datetime <- with_tz(ymd_hms(df$datetime),'America/Denver')
  #
  tdf <- df[,c(1,grep("wtr", names(df)))]
  irrdf <- df[,c(1,grep("irr", names(df)))]
  
  
  write.csv(tdf, file = paste(pat,"/wtr_summer.txt", sep = ""))
  write.csv(irrdf, file = paste(pat,"/irr_summer.txt", sep = ""))
}
