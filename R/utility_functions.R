as.TFTRecording <- function(){}

is.TFTRecording <- function(){}

read.TFTRecording <- function(file, delim='x'){
  dt0 <- read.csv2(file=file,
                   sep='\t',
                   stringsAsFactors = FALSE)
  dt0 <- data.frame(mapply(FUN=as.numeric, dt0))
  stopifnot(names(dt0)[1:2] == c('Slice','Channel'))
  bg.cols <- which(mapply(grepl, delim, names(dt0)))
  dt1 <- list(
    'ch1' = dt0[dt0$Channel==1,-bg.cols],
    'ch2' = dt0[dt0$Channel==2, -bg.cols],
    'bg1' = dt0[dt0$Channel==1, c(1,2,bg.cols)],
    'bg2' = dt0[dt0$Channel==2, c(1,2,bg.cols)]
  )
  dt1$'ch2/ch1' <- round(dt1$ch2/dt1$ch1,3)
  dt1$'nCells' = length(names(dt0))/2-1
  return(dt1)
}

print.TFTRecording <- function(){}

write.TFTRecording <- function(){}

StopIfNotTFTRecording <- function(TFTRecording){}

getFrame <- function(slice){
  frame <- ceiling(slice/3)
  return(frame)
}

getTime <- function(slice){
  out <- floor(slice/3)
  return(out)
}

