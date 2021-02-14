as.TFTRecording <- function(){}

is.TFTRecording <- function(){}

read.TFTRecording <- function(file){
  dt0 <- xlsx::read.xlsx(file,sheetIndex=1)
  dt0 <- data.frame(mapply(FUN=as.numeric, dt0))
  stopifnot(names(dt0)[1:2] == c('Slice','Channel'))
  return(dt0)
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

