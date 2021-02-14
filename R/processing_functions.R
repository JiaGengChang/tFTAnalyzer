QualityControlTFTRecording <- function(){}
processTFTRecordingForExcel <- function(){}

processTFTRecordingForPrism <- function(TFTRecording, file=NA, delim='x'){
  bg.cols <- which(mapply(grepl, delim, names(dt0)))
  dt1 <- list(
    'ch1' = dt0[dt0$Channel==1, c(-2,-bg.cols)],
    'ch2' = dt0[dt0$Channel==2, c(-2,-bg.cols)],
    'bg1' = dt0[dt0$Channel==1, c(1, bg.cols)],
    'bg2' = dt0[dt0$Channel==2, c(1, bg.cols)]
  )
  dt1$'ch2/ch1' <- round(dt1$ch2/dt1$ch1,3)
  Time <- getTime(dt1$ch1$Slice)
  dt1$ch1 <- cbind(Time,dt1$ch1)
  dt1$ch2 <- cbind(Time,dt1$ch2)
  dt1$bg1 <- cbind(Time,dt1$bg1)
  dt1$bg2 <- cbind(Time,dt1$bg2)
  dt1$'ch2/ch1' <- cbind(Time,dt1$'ch2/ch1')
  dt1$'nCells' = length(names(dt0))/2-1
  n <- dt1$nCells
  coln <- c(
    'Time(h)',
    c('mNeonGreen', rep("", n-1)),
    c('mCherry', rep("", n-1)),
    c('mCherry/mNeon', rep("", n-1))
  )
  row0 <- c(NA,rep(names(dt1$ch1)[c(-1,-2)],3))
  df <- data.frame(dt1$ch1[,-2],
                   dt1$ch2[,c(-1,-2)],
                   dt1$'ch2/ch1'[,c(-1,-2)],
                   check.names = FALSE)
  df <- rbind(row0,df)
  colnames(df) <- coln
  rownames(df) <- seq(nrow(df))
  if (!is.na(file)){
    xlsx::write.xlsx(df, file, sheetName = "Sheet1",
               col.names = TRUE, row.names = FALSE, append = FALSE,
               showNA = FALSE)
  }
  invisible(df)
}

processTFTRecordingForR <- function(TFTRecording, file){
  file <- 'inst/extdata/sampleInputFromExcel.tsv'
  dt0 <- read.csv2(file=file,
                   sep='\t',
                   stringsAsFactors = FALSE)

}
