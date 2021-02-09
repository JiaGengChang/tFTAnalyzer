QualityControlTFTRecording <- function(){}
processtFTRecordingForExcel <- function(){}
processtFTRecordingForRPlot <- function(){}

processtFTRecordingForPrism <- function(TFTRecording, file){
  n <- TFTRecording$nCells
  coln <- c(
    'Time(h)',
    c('mNeonGreen', rep(NA, n-1)),
    c('mCherry', rep(NA, n-1)),
    c('mCherry/mNeon', rep(NA, n-1))
  )
  row0 <- c(NA,rep(names(TFTRecording$ch1)[c(-1,-2)],3))
  df <- data.frame(getTime(TFTRecording$ch1$Slice),
                   TFTRecording$ch1[,c(-1,-2)],
                   TFTRecording$ch2[,c(-1,-2)],
                   TFTRecording$'ch2/ch1'[,c(-1,-2)])
  df <- rbind(row0,df)
  colnames(df) <- coln
  library(xlsx)
  write.xlsx(df, file, sheetName = "Sheet1",
             col.names = TRUE, row.names = FALSE, append = FALSE,
             showNA = FALSE)
  invisible(df)
}

