# if(!require(dplyr)){
#   install.packages('dplyr',dependencies = TRUE,repos='http://cran.us.r-project.org')
# }
#
#
# require(dplyr)


#' Reads all the records in a SCID file (Version 1).
#'
#' First reads the header and checks for file format and version.
#' Then computes the number of records in the file and reads them one by one.
#' Finally, merges all the records in one data.frame.
#' See Intraday Data File Format documentation for details about this file format.
#'
#' @usage
#'
#' SCID.to.data.frame(filename)
#'
#' @param filename (character) path to a scid file.
#' @return data.frame with columns: DateTime, Open, High, Low, Last, NumberOfTrades, Volume, BidVolume, AskVolume
#'
#' @author Juan Luis Herrera Cortijo \email{juan.luis.herrera.cortijo@gmail.com}
#'
#' @references
#'
#' Intraday Data File Format: \url{https://www.sierrachart.com/index.php?page=doc/IntradayDataFileFormat.html}
#'
#' @export
#' @import dplyr

SCID.to.data.frame <- function(filename){

  # Get thefile size
  f.size<- file.info(filename)$size

  #Open a connection
  f <- file(filename,"rb")



  # Read the header and do some checkings

  FileTypeUniqueHeaderID <- readBin(f,"integer",n=4,size=1)

  FileTypeUniqueHeaderID <- intToUtf8(FileTypeUniqueHeaderID)

  stopifnot(FileTypeUniqueHeaderID=="SCID")



  HeaderSize <- readBin(f,"integer",n=1,size=4)

  stopifnot(HeaderSize==56)



  RecordSize <- readBin(f,"integer",n=1,size=4)

  stopifnot(RecordSize==40)



  Version <- readBin(f,"integer",n=1,size=2)

  stopifnot(Version==1)



  Unused1 <- readBin(f,"integer",n=1,size=2)

  UTCStartIndex <- readBin(f,"integer",n=1,size=4)

  stopifnot(UTCStartIndex==0)

  Reserve <- intToUtf8(readBin(f,"integer",n=36,size=1))



  # Compute number of records in the file
  n.records <- (f.size-HeaderSize)/RecordSize



  # Read all the records

  data <- lapply(seq(1,n.records),function(i){

    # Read the data in the record
    DateTime <- readBin(f,"double",n=1)

    DateTime <- as.POSIXct(DateTime*3600*24,tz="UTC",origin="1899-12-30")

    open_high_low_close <- readBin(f,"numeric",n=4,size=4)

    names(open_high_low_close) <- c("Open","High","Low","Last")

    numtrades_vol <- readBin(f,"integer",n=4,size=4)

    names(numtrades_vol) <- c("NumberOfTrades","Volume","BidVolume","AskVolume")

    #Create a data.frame

    open_high_low_close <- t(open_high_low_close)

    numtrades_vol <- t(numtrades_vol)

    data.frame(DateTime,open_high_low_close,numtrades_vol)


  })



  close(f)

  # Merge all the data.frames

  data <- bind_rows(data)

  data

}


