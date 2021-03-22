#' Downloading and adding technical indicators to the historical data of a ticker
#' @export
#' @param ticker The ticker of the company to download
#' @param start_date The first date of the historical data
#' @param end_date The last date of the data
#' @param mas List of the simple moving averages to calculate
#' @importFrom  data.table data.table
#' @importFrom rtsdata ds.getSymbol.yahoo
#' @importFrom TTR RSI
#' @importFrom pracma movavg
stock_get_one_ticker  <- function(ticker, start_date = "2000-01-01", end_date = Sys.Date(),  mas=c(50, 100, 200)) {
  tryCatch({
    df <- data.frame(ds.getSymbol.yahoo(ticker, from = (as.Date(start_date)-250), to =end_date ))
    names(df) <- tolower(sapply(strsplit(names(df), '.', fixed = T), '[[', 2))
    df$date <- as.Date(row.names(df))
    row.names(df) <- 1:nrow(df)
    df <- data.table(df)

    if( !identical(names(df) , c("open","high","low", "close","volume",  "adjusted","date"))) {
      text<- paste0('Error: ', ticker, ' # problem: names of dataframe is bad ', ' time: ', Sys.time())
      stop(text)
    }

  }, error=function(x) {
    print(x)
    stop('No ticker')
  })

  for (simple_mas in mas) {
    if (nrow(df)<=simple_mas) {
      df[[paste0('ma_', simple_mas, '_value')]] <- 0

    }else{
      df[[paste0('ma_', simple_mas, '_value')]] <- movavg( df[['close']], simple_mas,type = 's' )
      df[[paste0('diff_',simple_mas,'_ma_value')]] <-  (( df[["close"]]  /df[[paste0('ma_', simple_mas, '_value')]] )-1)*100
      df[[paste0('ma_', simple_mas, '_value')]] <- NULL
    }
  }

  df$rsi <- RSI(as.numeric(df$high),n = 14)
  df <- df[date >= as.Date(start_date) & !is.na(rsi),]
  return(df)
}
