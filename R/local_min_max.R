#' Check if a price is a local point
#' @param current_price actual price
#' @param surrended_price the price before and after
#' @param searchtype Are you looking for min or max
utility_local_point  <- function(current_price, surrended_price, searchtype='min'  ) {
  if (searchtype=='min') {
    return(sum(current_price>surrended_price)==0)
  }else if(searchtype=='max'){
    return(sum(current_price<surrended_price)==0)
  }
}

#' Add local points to charts
#' @export
#' @param df dataframe cointains financela data
#' @param number_of_days how many days to check for min or max
#' @importFrom data.table setDT
utility_ad_local_min_max <- function(df, number_of_days=20) {
  #df <- aranykez::stock_get_one_ticker('TSLA', start_date = '2018-01-01')
  # number_of_days=20
  df <- setDT(df)
  df$text <- ''
  df$rise <- ''
  df$fall <- ''
  for (i in 1:(nrow(df))) {
    if (i==nrow(df)) {
      current_min <- df$low[i]
      surounding_min <- c(df$low[ ifelse((i-number_of_days )<0, 0, (i-number_of_days )) : (i-1)  ] )

    }else{
      current_min <- df$low[i]
      surounding_min <- c(df$low[ ifelse((i-number_of_days )<0, 0, (i-number_of_days )) : (i-1)  ] ,
                          df$low[ (i+1):    ifelse((i+number_of_days)>nrow(df), nrow(df), (i+number_of_days) ) ])
    }
    mine <- utility_local_point(current_min, surounding_min,searchtype = 'min')
    if (mine) {
      df$text[i] <- 'minimum'
      next()
    }
    if (i==nrow(df)) {
      current_max <- df$high[i]
      surounding_max <- c(df$high[ ifelse((i-number_of_days )<0, 0, (i-number_of_days )) : (i-1)  ] )

    }else{
      current_max <- df$high[i]
      surounding_max <- c(df$high[ ifelse((i-number_of_days )<0, 0, (i-number_of_days )) : (i-1)  ] ,
                          df$high[ (i+1):    ifelse((i+number_of_days)>nrow(df), nrow(df), (i+number_of_days) ) ])

    }

    maxe <- utility_local_point(current_max, surounding_max, searchtype = 'max')
    if (maxe) {
      df$text[i] <- 'maximum'
      next()
    }
  }
  local_points <- which(df$text!='')

  # init message
  df$message <- ''
  if (df$text[local_points[1]] =='minimum') {
    df$message[local_points[1]] <- paste0('$', round(df$low[local_points[1]],2) , '\n', df$date[local_points[1]]   )
  }
  if (df$text[local_points[1]] =='maximum') {
    df$message[local_points[1]] <- paste0('$', round(df$high[local_points[1]],2) , '\n', df$date[local_points[1]]  )
  }

  # remove false local points
  temp_local_points <- NULL
  for (i in 2:length(local_points)) {
    if ( df$text[local_points[i]] !=df$text[local_points[ (i-1) ]]  ) {

      if (length(temp_local_points)>0 | i==length(local_points) ) {
        temp_local_points <- c(temp_local_points, (i-1) )

        if (df$text[local_points[i]]=='maximum') {
          lowest_low <- which.min(df$low[local_points[temp_local_points]]  )
          fake_lows <- temp_local_points[-lowest_low]
          df$text[ local_points[ fake_lows ]  ] <-''
        }

        if (df$text[local_points[i]]=='minimum') {
          highest_high <- which.max(df$high[local_points[temp_local_points]]  )
          fake_highs <- temp_local_points[-highest_high]
          df$text[ local_points[ fake_highs ]  ] <-''
        }

      }
      temp_local_points <- NULL
    }else{
      temp_local_points <- c(temp_local_points, (i-1) )
    }
  }

  # go thrugh on clean local points and ad text
  local_points <- which(df$text!='')
  for (i in 2:length(local_points) ) {
    if ( df$text[local_points[i]] =='minimum') {
      df$fall[local_points[i] ] <- round((1- (df$low[local_points[i]]/ df$high[local_points[i-1]]) )*100,2)
      df$message[local_points[i] ]<- paste0( '$', round(df$low[local_points[i] ],2) ,', down: ',round((1- (df$low[local_points[i]]/ df$high[local_points[i-1]]) )*100,2), '%', '\n', df$date[local_points[i]] )

    }else if(df$text[local_points[i]] =='maximum' ){
      my_up <- round(  ((df$high[local_points[i]] /df$low[local_points[i-1]])-1)   *100,2)
      df$rise[local_points[i] ] <- my_up

      my_up <- ifelse(my_up >=100, paste0(  round(((my_up+100)/100),2)  , 'x' ),  paste0(my_up, '%')  )
      df$message[local_points[i] ] <-  paste0( '$', round(df$high[local_points[i] ],2) , ', up: ',my_up , '\n', df$date[local_points[i]])
    }
  }
  return(df)
}

