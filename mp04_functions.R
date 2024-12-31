
convert_fred_datatype=function(fred_data){
  fred_data=fred_data|>
    mutate(date=as.Date(date),value=as.numeric(value))|>
    group_by(date)|>
    summarise(value = mean(value, na.rm = TRUE), .groups = "drop")
  return(fred_data)
}

  
  convert_to_a_table<-function(symbol_data)
  { do.call(
    rbind, 
    lapply(
      names(symbol_data$`Monthly Adjusted Time Series`), 
      function(date) {
        # Extract the record for the given date
        record <- symbol_data$`Monthly Adjusted Time Series`[[date]]
        
        # Check if all required fields are present
        if (!is.null(record$`1. open`) && 
            !is.null(record$`2. high`) && 
            !is.null(record$`3. low`) && 
            !is.null(record$`4. close`) && 
            !is.null(record$`6. volume`) && 
            !is.null(record$`7. dividend amount`)) {
          
          # Create a data frame for non-missing records
          return( data=data.frame(
            date = as.Date(date),                      # Convert date string to Date
            open = as.numeric(record$`1. open`),
            high = as.numeric(record$`2. high`),
            low = as.numeric(record$`3. low`),
            close = as.numeric(record$`4. close`),
            adjusted_close = as.numeric(record$`5. adjusted close`),
            volume = as.numeric(record$`6. volume`),
            dividend = as.numeric(record$`7. dividend amount`),
            stringsAsFactors = FALSE
          ))
        } else {
          # Skip records with missing fields
          return(NULL)
        }
      }
    )
  )
  }
  
  fetch_alpha_vantage <- function(symbol, function_name = "TIME_SERIES_MONTHLY_ADJUSTED") {
    av_key <- Sys.getenv("AV_KEY")
    url <- paste0(
      "https://www.alphavantage.co/query?",
      "function=", function_name,
      "&symbol=", symbol,
      "&apikey=", av_key
    )
    
    # Make the API request
    response <- GET(url)
    
    if(status_code(response)==200){
      # Parse and print the JSON data
      data <- content(response, as = "parsed")
      data_table<-data|>convert_to_a_table()
      data_table=data_table|>mutate(month=floor_date(as.Date(date), "month"))|>group_by(month) |>
        summarize(adjusted_close = last(adjusted_close))
      return(data_table)
    }
    else
    {
      stop("failed to fetch data")
    }
  }
  
  
  # Function to fetch data from FRED
  fetch_fred <- function(series_id) {
    fred_key <- Sys.getenv("FRED_KEY")
    real_time_start="1980-01-01"
    real_time_end=Sys.Date()
    frequency="m"
    url2 <- paste0("https://api.stlouisfed.org/fred/series/observations?",
                   "series_id=",series_id,
                   "&realtime_start=",real_time_start,
                   "&realtime_end=",real_time_end,
                   "&frequency",frequency,
                   "&api_key=",fred_key,
                   "&file_type=json")
    response_t=GET(url2)
    
    
    # Check response status
    if (status_code(response_t) == 200) {
      content<-content(response_t)
      my_list<-content$observations|>as.list()
      my_table <- do.call(rbind, lapply(my_list, function(x) {
        data.frame(
          realtime_start = x$realtime_start,
          realtime_end   = x$realtime_end,
          date           = x$date,
          value          = x$value,
          stringsAsFactors = FALSE
        )
      }))
      
      
    } else {
      stop("Failed to fetch data from FRED.")
    }
  }
monte_carlo_simulations<-function(return_vect,ndays = 1000, n_sim = 100){
  set.seed(0)
  return_vect=1+return_vect
  paths<-replicate(n_sim,
                   expr=round(sample(return_vect,ndays,replace=TRUE),2)
                   )
  #to seee if there are any tail events
  paths<-apply(paths,2,cumprod)
  
  paths<-data.table::data.table(paths)
  paths$days<-1:nrow(paths)
  paths<-data.table::melt(paths,id.vars="days")
 
  return(paths)
}

visualize_simulations=function(path)
{
  ggplot(path,aes(x=days,y=(value-1)*100,col=variable))+
    geom_line()+
    theme_bw()+
    theme(legend.position="none")+
    
    xlab("Days Invested")+
    ylab("Portfolio Return (%)")
}

  
  # Calculate returns and rename the column
  ret_vect <- alpha_vantage_symbol_data %>%
    mutate(returns = adjusted_close / lag(adjusted_close) - 1) %>%
    rename(!!return_column_name := returns) %>%
    select(month, all_of(return_column_name))
  
  return(ret_vect)
}


calculate.return=function(alpha_vantage_symbol_data){
  ret_vect<-alpha_vantage_symbol_data$adjusted_close/lag(alpha_vantage_symbol_data$adjusted_close)-1
  return(ret_vect)
}
calculate_monthly_average_return=function(alpha_vantage_symbol_data){
  ret_df=alpha_vantage_symbol_data|>group_by(month)|>
    summarize(average_monthly_return=round(mean(returns,na.rm=TRUE),3),
              `average_monthly_return_(%)`=average_monthly_return*100)|>na.omit()|>  # Remove NA values for better plotting
    mutate(direction = ifelse(`average_monthly_return_(%)` >= 0, "Increasing", "Decreasing"),
           year = year(month), 
           month = as.Date(paste0(month, "-01")))
  return(ret_df)
}
calculate_annual_average_return=function(average_return_data){
  rect_df<-average_return_data %>%
    group_by(year) %>%
    summarise(`annual_return_(%)` = round((prod(1 + average_monthly_return, na.rm = TRUE)^(1 / 12) - 1),5)*100)
  return(rect_df)
}
