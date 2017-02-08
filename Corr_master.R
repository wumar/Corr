# This script will test for correlations between a universe of instruments that can be
# grabbed from the IG markets website. There is a universe selection bog post that
# outlines the rationale for this script.

# Load Packages httr, jsonlite (for API data retrecal), quanstrat

library(httr)
library(jsonlite)
library(quantstrat)
options(stringsAsFactors = FALSE)

# First login to the IG API as already established 

site <- "https://api.ig.com/gateway/deal/session"      # variable for the site
tickers <- vector()      # create a ticker vector
epics <- vector()        # create a epic vector

# now authenticate with POST command, site and API in header and username and password in content
auth_out <- POST(url = site, 
                 config = (add_headers("X-IG-API-KEY" = IG_API,
                                       VERSION = 2,
                                       Accept = "application/json; charset=UTF-8",
                                       "Content-Type" = "application/json; charset=UTF-8"
                 )
                 ),
                 body = list("identifier" = usr, "password" = psswd),
                 encode = "json"
)

auth_headers <- headers(auth_out)            # export the headers as a list
cst_token = auth_headers$cst                 # and get the cst token.
x_token = auth_headers$`x-security-token`    # and the x_token

# The next step is to search for a instrument

search_string <- readline("Search IG markets for? ")          #Request search string from user  
search_site <- paste("https://api.ig.com/gateway/deal/markets?searchTerm=", search_string, sep = "")  

raw_search_results <- GET(url = search_site, 
                          config = (add_headers("X-IG-API-KEY" = IG_API,
                                                Accept = "application/json; charset=UTF-8",
                                                "Content-Type" = "application/json; charset=UTF-8",
                                                "CST" = cst_token,
                                                "X-SECURITY-TOKEN" = x_token)
                          )
)

# From the raw output of the GET request, the information needs to be presented as a dataframe

raw_content <- rawToChar(raw_search_results$content) # get the raw content
content <- fromJSON(raw_content)                     # use JSON light to parse, httr is no good  
search_df <- content[[1]]                            # get your dataframe
View(search_df)

# Now assemble lists symbols and their EPICs to use for assembling a dataframe

symbol_string <- readline("Symbol = ") # Ask for the symbol
epic_string <- readline("EPIC = ")     # Ask for the epic
tickers <- c(tickers,symbol_string)    # Assign to vector
epics <- c(epics, epic_string)         # Assign to Vector  

# Once all symbols and EPICs have been added make a data frame
ticker_epic <- data.frame(tickers, epics)

# Now use the data frame to interatively request weekly data from IG, zooming out a bit
# to conserve the data limit

start_date <- "2006-01-01"  # set a start date for the data
end_date <- "2017-01-01"    # set a end date for the data

for (epic in epics){        # this loop constructs the correct url and gets prices
  i <- match(epic, epics)
  epic_site <- paste("https://api.ig.com/gateway/deal/prices/",
                     epic,
                     "?resolution=WEEK&from=",
                     start_date,
                     "&to=",
                     end_date,
                     "&max=0&pageSize=0&pageNumber=0", #API doesn't work like in the docs, this is required syntax
                     sep = "")
  
  raw_data_results <- GET(url = epic_site, 
                          config = (add_headers("X-IG-API-KEY" = IG_API,
                                                Accept = "application/json; charset=UTF-8",
                                                "Content-Type" = "application/json; charset=UTF-8",
                                                "CST" = cst_token,
                                                VERSION = 3,  
                                                "X-SECURITY-TOKEN" = x_token
                          )
                          )
  )
  
  raw_data_content <- rawToChar(raw_data_results$content) # get the raw content
  data_content <- fromJSON(raw_data_content)              # use JSON light to parse, httr is no good  
  daily_data_df <- data_content[[1]]                      # grab the price data as a data frame
  # extract the prices we want in order to produce an xts object
  ohlc_df <- data.frame(daily_data_df$snapshotTimeUTC,
                        ((daily_data_df$openPrice$bid + daily_data_df$openPrice$ask)/2),
                        ((daily_data_df$highPrice$bid + daily_data_df$highPrice$ask)/2),
                        ((daily_data_df$lowPrice$bid + daily_data_df$lowPrice$ask)/2),
                        ((daily_data_df$closePrice$bid + daily_data_df$closePrice$ask)/2),
                        daily_data_df$lastTradedVolume
  )
  colnames(ohlc_df) <- c("Date","Open","High","Low","Close","Volume") # set column names
  ohlc_df["Adjusted"] <- 0                                            # column for adjusted
  xts1 <- xts(ohlc_df[, -1], order.by=as.Date(ohlc_df$Date))          # create xts object
  assign(tickers[i], xts1)                                            # assign xts object with its symbol
}

# The IG data allowance ran out before I was able to pull all the data so I need a new
# vector with the tickers I have

tickers_have <- tickers[1:18]

# And I should save as a csv the data I have already pulled for future reference

for (tick in tickers_have){
  csv_file <- paste("C:/Users/RJK/Documents/SpiderOak Hive/Financial/IGdata/weekly/",
                    tick, ".csv", sep ="")          # Generate the file name
  writedf <- data.frame(get(tick))      # revert the xts to a dataframe
  out <- write.csv(writedf,             # write to file
                   file = csv_file,
                   quote = FALSE, row.names = TRUE)
}

merged_prices <- do.call(merge, lapply(tickers_have, function(x) Cl(get(x))))
colnames(merged_prices) <- tickers_have
merged_returns <- do.call(merge, lapply(merged_prices, 
                                        function(x) periodReturn(x, period = 'weekly', type = 'log')))
colnames(merged_returns) <- tickers_have

chart.Correlation(merged_returns[,c(7:9)], method = "kendall")
chart.Correlation(merged_returns, method = "pearson")
chart.Correlation(merged_returns[,c(1,4:7,10:15,17,18)], method = "spearman")