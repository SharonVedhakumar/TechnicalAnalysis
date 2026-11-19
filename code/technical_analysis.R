library(quantmod)

# Read portfolio symbols and download their historical prices.
load_stock_data <- function(portfolio_file = "portfolio.txt",
                            from = "2025-01-01",
                            to = "2026-01-01") {
  
  if (!file.exists(portfolio_file)) {
    stop("Portfolio file not found. Check its location.")
  }
  
  symbols <- readLines(portfolio_file, warn = FALSE)
  symbols <- unique(toupper(trimws(symbols)))
  symbols <- symbols[nzchar(symbols)]
  
  if (length(symbols) == 0) {
    stop("The portfolio file contains no stock symbols.")
  }
  
  stock_data <- list()
  
  for (symbol in symbols) {
    message("Downloading: ", symbol)
    
    stock_data[[symbol]] <- tryCatch({
      
      prices <- getSymbols(
        Symbols = symbol,
        src = "yahoo",
        from = from,
        to = to,
        auto.assign = FALSE
      )
      
      if (NROW(prices) == 0) {
        stop("No data returned.")
      }
      
      # Preserve dates when converting from xts to a data frame.
      df <- data.frame(
        Date = as.Date(zoo::index(prices)),
        zoo::coredata(prices),
        row.names = NULL
      )
      
      # Use consistent column names across stocks.
      names(df) <- sub(
        paste0(symbol, "."),
        "",
        names(df),
        fixed = TRUE
      )
      
      df
      
    }, error = function(e) {
      warning(
        paste("Could not load", symbol, ":", conditionMessage(e)),
        call. = FALSE
      )
      NULL
    })
  }
  
  return(stock_data)
}

# Run the function.
stock_data <- load_stock_data()

# Show which stocks loaded successfully.
print(names(stock_data))

# Show the first six rows and structure for each stock.
for (symbol in names(stock_data)) {
  cat("\nStock:", symbol, "\n")
  print(head(stock_data[[symbol]]))
  str(stock_data[[symbol]])
}




# Calculate statistics for one stock's data frame.
calculate_statistics <- function(stock_df, window = 20) {
  
  if (!"Close" %in% names(stock_df)) {
    stop("The data frame must contain a Close column.")
  }
  
  # Ensure observations are ordered by date.
  stock_df <- stock_df[order(stock_df$Date), ]
  prices <- stock_df$Close
  
  if (any(!is.finite(prices))) {
    stop("Close prices contain missing or non-finite values.")
  }
  
  if (length(prices) < window) {
    stop("Not enough observations for the moving average.")
  }
  
  # Statistical mode: retain all values tied for highest frequency.
  unique_prices <- unique(prices)
  frequencies <- tabulate(match(prices, unique_prices))
  
  if (max(frequencies) == 1) {
    mode_text <- "No repeated value"
  } else {
    modes <- unique_prices[frequencies == max(frequencies)]
    mode_text <- paste(sort(modes), collapse = ", ")
  }
  
  # Moving average series aligned with the original dates.
  moving_average <- TTR::SMA(prices, n = window)
  
  summary_table <- data.frame(
    Observations = length(prices),
    Mean_Close = mean(prices),
    Mode_Close = mode_text,
    Median_Close = median(prices),
    SD_Close = sd(prices),
    MA_Window = window,
    Latest_Moving_Average = tail(moving_average, 1),
    row.names = NULL
  )
  
  moving_average_table <- data.frame(
    Date = stock_df$Date,
    Close = prices,
    Moving_Average = as.numeric(moving_average)
  )
  
  return(list(
    summary = summary_table,
    moving_average = moving_average_table
  ))
}

# Calculate statistics for every successfully loaded stock.
stock_statistics <- lapply(
  stock_data,
  calculate_statistics,
  window = 20
)

# Combine individual summaries into one table.
statistics_summary <- do.call(
  rbind,
  lapply(names(stock_statistics), function(symbol) {
    data.frame(
      Symbol = symbol,
      stock_statistics[[symbol]]$summary,
      row.names = NULL
    )
  })
)

rownames(statistics_summary) <- NULL
print(statistics_summary)



# Display the first few rows for each stock.
display_stock_data <- function(stock_data, rows = 6) {
  
  for (symbol in names(stock_data)) {
    cat("\nStock:", symbol, "\n")
    cat("Number of observations:", nrow(stock_data[[symbol]]), "\n")
    
    print(head(stock_data[[symbol]], rows))
  }
}


# Display the combined statistics table.
display_statistics <- function(statistics_summary) {
  
  cat("\nClosing Price Statistics\n")
  print(statistics_summary, row.names = FALSE)
}


# Plot closing prices and their moving average for each stock.
plot_stock_data <- function(stock_statistics) {
  
  for (symbol in names(stock_statistics)) {
    
    plot_data <- stock_statistics[[symbol]]$moving_average
    window <- stock_statistics[[symbol]]$summary$MA_Window
    
    plot(
      plot_data$Date,
      plot_data$Close,
      type = "l",
      col = "blue",
      lwd = 1,
      main = paste(symbol, "Closing Price and Moving Average"),
      xlab = "Date",
      ylab = "Price (USD)",
      ylim = range(
        c(plot_data$Close, plot_data$Moving_Average),
        na.rm = TRUE
      )
    )
    
    lines(
      plot_data$Date,
      plot_data$Moving_Average,
      col = "red",
      lwd = 2
    )
    
    legend(
      "topleft",
      legend = c(
        "Closing price",
        paste(window, "observation moving average")
      ),
      col = c("blue", "red"),
      lty = 1,
      lwd = c(1, 2),
      bty = "n"
    )
  }
}


# Run the display functions.
display_stock_data(stock_data)
display_statistics(statistics_summary)
plot_stock_data(stock_statistics)
