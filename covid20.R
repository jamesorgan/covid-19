library(rio)

setwd("~/Statistics/Datasets")

covid <- import("time-series-19-covid-combined.csv")
covid$Date <- as.Date.character(covid$Date)

timestamp()
cat("Last updated", paste0(max(covid$Date)), "\n")

UK <- covid[covid$`Country/Region` == "United Kingdom",]

country <- function(
  country, province = "all", output = T, plot = T, data = covid)
  {
  
  # This dataset will be the basis of the rest of the analysis
  
  # Option for worldwide dataset
  if (country %in% c("World", "world", "all")) {
    new_df <- aggregate(. ~ Date, FUN = sum, 
              data = data[,c("Date","Confirmed","Recovered","Deaths")])
  }
  
  # Option for dataset excluding China
  else if (country == "!China"){
    new_df <- aggregate(. ~ Date, FUN = sum, 
              data = data[data$`Country/Region` != "China",
                          c("Date","Confirmed","Recovered","Deaths")])
  } 
  
  # Aggregate all areas of a country
  else if (province == "all") {
    new_df <- data[data$`Country/Region` == country, ]
    new_df <- aggregate(. ~ Date,
              FUN = sum,
              data = new_df[,c("Date","Confirmed","Recovered","Deaths")])
  } 
  
  # Everything else
  else {
    new_df <- data[
      data$`Country/Region` == country & data$`Province/State` == province
      , ]
  }
  
  # Make Active cases column
  new_df$Active <- new_df$Confirmed - (new_df$Recovered + new_df$Deaths)
  
  # Make daily totals
  # Confirmed
  prev_day <- c(NA,new_df$Confirmed[-length(new_df$Confirmed)])
  new_df$C_Daily <- new_df$Confirmed - prev_day
  
  # Growth Rate
  new_df$Growth <- new_df$Confirmed/prev_day
  
  #Recovered
  prev_day <- c(NA,new_df$Recovered[-length(new_df$Recovered)])
  new_df$R_Daily <- new_df$Recovered - prev_day
  #Deaths
  prev_day <- c(NA,new_df$Deaths[-length(new_df$Deaths)])
  new_df$D_Daily <- new_df$Deaths - prev_day
  
  # Mortality Rate
  new_df$Mortality <- new_df$Deaths/(new_df$Recovered + new_df$Deaths)
  
  
  # Make some pretty plots
  if (plot == TRUE) {
    
    # Time series of dailys
    plot(new_df$Date, new_df$C_Daily, type = "l",
         main = c("Daily Cases of COVID-19 in ", country),
         xlab = "Date", ylab = "Cases each day")
    lines(new_df$Date, new_df$D_Daily, col = "red")
    lines(new_df$Date, new_df$R_Daily, col = "green")
    legend("topleft",
           legend = c("Confirmed cases", "Deaths", "Recovered"),
           col = c("black", "red", "green"), lty = 1, bty = "n")
    
    # Time series of cumulatives
    plot(new_df$Date,new_df$Confirmed, type = "l",
         main = c("Cumulative Cases of COVID19 in ", country),
         xlab="Date", ylab="Cumulative cases")
    lines(new_df$Date, new_df$Deaths, col = "red")
    lines(new_df$Date, new_df$Recovered, col = "green")
    lines(new_df$Date, new_df$Active, col = "blue")
    legend("topleft",
           legend = c("Confirmed cases", "Deaths", "Recovered", "Active"),
           col = c("black", "red", "green", "blue"), lty = 1, bty = "n")
  }
  
  #Ouput the df
  if (output == TRUE) {
    new_df
  }
  
}


# Doubling rate
  # Powers of 2
  # powers <- 2^seq(0,by=1,length.out = 10)

# Current mortality rate with and without China
cat("Current worldwide mortality rate including China",
    country("World",plot=F)$Mortality[-1:-70],
    "\nand not including China",
    country("!China",plot=F)$Mortality[-1:-70])

# Predict Growth Rate
GrowthTrend <- lm(Growth ~ Date, data = notChina)
plot(notChina$Date, notChina$Growth)
abline(a = GrowthTrend$coefficients[1], b = GrowthTrend$coefficients[2])

country("United Kingdom",plot=T,output=F)
