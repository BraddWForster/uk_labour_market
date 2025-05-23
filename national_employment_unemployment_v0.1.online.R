##    Programme:  R
##
##    Objective: Plot employment and unemployment in the UK through since 1995-2025.
##
##    Plan of  : 
##    Attack   : 
##
##    Note: Seasonally adjusted and in thousands and three-month rolling average and this is for people aged 16-64.
##
##               1. Load in and tidy data.
##               2. Basic calculations.
##
##    Author   :  Bradd Forster
##
##  Clear the decks and load up some functionality
    ##
    rm(list=ls(all=TRUE))

##  Core libraries
    ##
    library(stringr)
    library(lubridate)
    library(ggplot2)
    library(readxl)
    
##  Optional libraries
    ##
    library(scales)
    
##  Set up paths for working directories
    ##
    userid <- "bradd"
    r <- paste0("C:/Users/", userid, "/...")
    raw <- paste0("C:/Users/", userid, "/...")
    processed <- paste0("C:/Users/", userid, "/...")
    uk_raw <- paste0("C:/Users/", userid, "/...")
    
##  Setting the working directory
    ##
    setwd(uk_raw)

################################################################################
## 1.                Load and tidy national employment data                   ##
################################################################################

df <- read_excel("a01may2025.xls", sheet = 3)
df <- as.data.frame(df)

first_row <- which(grepl(pattern = "jan", df[,1], ignore.case = TRUE))[[1]]

last_row <- which(grepl(pattern = "1. ", df[,1], ignore.case = TRUE))[[1]]-2

columns <- c(1, 11:15)

tidy <- df[first_row:last_row, columns]

colnames(tidy) <- c("Date", "All", "EconActive", "Employed", "Unemployed", "EconInactive")

tidy$Year <- as.numeric(str_sub(tidy$Date, start = str_length(tidy$Date)-4, end = str_length(tidy$Date)))

tidy$QuarterEnd <- str_trim(str_sub(tidy$Date, start = 5, end = 8))
tidy$QuarterEnd <- str_replace(tidy$QuarterEnd, pattern = "Sep", replacement = "Sept")

tidy[,2:7] <- lapply(tidy[2:7], as.numeric)

tidy$Date <- as.Date(paste0("01-",tidy$QuarterEnd,"-",tidy$Year), format = "%d-%b-%Y")


################################################################################
## 2.                           Basic calculations                            ##
################################################################################

df <- tidy[tidy$QuarterEnd == "Mar" & tidy$Year == 2025 | tidy$QuarterEnd == "Mar" & tidy$Year == 2024,]

df$UmpRate <- df$Unemployed/df$EconActive
df$EmpRate <- df$Employed/df$All

df[1,2:7] - df[2,2:7]

tidy$UmpRate <- tidy$Unemployed/tidy$EconActive


################################################################################
## 3.                         Plot the unemployment rate                      ##
################################################################################
start_date <- as.Date("1995-01-01")

p <- ggplot(data = tidy[tidy$Date >= start_date,]) +
  geom_line(aes(x = Date, y = UmpRate), color = "orange") +
  scale_y_continuous(labels = percent_format())+
  labs(y = "Rate (%)", x = NULL, title = "United Kingdom Unemployment Rate") +
  theme(
    plot.background = element_rect(fill = "#132E35", color = NA),
    panel.background = element_rect(fill = "#132E35", color = NA),
    legend.background = element_rect(fill = "#132E35", color = NA),
    legend.text = element_text(color = "#AFB3B7"),
    legend.title = element_text(color = "#AFB3B7"),
    axis.title = element_text(color = "#AFB3B7"),
    axis.text = element_text(color = "#AFB3B7"),
    title = element_text(color = "#AFB3B7")
  )

setwd(processed)

png("uk_ump_rate_v0.1.png",  width = 2000, height = 1600, res = 300)
print(p)
dev.off()
