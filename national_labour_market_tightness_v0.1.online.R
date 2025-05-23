##    Programme:  R
##
##    Objective: Plot recent labour tightness in the UK since 2015-2025.
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

df <- read_excel("a01may2025.xls", sheet = "20")
df <- as.data.frame(df)

first_row <- which(grepl(pattern = "2001", df[,1], ignore.case = TRUE))[[1]]

last_row <- which(grepl(pattern = "1. ", df[,1], ignore.case = TRUE))[[1]]-2

columns <- c(1, 3, 4)

tidy <- df[first_row:last_row, columns]

colnames(tidy) <- c("Date", "Vac", "Ump")

tidy$Year <- as.numeric(str_sub(tidy$Date, start = str_length(tidy$Date)-4, end = str_length(tidy$Date)))

tidy$QuarterEnd <- str_trim(str_sub(tidy$Date, start = 5, end = 8))
tidy$QuarterEnd <- str_replace(tidy$QuarterEnd, pattern = "Sep", replacement = "Sept")

tidy[,2:4] <- lapply(tidy[2:4], as.numeric)

tidy$Date <- as.Date(paste0("01-",tidy$QuarterEnd,"-",tidy$Year), format = "%d-%b-%Y")


################################################################################
## 2.                           Basic calculations                            ##
################################################################################

df <- tidy[tidy$QuarterEnd == "Mar" & tidy$Year == 2025 | tidy$QuarterEnd == "Mar" & tidy$Year == 2024,]

df$UmpRate <- df$Unemployed/df$EconActive
df$EmpRate <- df$Employed/df$All

df[1,2:7] - df[2,2:7]

tidy$Tightness <- tidy$Ump/tidy$Vac


################################################################################
## 3.                         Plot the unemployment rate                      ##
################################################################################
start_date <- as.Date("2015-01-01")

p_1 <- ggplot(data = tidy[tidy$Date >= start_date,]) +
  geom_line(aes(x = Date, y = Tightness), color = "orange") +
  labs(y = "Ratio", x = NULL, title = "United Kingdom Inverse Labour Market Tightness") +
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

long <- reshape(data = tidy[,1:3], varying = list(c("Vac", "Ump")), v.names = "Count", times = c("Vac", "Ump"), direction = "long")

colnames(long) <- c("Date", "Measure", "Count", "Id")

p_2 <- ggplot(data = long[long$Date >= start_date,]) +
  geom_line(aes(x = Date, y = Count, color = Measure)) +
  labs(y = "Count", x = NULL, title = "United Kingdom Labour Market") +
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
p_2

png("uk_tightness_components_v0.1.png",  width = 2000, height = 1600, res = 300)
print(p_2)
dev.off()
