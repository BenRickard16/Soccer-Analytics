## 3.1 Importing and Editing Data Files in R

# Installing required package
install.packages('readxl')
library(readxl)

# Dataset containing results for Arsenal's home games in the 2020/21 season
ArsenalHome <- read.csv("C:\\Users\\benan\\OneDrive\\Documents\\Coding Practice\\Datasets\\Arsenal_home_2020.csv")

# Variable names
names(ArsenalHome)

# Structure of the data frame
str(ArsenalHome)

# Create a working copy of the data
Ar_dat <- ArsenalHome

# We want to add new columns containing different measurements
# Create new variables populated with NAs
Ar_dat["GD"] <- NA      # Goal difference
Ar_dat["TG"] <- NA      # Total goals
Ar_dat["HTSR"] <- NA    # Home team shot ratio
Ar_dat["ATSR"] <- NA    # Away team shot ratio

# Populate the columns with calculated values
Ar_dat$GD <- Ar_dat$HG - Ar_dat$AG
Ar_dat$TG <- Ar_dat$HG + Ar_dat$AG
Ar_dat$HTSR <- round(Ar_dat$HS / (Ar_dat$AS + Ar_dat$HS), 3)
Ar_dat$ATSR <- round(Ar_dat$AS / (Ar_dat$AS + Ar_dat$HS), 3)

# Checking our work
names(Ar_dat)
head(Ar_dat, 8)


## 3.2 Diving Data into Subgroups

# Split the data into win, lose, and draw data frames
win <- Ar_dat[Ar_dat$Result == "W",]
lose <- Ar_dat[Ar_dat$Result == "L",]
draw <- Ar_dat[Ar_dat$Result == "D",]


## 3.3 Missing Data

# Create data with some missing data entries
players <- c("Paul","Stephen","James","Kevin","Tom","Edward","John","David")  # Players
shots <- c(2.4,3.6,0.3,1.1,4.2,2.3,NA,0.6)  # Average number of shots per game
goals <- c(0.2,0.6,0.0,0.1,0.7,0.3,0.1,0.0)   # Average number of goals per game
passes <- c(23.1,NA,39.2,25.5,18.6,37.4,28.3,28.3)  # Average number of passes per game
tackles <- c(6.3,4.5,10.6,9.8,4.1,5.3,11.2,7.8)   # Average number of tackles per game

# Create the data frame
perf_dat <- cbind.data.frame(players, shots, goals, passes, tackles)


# Remove lines with NA
na.omit(perf_dat)
# Lose a lot of the data, so better to adapt functions to deal with NAs

# Mean function
mean(perf_dat$shots, na.rm = TRUE)
mean(perf_dat$goals, na.rm = TRUE)
mean(perf_dat$passes, na.rm = TRUE)
mean(perf_dat$tackles, na.rm = TRUE)

# Or we can use the 'describeBy' function in the 'psych' package which automatically ignores NAs
install.packages('psych')
library(psych)
describeBy(perf_dat[,c(2:5)])


## 3.4 Importing Data from the Internet

# Loading a CSV into R from a website
EPL_2020_dat <- read.csv('https://www.football-data.co.uk/mmz4281/2021/E0.csv')

# Taking only the first 16 columns
EPL_2020_dat <- head(read.csv('https://www.football-data.co.uk/mmz4281/2021/E0.csv'), 380)[,1:16]

# Inspect data
names(EPL_2020_dat)
head(EPL_2020_dat, 10)

# Exporting the data frame
write.csv(EPL2020_dat, "C:\\Users\\benan\\OneDrive\\Documents\\Coding Practice\\Datasets\\EPL_results_2021.csv")

# Download results from website for the 5 seasons 2016-2020.
seasons <- c(rep("1617",1), rep("1718",1), rep("1819",1), rep("1920",1), rep("2021",1))

division <- c(rep(c("E0"),5)) # "E0" is the EPL and 5 refers to the number of seasons

urls = paste(seasons, division, sep="/")
urls = paste("https://www.football-data.co.uk/mmz4281", urls, sep="/")

# Load all the data using a loop and selecting just a few variables.
download_data = NULL
for(i in 1:length(urls)){
  temp = read.csv(urls[i])
  temp = temp[,c("Div","Date","HomeTeam","AwayTeam","FTHG","FTAG","FTR")]
  download_data = rbind(download_data, temp)
}

# Inspect data frame
head(download_data,10) # This displays the first 10 rows.
tail(download_data,10) # This displays the last 10 rows.


## 3.5 Harvesting Soccer Data from the Internet

# Need to use 'worldfootballR' package which must be installed from GitHub to harvest data from FBrev.com
# The data is coming directly from the webpage and isn't stored as a CSV
install.packages("devtools")
devtools::install_github("JaseZiv/worldfootballR", ref="main")
library(worldfootballR)

# URLs for all match reports in EPL 2020/2021
match_urls <- fb_match_urls(country = "ENG", gender = "M", tier = "1st", season_end_year = c(2021))

head(match_urls)

# Can now get a data frame containing the match events of  Fulham vs Arsenal on the 12th September 2020
match_summary <- fb_match_summary(match_url = "https://fbref.com/en/matches/bf52349b/Fulham-Arsenal-September-12-2020-Premier-League" )

print(match_summary[,c(19:25)])   # Print only desired columns

# Now we access squad performance data from FBref.com
EPL_2020_standard <- fb_season_team_stats(country = "ENG", gender = "M", season_end_year = "2021", 
                                         tier = "1st", stat_type = "standard")
names(EPL_2020_standard)

# Viewing only key statistics
print(EPL_2020_standard[,c(4,5,6,8,9,14,15)])

# Exporting as a CSV file
write.csv(EPL_2020_standard, "C:\\Users\\benan\\OneDrive\\Documents\\Coding Practice\\Datasets\\EPL_2020_standard.csv")

# Accessing player data from FBref.com
Ronaldo_shooting <- fb_player_season_stats("https://fbref.com/en/players/dea698d9/Cristiano->Ronaldo", 
                                           stat_type = 'shooting')

# Inspecting the data
names(Ronaldo_shooting)
head(Ronaldo_shooting[,c(1,3:5,9,11)], 20)


## 3.6 Scraping Soccer Data from the Internet

# Require the library 'rvest'
install.packages("rvest")
library(rvest)

# Selecting the relevant html section
css_selector <- "#mw-content-text > div.mw-content-ltr.mw-parser-output > table.wikitable"

# Read the html
tran_window <- read_html("https://en.wikipedia.org/wiki/Transfer_window")


# Scrape table and convert it to a data frame
tw_df <- as.data.frame(tran_window %>%
                         html_element(css = css_selector) %>%
                         html_table(fill=TRUE))

print(tw_df)



