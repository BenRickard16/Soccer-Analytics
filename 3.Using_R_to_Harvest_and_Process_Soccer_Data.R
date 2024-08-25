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
EPL2020_dat <- read.csv('https://www.football-data.co.uk/mmz4281/2021/E0.csv')

# Taking only the first 16 columns
EPL2020_dat <- head(read.csv('https://www.football-data.co.uk/mmz4281/2021/E0.csv'), 380)[,1:16]

# Inspect data
names(EPL2020_dat)
head(EPL2020_dat, 10)

# Exporting the data frame
write.csv(EPL2020_dat, "C:\\Users\\benan\\OneDrive\\Documents\\Coding Practice\\Datasets\\EPL_results_2021.csv")

