## 4.1 Compiling Team Performance Statistics from Historical Match Data

# Importing the data from EPL 2020/21 season
EPL2020_data <- head(read.csv('https://www.football-data.co.uk/mmz4281/2021/E0.csv'),380)[,1:16]

# Inspecting the data
names(EPL2020_data)

# Selecting only the columns we want
dat <- EPL2020_data[,c("Date","HomeTeam","AwayTeam","FTHG","FTAG",
                       "FTR","HS","AS","HST","AST")]   # This creates a working data frame
head(dat) # Displays first six rows of dat

# Want to augment the data by adding new derived variables
library(dplyr)

dat <- dat %>%
  dplyr::mutate(GD = FTHG - FTAG,    # Goal difference
                TG = FTHG + FTAG,    # Total goals
                HTSR = round(HS / (HS + AS), 3),    # Home team shots ratio
                ATSR = round(AS / (HS + AS), 3)    # Away team shots ratio
         ) %>%
  # Renaming some columns
  dplyr::rename(HG = FTHG) %>%
  dplyr::rename(AG = FTAG) %>%
  dplyr::rename(Result = FTR) %>%
  # Columns with points awarded to home and away team
  dplyr::mutate(
    HPts = case_when(
      Result == "H" ~ 3,
      Result == "D" ~ 1,
      TRUE ~ 0
    ),
    APts = case_when(
      Result == "A" ~ 3,
      Result == "D" ~ 1,
      TRUE ~ 0
    )
  )

# Checking the data
head(dat)
     
# Now we will assess the performance of Liverpool during the season
study.team <- 'Liverpool'

# Extracting the data just for Liverpool
home.matches <- dat[dat$HomeTeam == study.team,]
away.matches <- dat[dat$AwayTeam == study.team,]

# Need to add a status variable to show whether home or away
home.matches['Status'] <- 'Home'
away.matches['Status'] <- 'Away'

# Copies of the data
home <- home.matches
away <- away.matches

# Inspect the data
head(home)
head(away)

# Can now rename the variables in terms of for and against
home <- dplyr::rename(home, c("GF"="HG","GA"="AG","SF"="HS","SA"="AS",
                       "STF"="HST","STA"="AST","TSRF"="HTSR","TSRA"="ATSR",
                       "PF"="HPts","PA"="APts"))

# Now, we replace the ‘H’, ‘A’ and ‘D’ elements in Results vector with 'W', 'L' and 'D'.
home$Result <- dplyr::recode(home$Result, "H"="W", "A"="L", "D"="D")
head(home)

# Now we repeat the process for the 'away' data frame.
away <- dplyr::rename(away, c("GA"="HG","GF"="AG","SA"="HS","SF"="AS",
                       "STA"="HST","STF"="AST","TSRA"="HTSR","TSRF"="ATSR",
                       "PA"="HPts","PF"="APts"))

away$GD <- -1*away$GD # Change sign on goal difference to reflect use of 'for' and 'against'.

# Replace elements in Results vector with 'W', 'L', 'D'.
away$Result <- dplyr::recode(away$Result, "H"="L", "A"="W", "D"="D")
head(away)

# Now we will produce some descriptive statistics
# Use 'psych' package 
library(psych)

# Home matches descriptive statistics
home.temp <- home[,c(4,5, 7:16)] # Variables selected for statistical analysis
H.stats <- describeBy(home.temp) # These are the descriptive statistics
H.sums <- colSums(home.temp) # Column sums
home.des <- cbind.data.frame(H.stats$n, H.stats$mean, H.stats$median, H.stats$sd, H.sums) 
colnames(home.des) <- c("Pld", "Mean", "Median", "SD", "Total") # Rename variables
print(round(home.des,3)) # Display the home match descriptive statistics

# Away matches descriptive statistics
away.temp <- away[,c(4,5, 7:16)] # Variables selected for statistical analysis
A.stats <- describeBy(away.temp) # These are the descriptive statistics
A.sums <- colSums(away.temp) # Column sums
away.des <- cbind.data.frame(A.stats$n, A.stats$mean, A.stats$median, A.stats$sd, A.sums) 
colnames(away.des) <- c("Pld", "Mean", "Median", "SD", "Total") # Rename variables
print(round(away.des,3)) # Display the home match descriptive statistics

# Produce descriptive statistics for all the matches in the season.
all <- rbind(home, away)
# All matches descriptive statistics
all.temp <- all[,c(4,5, 7:16)] # Variables selected for statistical analysis
all.stats <- describeBy(all.temp) # These are the descriptive statistics
all.sums <- colSums(all.temp) # Column sums
all.des <- cbind.data.frame(all.stats$n, all.stats$mean, all.stats$median, all.stats$sd, all.sums) 
colnames(all.des) <- c("Pld", "Mean", "Median", "SD", "Total") # Rename variables
print(round(all.des,3)) # Display the home match descriptive statistics

# Producing box plots of home and away shots on target
HSTF <- home$STF    # Home shots on target for
HSTA <- home$STA    # Home shots on target against
ASTF <- away$STF    # Away shots on target for
ASTA <- away$STA    # Away shots on target against
SoT <- cbind.data.frame(HSTF,HSTA,ASTF,ASTA)
boxplot(SoT, ylab="Shots on target")







 