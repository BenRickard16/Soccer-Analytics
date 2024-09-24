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


## 4.2 Producing Head-to-Head Statistics from Historical Match Data

# Downloading histoircal data for the 5 seasons 2016/17 to 2020/21
seasons <- c(rep("1617", 1), rep("1718", 1), rep("1819", 1), rep("1920", 1), rep("2021", 1))
division <- c(rep(c("E0"), 5))

urls <- paste(seasons, division, sep='/')
urls <- paste("https://www.football-data.co.uk/mmz4281", urls, sep="/")

# For loop to load the data in
download_data = NULL
for (i in 1:length(urls)){
  temp = read.csv(urls[i])
  temp = temp[,c("Div", "Date", "HomeTeam", "AwayTeam", "FTHG", "FTAG", "FTR", "HS", "AS", "HST", "AST")]
  download_data = rbind(download_data, temp)
}

# Checking the download has worked
head(download_data)
tail(download_data) 

# Now we want only the H2H matches involving Arsenal and Chelsea
teamH <- 'Arsenal'
teamA <- 'Chelsea'

# Can extract these games using an indicator variable
n <- nrow(download_data)
ind <- matrix(0, n, 1) # nx1 matrix full of zeros
ndat <- cbind.data.frame(download_data, ind)

# Populate with a 1 if we have Arsenal vs Chelsea
for (i in 1:n){
  if(ndat$HomeTeam[i] == teamH & ndat$AwayTeam[i] == teamA){ndat$ind[i] <- 1}
}

# Selecting only the games we want
H2H <- ndat[ndat$ind == 1,]

# Adding shots ratio to these games
library(dplyr)
H2H <- H2H %>%
  dplyr::mutate(HTSR = round(HS / (HS + AS), 3)) %>%
  dplyr::mutate(ATSR = round(AS / (HS + AS), 3))

print(H2H)


## 4.3 Producing PiT League Tables from Historical Match Data

#Load in the historical match data - first 98 EPL games and first seven variables
PiT_dat <- head(read.csv('https://www.football-data.co.uk/mmz4281/2021/E0.csv'), 98)[,1:7]

# To use our functions, we need to define the 4 variables
HomeTeam <- PiT_dat$HomeTeam
AwayTeam <- PiT_dat$AwayTeam
HomeGoals <- PiT_dat$FTHG
AwayGoals <- PiT_dat$FTAG

# Function 1. Creates a vector of match outcomes
outcome <- function(hGoals, aGoals){
  nMatches <- length(hGoals)
  results <- matrix(NA, nMatches, 1)
  
  for (i in 1:nMatches){
    if (hGoals[i] > aGoals[i]) {results[i] <- 'H'}
    if (hGoals[i] < aGoals[i]) {results[i] <- 'A'}
    if (hGoals[i] == aGoals[i]) {results[i] <- 'D'}
  }
  return(results)
}

# Function 2. This creates a current league table from the match results data
create.table <- function(hTeam, aTeam, hGoals, aGoals){
  
  # Harvest team names and collate in to a vector
  teams.temp <- unique(hTeam)
  (teams <- sort(teams.temp)) # Arrange in alphabetical order.
  nTeams = length(teams) # This identifies the number of teams in the league.
  
  # Create a vector containing the match outcomes (i.e. H, A or D)
  results <- outcome(hGoals, aGoals)
  
  # Create empty vectors to store results.
  x <- numeric(nTeams)
  hWins <- x; hLoss <- x; hDraws <- x;
  aWins <- x; aLoss <- x; aDraws <- x;
  goals.for <- x; goals.against <- x; goal.diff <- x;
  matches.played <- x; pts <- x;
  
  # Populate vectors
  for (i in 1:nTeams) {
    hResults <- results[hTeam == teams[i]]
    aResults <- results[aTeam == teams[i]]
    matches.played[i] <- length(hResults) + length(aResults)
    goals.H <- sum(hGoals[hTeam == teams[i]])
    goals.A <- sum(aGoals[aTeam == teams[i]])
    goals.for[i] <- goals.H + goals.A
    conceded.H <- sum(aGoals[hTeam == teams[i]])
    conceded.A <- sum(hGoals[aTeam == teams[i]])
    goals.against[i] <- conceded.H + conceded.A
    goal.diff[i] <- goals.for[i] - goals.against[i]
    hWins[i] <- sum(hResults == "H")
    hDraws[i] <- sum(hResults == "D")
    hLoss[i] <- sum(hResults == "A")
    aWins[i] <- sum(aResults == "A")
    aDraws[i] <- sum(aResults == "D")
    aLoss[i] <- sum(aResults == "H")
    
    # Compute total points from the number of wins and draws for the respective teams.
    # Points awarded for the match outcomes
    win.pts <- 3
    draw.pts <- 1
    pts[i] <- (win.pts*(hWins[i] + aWins[i])) + (draw.pts * (hDraws[i] + aDraws[i]))
  }
  
  table <- data.frame(cbind(matches.played, hWins, hDraws,hLoss, aWins, aDraws, aLoss,
                            goals.for, goals.against, goal.diff, pts), row.names=teams)
  
  names(table) <- c("PLD", "HW", "HD", "HL", "AW", "AD", "AL", "GF", "GA", "GD", "PTS")
  ord <- order(-table$PTS, -table$GD, -table$GF)
  table <- table[ord, ]
  return(table)
}

# Applying the 2 functions to produce the league table
League.table <- create.table(HomeTeam, AwayTeam, HomeGoals, AwayGoals)
print(League.table)


## 4.4 Compiling PiT Feature Tables from Historical Match Data

# Load in the match data for 2020-2021 selecting first 20 variables, and 98 matches
fb_data <- head(read.csv('https://www.football-data.co.uk/mmz4281/2021/E0.csv'), 98)[, 1:20]

# Select only the variables we want in the analysis
soc_dat <- fb_data[, c("Date", "HomeTeam", "AwayTeam", "FTHG", "FTAG", "FTR", "HS", "AS", "HST", 
                       "AST", "HC", "AC")]

# Add some new derived variables
soc_dat["HPts"] <- 0 # This creates the new column to store home team points per match 
soc_dat["APts"] <- 0 # This creates the new column to store away team points per match 

for(i in 1:nrow(soc_dat)){
  if(soc_dat$FTR[i] == "H"){soc_dat$HPts[i] <- 3}
  if(soc_dat$FTR[i] == "A"){soc_dat$APts[i] <- 3}
  if(soc_dat$FTR[i] == "D") {soc_dat$HPts[i] <- 1}
  if(soc_dat$FTR[i] == "D") {soc_dat$APts[i] <- 1}
}

# Rename variables
colnames(soc_dat)[colnames(soc_dat) == 'FTHG'] <- 'HG'
colnames(soc_dat)[colnames(soc_dat) == 'FTAG'] <- 'AG'
colnames(soc_dat)[colnames(soc_dat) == 'FTR'] <- 'Result'

# Inspect data
head(soc_dat)

# Create user-defined function ‘feature.Calc’ to compute PiT feature scores for any given team
feature.Calc <- function(df, team){
  Hmatches <- df[df$HomeTeam == team,] # This selects the target team's home matches.
  Amatches <- df[df$AwayTeam == team,] # This selects the target team's away matches.
  all <- rbind.data.frame(Hmatches,Amatches)
  n <- nrow(all) # Number of matches
  
  # Create empty vectors to store results.
  x <- numeric(n)
  GF <- x; GA <- x; SF <- x; SA <- x; STF <- x; STA <- x;
  CF <- x; CA <- x; Pts <- x;
  
  # Goals for
  for(i in 1:n){
    if(all$HomeTeam[i] == team){GF[i] <- all$HG[i]}
    else {GF[i] <- all$AG[i]}
  }
  
  # Goals against
  for(i in 1:n){
    if(all$HomeTeam[i] == team){GA[i] <- all$AG[i]}
    else {GA[i] <- all$HG[i]}
  }
  
  # Shots for
  for(i in 1:n){
    if(all$HomeTeam[i] == team){SF[i] <- all$HS[i]}
    else {SF[i] <- all$AS[i]}
  }
  
  # Shots against
  for(i in 1:n){
    if(all$HomeTeam[i] == team){SA[i] <- all$AS[i]}
    else {SA[i] <- all$HS[i]}
  }
  
  # Shots on target for
  for(i in 1:n){
    if(all$HomeTeam[i] == team){STF[i] <- all$HST[i]}
    else {STF[i] <- all$AST[i]}
  }
  
  # Shots on target against
  for(i in 1:n){
    if(all$HomeTeam[i] == team){STA[i] <- all$AST[i]}
    else {STA[i] <- all$HST[i]}
  }
  
  # Corners for
  for(i in 1:n){
    if(all$HomeTeam[i] == team){CF[i] <- all$HC[i]}
    else {CF[i] <- all$AC[i]}
  }
  
  # Corners against
  for(i in 1:n){
    if(all$HomeTeam[i] == team){CA[i] <- all$AC[i]}
    else {CA[i] <- all$HC[i]}
  }
  
  # Points awarded
  for(i in 1:n){
    if(all$HomeTeam[i] == team){Pts[i] <- all$HPts[i]}
    else {Pts[i] <- all$APts[i]}
  }
  
  Pld <- matrix(1,n,1) # Vector containing matches played
  GD <- GF-GA
  TG <- GF+GA
  TSRF <- SF/(SF+SA)
  TSRA <- SA/(SF+SA)
  
  feats <- cbind.data.frame(Pld,GF,GA,GD,TG,SF,SA,STF,STA,TSRF,TSRA,CF,CA,Pts)
  featsSums <- colSums(feats)
  featsRes <- featsSums
  nOb <- nrow(feats)
  featsRes[10] <- featsSums[10]/nOb # This compute the average TSRF.
  featsRes[11] <- featsSums[11]/nOb # This computes the average TSRA.
  return(round(featsRes,2))
}

# Applying function to Tottenham and Man City
Tot.features <- feature.Calc(soc_dat, 'Tottenham')
print(Tot.features)

MC.features <- feature.Calc(soc_dat, 'Man City')
print(MC.features)

# We now want to apply the function the all teams in turn
Teams <- unique(soc_dat$HomeTeam)
Teams <- sort(Teams) # Put them in alphabetical order
nTeams <- length(Teams) # Number of teams
print(Teams)

featureRes <- matrix(NA,nTeams,14)
for(i in 1:nTeams){
  featureRes[i,] <- feature.Calc(soc_dat, Teams[i])
}

# Compile feature table results
featureTab <- cbind.data.frame(Teams, featureRes)
colnames(featureTab) <- c("Team","Pld","GF","GA","GD","TG","SF","SA","STF",
                          "STA","TSRF","TSRA","CF","CA","Pts")
print(featureTab)

# Now we want to produce a scatter plot of TSRF and points awarded to check for correlation
x <- featureTab$TSRF
y <- featureTab$Pts

# Correlation test
cor.test(x,y)

# Scatter plot
plot(x,y, pch=20, xlim=c(0.2,0.8), ylim=c(0,25), xlab="Average TSRF score", ylab="Points earned")
text(y~x, labels=Teams, cex=0.8, font=1, pos=4)  # This puts team names on the data points. 
abline(lm(y~x), lty=2) # This draws a least squares best fit line through the data points.
