fifa <- read.csv("fifa.csv")

# 2. Remove Newlines ---------------------------------------------------------

fifa[] <- lapply(fifa, function(x) gsub("\n", "", x)) #remove new line characters

# 1. Adjusting Height and Wight -------------------------------------------

heightVector <- unlist(strsplit(substr(fifa$Height, 0, nchar(fifa$Height) - 1), "'")) #removes backslash, splits the feet and inches and forms a vector
feet <- as.numeric(heightVector[c(T, F)]) #get the feet
inches <- as.numeric(heightVector[c(F, T)]) #get the inches

fifa$Height <- as.numeric(feet * 12 + inches) #a little advanced math to convert it all into inches

fifa$Weight <- substr(fifa$Weight, 0, nchar(fifa$Weight) - 3) #remove the "lbs" and convert it to a numeric

# 3. Time in the club --------------------------------------------------------

fifa$Joined <- as.Date(fifa$Joined, "%b %d, %Y") #convert Joined from a string to a date
today <- Sys.Date() #get today's date
oldies <- fifa[(today - fifa$Joined) > 365 * 10, "LongName"] #playes that have been with fifa for 10+ years
cat("Players that have been with fifa for 10+ years:", paste(head(oldies), combine=", "), "... \n") 

# 4. Money Adjustment ---------------------------------------------------------

getMoney <- function(moneyVector){
  cleanedVector <- c()
  for(moneyString in moneyVector){

    length <- nchar(moneyString)
    lastChar <- substr(moneyString, length, length)
    money <- 0
  
    if (lastChar == 'K'){
      money <- as.numeric(substring(moneyString, 2,  length - 1)) * 1000
    } else if (lastChar == 'M'){
      money <- as.numeric(substring(moneyString, 2, length - 1)) * 1000000
    } else {
      money <- as.numeric(substring(moneyString, 2, length))
    }
    
    cleanedVector <- append(cleanedVector, money)
    
  }

  cleanedVector
}


columns <- c("Wage", "Value", "Release.Clause")
fifa[, columns] <-apply(fifa[, columns], 2, getMoney)

# 5. Remove Stars ----------------------------------

columns <- c("W.F", "SM", "IR")
fifa[, columns] <- lapply(fifa[, columns], function(x) as.numeric(gsub("\U2605","", x))) #quick little substitution to remove the stars

# 6. Underpaid Players -------------------------------------------------------

underPaidPlayers = fifa$LongName[fifa$Wage < predict(lm(Wage ~ Value, fifa), fifa)] #does a linear regression and checks what players are below the predicted wage
cat("Underpaid players:", paste(head(underPaidPlayers), collapse=', '), "...")

# 7. Dummy Variables ------------------------------------------------------

fifa$left <- 0
fifa$left[fifa$foot == "Left"] <- 1

for(name in unique(fifa$BP)){
  fifa[,name] <- 0
  fifa[,name][fifa[,"BP"] == name] <- 1
}

