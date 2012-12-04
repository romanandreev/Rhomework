input <- "study-fall2012-intror-creditcard.txt"
output <- file("answers2.txt", "w")
printf <- function(a) {
  cat(file = output, a, "\n")
}
Months <- c(31,28,31,30,31,30,31,31,30,31,30,31)
leap <- function(y) {
  if (y %% 400 == 0){
    return (TRUE)
  }
  if (y %% 100 == 0){
    return (FALSE)
  }
  if (y %% 4 == 0){
    return (TRUE)
  }
  return (FALSE)
}
leap <- Vectorize(leap)
getDays <- function(a) {   
  y <- 2000 - ((a + 11) %/% 12)
  m <- (12 - a) %% 12 + 1  
  return (Months[m] + (m == 2 & leap(y)))
}
getDaysFromMonth <- function(a) {  
  if (a <= 0) {
    return (0)
  }  
  return (sum(getDays(1:a)))
}
getDaysFromMonths <- Vectorize(getDaysFromMonth)

data <- read.table(file = input, sep = ";", header = TRUE)
ndata <- data
ndata$"income per family member" <- ndata$income / (1 + ndata$dependents)
ndata$incomeGroup <- floor(ndata$income)
ndata <- ndata[order(ndata$income),]
ndata$days <- getDaysFromMonths(ndata$months)
write.table(file = "answers1.txt", subset(ndata, selfemp == "yes", select = - c(selfemp)), sep = ":", row.names = FALSE)
printf("2a)")
printf(colMeans(subset(data, owner == "no",select = dependents)))
printf("2b)")
write.table(aggregate(age~incomeGroup, FUN=mean, data = ndata), file = output, append = TRUE, row.names = FALSE)
printf("2c)")
ages  <- subset(data, card == "no")
ages <- ages[order(ages$age),]
printf("Youngest")
write.table(ages[1:5, ], file = output, append = TRUE, row.names = FALSE)
printf("Oldest")
write.table(ages[(nrow(ages) - 4):nrow(ages), ], file = output, append = TRUE, row.names = FALSE)
printf("2d)")
printf(mean(data[order(data$income, decreasing = TRUE)[1:10], "majorcards"]))

close(output)

