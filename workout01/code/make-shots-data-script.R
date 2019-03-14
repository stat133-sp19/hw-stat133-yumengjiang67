#title:GSW player data 
#description: The script tidy the raw data of Golden State Warriors's players nad make thm into a csv data.frame.
#Input(s): stephen-curry.csv andre-iguodala.csv draymond-green.csv kevin-durant.csv klay-thompson.csv
#output(s): what are the outputs created when running the script?

#Read in the five data sets, using relative file paths;   
curry <- read.csv("../data/stephen-curry.csv", stringsAsFactors = FALSE)
iguodala <- read.csv("../data/andre-iguodala.csv",stringsAsFactors = FALSE)
green <- read.csv("../data/draymond-green.csv", stringsAsFactors = FALSE)
durant <- read.csv("../data/kevin-durant.csv",stringsAsFactors = FALSE)
thompson <- read.csv("../data/klay-thompson.csv",stringsAsFactors = FALSE)

#Add a column name to each imported data frame, that contains the name of the corresponding player:
curry$name <- c("Stephen Curry")
iguodala$name <- c("Andre Iguodala")
green$name <- c("Graymond Green")
durant$name <- c("Kevin Durant")
thompson$name <- c("Klay Thompson")

#Change the original values of shot_made_flag to more descriptive values:
curry$shot_made_flag[curry$shot_made_flag=="n"] <- "shot_no"
curry$shot_made_flag[curry$shot_made_flag=="y"] <- "shot_yes"
durant$shot_made_flag[durant$shot_made_flag=="y"] <- "shot_yes"
durant$shot_made_flag[durant$shot_made_flag=="n"] <- "shot_no"
green$shot_made_flag[green$shot_made_flag=="y"] <- "shot_yes"
green$shot_made_flag[green$shot_made_flag=="n"] <- "shot_no"
iguodala$shot_made_flag[iguodala$shot_made_flag=="y"] <- "shot_yes"
iguodala$shot_made_flag[iguodala$shot_made_flag=="n"] <- "shot_no"
thompson$shot_made_flag[thompson$shot_made_flag=="y"] <- "shot_yes"
thompson$shot_made_flag[thompson$shot_made_flag=="n"] <- "shot_no"

#Add a column minute that contains the minute number where a shot occurred.
curry$minute <- curry$period*12-curry$minutes_remaining
durant$minute <- durant$period*12-durant$minutes_remaining
green$minute <- green$period*12-green$minutes_remaining
iguodala$minute <- iguodala$period*12-iguodala$minutes_remaining
thompson$minute <- thompson$period*12-thompson$minutes_remaining

#Use sink() to send the summary() output of each imported data frame into individuals text file
#stephen-curry
sink("./output/stephen-curry-summary.txt")
summary(curry)
sink()
#Kevin Durant
sink("./output/kevin-durant-summary.txt")
summary(durant)
sink()
#Graymond Green
sink("./output/graymond-green-summary.txt")
summary(green)
sink()
#Andre Iguodala
sink("./output/andre-iguodala-summary.txt")
summary(iguodala)
sink()
#Klay Thompson
sink("./output/klay-thompson-summary.txt")
summary(thompson)
sink()


#Use the row binding function rbind() to stack the tables into one single data frame
gsw <- rbind(curry,durant,green,iguodala,thompson)


#Export (i.e. write) the assembled table as a CSV file shots-data.csv inside the folder data/. Use a relative path for this operation.
write.csv(gsw,file="./data/shots-data.csv")


