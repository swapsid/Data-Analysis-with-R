set.seed(100)
N=20
CX101 <- rnorm(N,45,8)
CX102 <- rnorm(N,65,8)
CX103 <- rnorm(N,85,10)
CX104 <- rnorm(N,45,10)
CX105 <- rnorm(N,70,1)

#Creating the required matrix
subjects <- c(CX101, CX102, CX103, CX104, CX105)
res <- matrix(round(subjects, digits = 2), nrow = 20, ncol = 5)

cnames <- c("CX101", "CX102", "CX103", "CX104", "CX105")
rnames <- c("Student_1", "Student_2", "Student_3", "Student_4", "Student_5", "Student_6", "Student_7", "Student_8", "Student_9","Student_10",
            "Student_11","Student_12","Student_13","Student_14", "Student_15","Student_16","Student_17","Student_18","Student_19","Student_20")
            
rownames(res) <- rnames
colnames(res) <- cnames

#Converting values outside of [0,100] to NA
res1 <- apply(res, 2,  function(x) ifelse(x>100 | x<0, NA, x))

#Relacing the missing values with the mean of the valid values in that column
res2 <- apply(res1, 2, function(x) ifelse(is.na(x), round(mean(x, na.rm=TRUE), digits = 2),x))

#Generating aggregate values and an overall rank
Min <- round(apply(res2, 1, min), digits = 2)
SD <- round(apply(res2, 1, sd), digits = 2)
Mean <- round(apply(res2, 1, mean), digits = 2)
Rank <- rank(Mean)
final<- cbind(res2, Min, SD, Mean, Rank)

#Producing student with the highest average using which()
index <- which(final[, "Mean"] == max(final[, "Mean"]))
highest_avg <- (final[index, , drop = FALSE])

#Summary statistics for each subject by adding new rows at the end of the cleaned data set          
Subject_Mean <- round(apply(res2, 2, mean), digits = 2)
Subject_Max <- round(apply(res2, 2, max), digits = 2)
Subject_Min <-round(apply(res2, 2, min), digits = 2)
subj_summ <- rbind(res2, Subject_Mean, Subject_Max, Subject_Min)


