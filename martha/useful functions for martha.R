split_data <- function(martha, outcome){
  
  
  # only keep necessary characteristic variables we need for now
  characteristics <- which(martha[1,] %in% c("StudyID", "Drug", "No_randomised", "Age_mean", "Scale", "Mean", "Dose_intended_min","Dose_range" ,"%_Female", "Dosing_schedule"))
  martha_characteristics <- martha[, characteristics]
  colnames(martha_characteristics) <- martha[1, characteristics]
  
  # extract outcome; only keep trials that are not all '*'; also remove the first 6 row headers
  martha_outcome <- as.matrix(martha[,martha[1,] %in% outcome])
  ind <- apply(martha_outcome, 1, function(x) all(x == "*"))
  ind[1:6] <- TRUE
  
  martha_outcome <- as.matrix(martha_outcome[!ind, ])
  martha_characteristics <- martha_characteristics[!ind,]
  
  # change No_randomised to numeric
  martha_characteristics["No_randomised"][martha_characteristics["No_randomised"] == "*"] <- 0   #some values are missing - denoted with *
  martha_characteristics["No_randomised"] <- sapply(martha_characteristics[,"No_randomised"], as.numeric)
  
  # change the * to 0 and add the same columns up; change the values to numeric
  martha_outcome[martha_outcome == "*"] <- 0
  martha_outcome <- apply(martha_outcome, 2, function(x){x[grepl('S ', x)] <- NA; x}) #delete some weird values (i.e. "S 0" for Suicidal Ideation
  martha_outcome <- apply(martha_outcome, 2, as.numeric)
  martha_outcome <- apply(martha_outcome, 1, sum, na.rm = TRUE)
  
  # combine the characteristics and outcome
  martha_combined <- cbind(martha_characteristics, martha_outcome)
  # print(sum(martha_combined$martha_outcome, na.rm = TRUE)) #check this number with what we have calculated previously
  
  return(martha_combined)
}

# function to remove one arm trial; think the netmeta automatically gets rid of one arm
remove.onearm <- function(data, warn=TRUE) {
  # Remove 1-arm studies
  sel <- as.logical(sapply(data[,'StudyID'],
                           function(study) {sum(data[,'StudyID'] == study) > 1
                           }))
  
  if (warn && !all(sel)) {
    warning(paste("Removed", sum(!sel), "one-arm studies:",
                  paste(data[!sel,'StudyID'], collapse=", ")))
  }
  data[sel, , drop=FALSE]
}
