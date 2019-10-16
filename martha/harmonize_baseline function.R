

harmonize_baseline <- function(data){
  
  data$Mean=round(as.numeric(data$Mean))
  baseline <- rep(NA, length(data$Mean))
  
  #let's first extract HAMD21
  HAMD21_values = data[data$Scale == "HAMD21",]$Mean 
  HAMD21_values[HAMD21_values == 17] = 16
  HAMD21_values[HAMD21_values == 18] = 17
  HAMD21_values[HAMD21_values == 19] = 17
  HAMD21_values[HAMD21_values == 20] = 18
  HAMD21_values[HAMD21_values == 21] = 19
  HAMD21_values[HAMD21_values == 22] = 19
  HAMD21_values[HAMD21_values == 23] = 20
  HAMD21_values[HAMD21_values == 24] = 21
  HAMD21_values[HAMD21_values == 25] = 22
  HAMD21_values[HAMD21_values == 26] = 23
  HAMD21_values[HAMD21_values == 27] = 24
  HAMD21_values[HAMD21_values == 28] = 25
  HAMD21_values[HAMD21_values == 29] = 26.5
  HAMD21_values[HAMD21_values == 30] = 26.5
  HAMD21_values[HAMD21_values == 31] = 26.5
  HAMD21_values[HAMD21_values == 32] = 28
  HAMD21_values[HAMD21_values == 33] = 29
  HAMD21_values[HAMD21_values == 34] = 29
  HAMD21_values[HAMD21_values == 35] = 30.5
  
  # Then extract HAMD24
  HAMD24_values = data[data$Scale == "HAMD24",]$Mean 
  HAMD24_values[HAMD24_values == 24] = 18
  HAMD24_values[HAMD24_values == 25] = 19
  HAMD24_values[HAMD24_values == 26] = 19
  HAMD24_values[HAMD24_values == 27] = 20
  HAMD24_values[HAMD24_values == 28] = 20
  HAMD24_values[HAMD24_values == 29] = 21
  HAMD24_values[HAMD24_values == 30] = 22
  HAMD24_values[HAMD24_values == 31] = 23
  HAMD24_values[HAMD24_values == 32] = 23
  HAMD24_values[HAMD24_values == 33] = 24
  HAMD24_values[HAMD24_values == 34] = 25
  HAMD24_values[HAMD24_values == 35] = 26.5
  HAMD24_values[HAMD24_values == 36] = 26.5
  HAMD24_values[HAMD24_values == 37] = 26.5
  HAMD24_values[HAMD24_values == 38] = 26.5
  
  # Then extract MADRS
  MADRS_values = data[data$Scale == "MADRS",]$Mean 
  MADRS_values[MADRS_values == 24] = 19
  MADRS_values[MADRS_values == 25] = 19
  MADRS_values[MADRS_values == 26] = 20
  MADRS_values[MADRS_values == 27] = 22
  MADRS_values[MADRS_values == 28] = 22
  MADRS_values[MADRS_values == 29] = 23
  MADRS_values[MADRS_values == 30] = 23
  MADRS_values[MADRS_values == 31] = 24
  MADRS_values[MADRS_values == 32] = 25
  MADRS_values[MADRS_values == 33] = 25
  MADRS_values[MADRS_values == 34] = 26
  MADRS_values[MADRS_values == 35] = 27
  MADRS_values[MADRS_values == 36] = 28
  MADRS_values[MADRS_values == 37] = 29
  MADRS_values[MADRS_values == 38] = 29
  
  baseline[data$Scale == "HAMD17"] = data[data$Scale=="HAMD17",]$Mean
  baseline[data$Scale == "HAMD21"] = HAMD21_values
  baseline[data$Scale == "HAMD24"] = HAMD24_values
  baseline[data$Scale == "MADRS"] = MADRS_values
  data$baseline <- baseline  
  
  # What about other ones?

  return(data)
}



