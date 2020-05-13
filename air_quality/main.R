require(dplyr)

lombardia_stations <- 
  read.csv("lombardia_stations.csv") 

lombardia_2020 <- 
  read.csv("lombardia_20200425.csv")

# sum(!lombardia_2020$IdSensore %in% lombardia_stations$IdSensore)


  
