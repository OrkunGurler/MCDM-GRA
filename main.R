sprintf('MCDM with GRA')

entropy <- function(){}

#
#
#
#
#
#
# 
#
# Cost(C) and Benefit(B)
gra <- function(dataPath, header = TRUE, sep = ",", dec = '.', fill = TRUE, na = FALSE, d = 0.5, weight = FALSE){
  ### Get data from csv
  data <- read.csv(dataPath, header = header, sep = sep, dec = dec, fill = fill)
  
  ### Splitting data into R, W and X vectors/matrices
  # R -> Reference Vector
  R <- data[1,]
  
  ### Preparing the Decision Matrix
  ## if weight is given in the data, else calculate with entropy method
  # W -> Weight Vector
  # X -> Data Matrix
  if(weight) {
    W <- data[2,]
    X <- data[3:nrow(data),]
  } else {
    W <- entropy()
    X <- data[-1,]
  }
  
  ### Adding Up Reference Series
  # RS -> Reference Series Vector
  RS <- apply()
  
  ### Preparing Comparison Series and Normalizing Data
  # Nx -> Normalized Data Matrix
  # Nrs -> Normalized Reference Series Vector
  Nx <- apply()
  Nrs <- apply()
  
  ### Calculating Distance and Preparing Absolute Value Tables with Nx and Nrs
  # AV -> Absolute Value Matrix
  AV <- apply()
  
  ### Calculating Grey Relational Coefficient Matrix with AV and d value
  # C -> Coefficient Matrix
  C <- apply()
  
  ### Calculate the Grey Relational Degree with C and W
  # D -> Grey Relational Degree
  D <- apply()
  
  ### Ranking According to GRD and Plotting
}

gra("lastik-data.csv", weight = TRUE)
