########## MCDM with GRA ##########

# install.packages(readxl)
library(readxl)

entropy <- function(X) {
  sumX <- as.matrix(apply(X, 2, sum))

  P <- apply(X, 1, function(x) x / sumX)

  CE <- apply(P, 1, function(x)((-1) / log(nrow(P))) * x * log(x))
  CE[is.na(CE)] <- 0 # **assign 0 for NA (-inf, inf) values**

  E <- apply(CE, 2, sum)

  D <- abs(1 - E)
  sumD <- sum(D)

  W <- sapply(D, function(x) x / sumD)

  return(W)
}

# d -> Distinguishing Coefficient Value, range -> (0, 1), default -> 0.5
gra <- function(data, refs, weight = vector(), d = 0.5) {
  ##### Preparing the Decision Matrix
  # X -> Data Matrix
  X <- data

  # rnX -> For plotting
  rnX <- rownames(X)

  ##### Assign mean of columns for NA values
  X <- as.matrix(sapply(X, function(x) ifelse(is.na(x), mean(x, na.rm = TRUE), x)))
  
  # columns max value
  mxX <- apply(X, 2, max)
  # columns min value
  mnX <- apply(X, 2, min)

  ##### Check or Calculate Weight
  if (all(is.na(weight))) {
    weight <- entropy(X)
  } else if (any(is.na(weight))) {
    print('Weight Vector\'s has NA\'s in it. Would you like to use entropy?')
    input <- readline(prompt = 'Y/N')
    if (input == 'Y' | input == 'y') {
      weight <- entropy(X)
    } else {
      stop('Process stopped by user.')
    }
  } else if (sum(weight) != 1) {
    print('Weight Vector\'s sum is not equal to 1. Would you like to use entropy?')
    input <- readline(prompt = 'Y/N')
    if (input == 'Y' | input == 'y') {
      weight <- entropy(X)
    } else {
      stop('Process stopped by user.')
    }
  }
  
  # R -> Reference Vector
  R <- as.matrix(refs)

  # RS -> Reference Series Vector
  RS <- as.vector(sapply(X[1,1], function(x) ifelse(R == 'B', mxX, mnX)))

  ##### Adding Up Reference Series
  X <- rbind(RS, X)

  ##### Preparing Comparison Series and Normalizing Data
  # NX -> Normalized Data Matrix
  # NX <- apply(X, 2, function(x) ifelse(R == 'B',(x - mnX) / (mxX - mnX), (mxX - x) / (mxX - mnX))) ** ? **
  NX <- X
  for (c in 1:ncol(X)) {
    for (r in 1:nrow(X)) {
      if (R[, c] == 'B') {
        NX[r, c] <- (X[r, c] - mnX[c]) / (mxX[c] - mnX[c])
      } else if (R[, c] == 'C') {
        NX[r, c] <- (mxX[c] - X[r, c]) / (mxX[c] - mnX[c])
      }
    }
  }

  ##### Calculating Distance and Preparing Absolute Value Tables
  # AV -> Absolute Value Matrix
  AV <- apply(NX[-1,], 2, function(x) abs(1 - x))
  mxAV <- as.matrix(apply(AV, 2, max)) # columns max value
  mnAV <- as.matrix(apply(AV, 2, min)) # columns max value

  ##### Calculating Grey Relational Coefficient Matrix
  # C -> Coefficient Matrix
  C <- apply(AV, 1, function(x)(mnAV + (d * mxAV)) / (x + (d * mxAV)))

  ##### Calculate the Grey Relational Degree
  # W -> Weight Vector
  W <- as.matrix(weight)

  WC <- apply(C, 2, function(x) W * x)

  # D -> Grey Relational Degree
  D <- as.matrix(apply(WC, 2, sum))

  ##### Ranking According to GRD and Plotting
  rownames(D) <- rnX
  colnames(D) <- c('GRD')
  D <- as.matrix(apply(D, 2, function(x) sort(x, decreasing = TRUE)))

  barplot(as.vector(D), main = "Grey Relational Degree", xlab = "Alternatives", ylab = "Degrees", names.arg = rownames(D), border = "darkgrey", density = 1)
}

##### SampleData(Optimum Lastik Seçimi)
# d <- read_excel('/home/thiasus/Desktop/Workspace/MCDM-GRA/SampleData/optimum-lastik-secimi.xlsx', sheet = 'data')
# r <- read_excel('/home/thiasus/Desktop/Workspace/MCDM-GRA/SampleData/optimum-lastik-secimi.xlsx', sheet = 'refs')
# w <- read_excel('/home/thiasus/Desktop/Workspace/MCDM-GRA/SampleData/optimum-lastik-secimi.xlsx', sheet = 'weight')
# gra(d, r, w)

##### SampleData(Optimum Lastik Seçimi - Ağırlıksız)
# d <- read_excel('/home/thiasus/Desktop/Workspace/MCDM-GRA/SampleData/optimum-lastik-secimi.xlsx', sheet = 'data')
# r <- read_excel('/home/thiasus/Desktop/Workspace/MCDM-GRA/SampleData/optimum-lastik-secimi.xlsx', sheet = 'refs')
# D <- 0.1
# gra(d, r, d = D)

##### SampleData(Economic Freedom of the World(2016))
# d <- read_excel('/home/thiasus/Desktop/Workspace/MCDM-GRA/SampleData/economic-freedom-of-the-world(2016).xlsx', sheet = 'data')
# r <- read_excel('/home/thiasus/Desktop/Workspace/MCDM-GRA/SampleData/economic-freedom-of-the-world(2016).xlsx', sheet = 'refs')
# gra(d, r)
