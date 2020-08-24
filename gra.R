sprintf('MCDM with GRA')

library(readxl)

entropy <- function(X) {
  sumX <- as.matrix(apply(X, 2, sum))
  
  P <- apply(X, 1, function(x) x / sumX)

  CE <- apply(P, 1, function(x) ((-1) / log(nrow(P))) * x * log(x))
  
  E <- apply(CE, 2, sum)
  
  D <- abs(1 - E)
  sumD <- sum(D)

  W <- sapply(D, function(x) x / sumD)

  return(W)
}

# d -> Distinguishing Coefficient Value, range -> (0, 1), default -> 0.5
gra <- function(data, refs, weight = entropy(data), d = 0.5) {
  ##### Preparing the Decision Matrix
  # X -> Data Matrix
  X <- data
  rnX <- rownames(X)
  mxX <- as.matrix(apply(X, 2, max)) # columns max value
  mnX <- as.matrix(apply(X, 2, min)) # columns max value

  # R -> Reference Vector
  R <- as.matrix(refs)

  # RS -> Reference Series Vector
  RS <- as.vector(sapply(X[, 1:ncol(X)], function(x) ifelse(R == 'B', mxX, mnX)))
  names(RS) <- colnames(X)

  ##### Adding Up Reference Series
  X <- rbind(RS, X)

  ##### Preparing Comparison Series and Normalizing Data
  # NX -> Normalized Data Matrix
  # NX <- apply(X, 2, function(x) ifelse(R == 'B',(x - mnX) / (mxX - mnX), (mxX - x) / (mxX - mnX)))
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

  barplot(as.vector(D), main = "Grey Relational Degree", xlab = "Alternatives", ylab = "Degrees", names.arg = rownames(D), border = "darkgrey", density = c(nrow(D):1))
}

##### SampleData(Optimum Lastik Seçimi)
d <- read_excel('/home/thiasus/Desktop/Workspace/MCDM-GRA/SampleData/optimum-lastik-secimi.xlsx', sheet = 'data')
r <- read_excel('/home/thiasus/Desktop/Workspace/MCDM-GRA/SampleData/optimum-lastik-secimi.xlsx', sheet = 'refs')
w <- read_excel('/home/thiasus/Desktop/Workspace/MCDM-GRA/SampleData/optimum-lastik-secimi.xlsx', sheet = 'weight')
D <- 0.25

gra(data = d, refs = r, weight = w, d = D)
