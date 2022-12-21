# data simulation
n <- 999

# categorical variables with different number of levels
x1 <- c(sample(1:5, size = n/3, replace = TRUE, prob = c(0.1, 0.1, 0.1, 0.1, 0.6)),
        sample(1:5, size = n/3, replace = TRUE, prob = c(0.1, 0.1, 0.6, 0.1, 0.1)),
        sample(1:5, size = n/3, replace = TRUE, prob = c(0.35, 0.2, 0.1, 0.3, 0.1)))
x1 <- factor(x1)
levels(x1) <- make.names(levels(x1))

x2 <- c(sample(1:4, size = n/3, replace = TRUE, prob = c(0.1, 0.1, 0.6, 0.1)),
        sample(1:4, size = n/3, replace = TRUE, prob = c(0.1, 0.6, 0.1, 0.1)),
        sample(1:4, size = n/3, replace = TRUE, prob = c(0.3, 0.1, 0.1, 0.3)))
x2 <- as.factor(x2)
levels(x2) <- make.names(levels(x2))

# independent numerical variables
x3 <- c(rnorm(n = n/3, mean = 5, sd = 2),
        rnorm(n = n/3, mean = 2, sd = 1),
        rnorm(n = n/3, mean = 7, sd = 1.5))

x4 <- c(rnorm(n = n/3, mean = 5, sd = 2),
        rnorm(n = n/3, mean = 2, sd = 1),
        rnorm(n = n/3, mean = 7, sd = 1.5))

# dependent numerical variables
mu1 <- c(3, 3, 3)
Sigma1 <- matrix(c(
  1,2,3,
  2,5,6,
  3,6,9), ncol = 3, nrow = 3)

mu2 <- c(2, 2, 2)
Sigma2 <- matrix(c(
  3,3,2,
  3,4,-1,
  2,-1,12), ncol = 3, nrow = 3)

mu3 <- c(10, 10, 10)
Sigma3 <- matrix(c(
  3,1,-1,
  1,3,1,
  -1,1,3), ncol = 3, nrow = 3)

x567 <- rbind(MASS::mvrnorm(n = n/3, mu = mu1, Sigma = Sigma1),
              MASS::mvrnorm(n = n/3, mu = mu2, Sigma = Sigma2),
              MASS::mvrnorm(n = n/3, mu = mu3, Sigma = Sigma3))

x <- data.frame(cat1 = x1,
                cat2 = x2,
                num_indep1 = x3,
                num_indep2 = x4,
                num_dep = x567[,1],
                num_dep = x567[,2],
                num_dep = x567[,3])
y <- as.factor(rep(c("C1","C2","C3"), each = n/3))

sd_thresh <- 1e-10
laplace <- 1
var_equal <- FALSE

p <- ncol(x)
n <- nrow(x)

if (!is.factor(y)) {
  y <- as.factor(y)
}

class_names <- levels(y)
k_class <- length(class_names)
n_classes <- sapply(class_names, function(m) sum(y == m))

f_vartypes_x <- function(x) {
  var_types <- c()
  for (i in 1:p) {
    var_types[i] <- class(x[,i])
    if (var_types[i] == "character") {
      x[,i] <- as.factor(x[[,i]])
      var_types[i] <- class(x[,i])
    }
  }
  i_factors <- which(var_types == "factor")
  i_numerics <- which(var_types != "factor")
  list(
    var_types = var_types,
    i_factors = i_factors,
    i_numerics = i_numerics,
    x = x
  )
}

m_vartypes_x <- f_vartypes_x(x = x)
i_factors <- m_vartypes_x$i_factors
i_numerics <- m_vartypes_x$i_numerics
x <- m_vartypes_x$x

x_numerics <- x[,i_numerics, drop = FALSE]
p_numerics <- ncol(x_numerics)
x_factors <- x[,i_factors, drop = FALSE]
p_factors <- ncol(x_factors)

x_classes_numerics <- lapply(class_names, function(m) x_numerics[y == m,])
x_classes_factors <- lapply(class_names, function(m) x_factors[y == m,])

# prior estimation
priors <- n_classes/n

# likelihood parameter estimations
## categorical variables

probs_classes <- list()
for (i in 1:k_class) {
  probs_classes[[i]] <- list()
  for (j in 1:p_factors) {
    probs_classes[[i]][[j]] <- table(x_classes_factors[[i]][,j])/n_classes[i]
  }
}
names(probs_classes) <- class_names

## number of levels for factors
k_factors <- sapply(1:p_factors, function(m) length(levels(x_factors[,m])))

## numerical variables
mu_classes <- lapply(x_classes_numerics, function(m) {
  apply(m, 2, mean)
})
names(mu_classes) <- class_names

sd_classes <- lapply(x_classes_numerics, function(m) {
  sd_temp <- apply(m, 2, sd)
  sd_temp[sd_temp == 0 | sd_temp < sd_thresh] <- sd_thresh
  return(sd_temp)
})
names(sd_classes) <- class_names

results <- list(
  class_names = class_names,
  k_class = k_class,
  priors = priors,
  probs_classes = probs_classes,
  k_factors = k_factors,
  mu_classes = mu_classes,
  sd_classes = sd_classes
)

