##
##fucntion 1 to make a vector
##
makeVector <- function(x = numeric()) {
  lamitjana <- NULL
  set <- function(y) {
    x <<- y
    lamitjana <<- NULL
  }
  get <- function() x
  setmean <- function(mean) lamitjana <<- mean
  getmean <- function() lamitjana
  list(set = set, get = get,
       setmean = setmean,
       getmean = getmean)
}
##
##function 2 to make the mean, look at caché first
##
cachemean <- function(x, ...) {
  lamitjana <- x$getmean()
  if(!is.null(lamitjana)) {
    message("if there's a cache, get there")
    return(lamitjana)
  }
  data <- x$get()
  lamitjana <- mean(data, ...)
  x$setmean(lamitjana)
  lamitjana
}
##
##function 3 to make a matrix, like make a vector
##
makeCacheMatrix <- function(x = matrix()) {
  lainversa <- NULL
  set <- function(y) {
    x <<- y
    lainversa <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) lainversa <<- inverse
  getInverse <- function() lainversa
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}
##
##function 4 to compute the inverse of a square matrix by means of SOLVE function
##
cacheSolve <- function(x, ...) {
  lainversa <- x$getInverse()
  if (!is.null(lainversa)) {
    message("obtenint la matriu caché")
      return(lainversa)
    }
    lamatriu <- x$get()
    lainversa <- solve(lamatriu, ...)
    x$setInverse(lainversa)
    lainversa
  }
##
## prova
prova <- makecachematrix(matrix(c(10,4,2,20,5,3,10,6,5), 3,3))
prova$get
#
#      [,1] [,2] [,3]
# [1,]   10   20   10
# [2,]    4    5    6
# [3,]    2    3    5    
#
cacheSolve(prova)
#             [,1]       [,2]       [,3]
# [1,] -0.10000000  1.0000000 -1.0000000
# [2,]  0.11428571 -0.4285714  0.2857143
# [3,] -0.02857143 -0.1428571  0.4285714
## to get the caché inverse matrix
cacheSolve(prova)
# obtenint la matriu caché
#             [,1]       [,2]       [,3]
# [1,] -0.10000000  1.0000000 -1.0000000
# [2,]  0.11428571 -0.4285714  0.2857143
# [3,] -0.02857143 -0.1428571  0.4285714
