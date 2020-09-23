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
#
