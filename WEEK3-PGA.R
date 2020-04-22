##Example: Caching the Mean of a Vector
##Caching the Mean of a Vector

makeVector <- function(x = numeric()) {
  ## Initialize the inverse property
  m <- NULL
  ##set the value of the vector
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  ##get the value of the vector
  get <- function() x
  ##set the value of the mean
  setmean <- function(mean) 
    m <<- mean
  ##get the value of the mean
  getmean <- function() m
  list(set = set, get = get,
       setmean = setmean,
       getmean = getmean)
}

##The following function calculates the mean of the special "vector" 
##created with the above function.

cachemean <- function(x, ...) {
  ##it first checks to see if the mean has already been calculated
  m <- x$getmean()
  ##If so, it gets the mean from the cache and skips the computation
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  ##calculates the mean of the data
  m <- mean(data, ...)
  ##sets the value of the mean in the cache via the setmean function
  x$setmean(m)
  ##return m
  m
}

##ASSIGNMENT: Caching the Inverse of a Matrix
##write a pair of functions that cache the inverse of a matrix

makeCacheMatrix <- function(m = matrix()) {
  ## Initialize the inverse property
  i <- NULL
  ##set the value of the matrix
  set <- function(y) {
    m <<- y
    i <<- NULL
  }
  ##get the value of the matrix
  get <- function() m
  ##set the value of the inverse
  setinverse <- function(inverse) 
    i <<- inverse
  ##get the value of the inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


cacheSolve <- function(x, ...) {
  ##return m calculated in makeCacheMatrix where x is the inverse
  m <- x$getinverse()
  ##If the inverse has already been calculated
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  ## Get the matrix of x
  data <- x$get()
  ##calculate the inverse of x
  m <- solve(data) %*% data
  ##set the value of the inverse via the setinverse function
  x$setinverse(m)
  ##return m
  m
}

