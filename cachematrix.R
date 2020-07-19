## makeCacheMatrix function 1: This function creates a special "matrix" object that can cache its inverse. 

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve   ## the solve is the name for inversion function used in R
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}

##cachesolve function2: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above and 
## Return a matrix that is the inverse of 'x'

cachesolve <- function(x, ...) {
  m <- x$getsolve()                   ##the solve is the inverse code used in R
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}

## an example showing that the two functions work

cachesolve(makeCacheMatrix(x))
## [,1] [,2]
## [1,]    2   -5
## [2,]   -1    3
## > m
## [1] 256
## > solve(y)
## [,1] [,2]
## [1,]    1    0
## [2,]    0    1
## > solve(x)
## [,1] [,2]
## [1,]    2   -5
## [2,]   -1    3
## > x
## [,1] [,2]
## [1,]    3    5
## [2,]    1    2
## > test <- matrix(c(2, -1, -5, 3), nrow = 2)
## > x %*% test
## [,1] [,2]
## [1,]    1    0
## [2,]    0    1
## > test %>% x
## > test %*% x
## [,1] [,2]
## [1,]    1    0
## [2,]    0    1

