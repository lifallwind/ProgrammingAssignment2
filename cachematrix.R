## Put comments here that give an overall description of what your
## functions do

##Solve and Cache the solve of matrix

## Write a short comment describing this function
# To create a special object that stores a matrix 
# and get the value of solve.


makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsol <- function(solve) m <<- solve
  getsol <- function() m
  list(set = set, get = get,
       setsol = setsol,
       getsol = getsol)
}


## Write a short comment describing this function

# ANS. This function look up wheather there is a 
# answer had been calculated.
# If the matrix had been solved, 
# the function will print the answer from cache.
cacheSolve <- function(x, ...) {
  m <- x$getsol()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsol(m)
  m
}

## Test data
x <- rbind(c(1, -1/2), c(-1/2, 1)) 
matrix <- makeCacheMatrix(x)
cacheSolve(matrix)
cacheSolve(matrix)
