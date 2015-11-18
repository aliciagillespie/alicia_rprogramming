## These functions make use of the caching abilities in R to more quickly return the inverse of a matrix. 

## The first function, makeCacheMatrix, creates a list that finds and defines the inverse of a matrix, x.

## Note that this function does not explicitly check to see that the matrix is invertible, 
## and it only works for matrices that are invertible.

## The second function, cacheSolve, checks the cache to see if the inverse has already been stored. If so,
## it returns the previously calculated inverse. If not, it calculates the inverse.


makeCacheMatrix <- function(x = matrix()) {
  mat_inverse <- NULL
  set <- function(y) {
    x <<- y
    mat_inverse <<- NULL
  }
  get <- function() x
  set_inverse <- function(solve) mat_inverse <<- solve
  get_inverse <- function() mat_inverse
  list(set = set, get = get,
       set_inverse = set_inverse,
       get_inverse = get_inverse)
}


cacheSolve <- function(x, ...) {
  mat_inverse <- x$get_inverse()
  if(!is.null(mat_inverse)) {
    message("getting cached data")
    return(mat_inverse)
  }
  data <- x$get()
  mat_inverse <- solve(data, ...)
  x$set_inverse(mat_inverse)
  mat_inverse
}