## Functions are created to cache a matrix and generate an 
## inverse of the matrix


## Firstly, the makeCacheMatrix is created to cache a matrix.

makeCacheMatrix <- function(x = matrix(c(-3,5,1,0),2,2)) {
  s <- NULL
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) s <<- solve
  getsolve <- function() s
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## Secondly, a function is created to find the inverse of the cached matrix 
## using the solve function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  s <- x$getsolve()
  if(!is.null(s)) {
    message("getting inversed matrix")
    return(s)
  }
  data <- x$get()
  s <- solve(data, ...)
  x$setsolve(s)
  s
}

## Estimating the inverse of a matrix
invmat <- cacheSolve(makeCacheMatrix())
invmat

## Retrieving the original matrix
origmat <- cacheSolve(makeCacheMatrix(invmat))
origmat
