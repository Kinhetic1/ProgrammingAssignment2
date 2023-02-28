## Functions are created to cache a matrix and generate an 
## inverse of the matrix


## Firstly, the makeCacheMatrix is created to cache a matrix.
## The makeCacheMatrix has a default invertible matrix as an argument.
## Although the function can accept any other matrix as argument when called.

makeCacheMatrix <- function(x = matrix(c(-3,5,1,0),2,2)) {
  # Initializes the inverse matrix
  s <- NULL 
  
  # Defines the function that sets the value of the matrix
  # Note that y and NULL are being assigned to x and s variables in the environment 
  # respectively, in terms of lexical scoping.
  set <- function(y) { 
        x <<- y
        s <<- NULL
  }
  # Defines the function that gets the value of the matrix
  get <- function(){ 
    x 
  }
   
  # Defines the function that sets the value of the inverse
  # Note that solve function is assigned to the s variable in the environment, 
  # in terms of lexical scoping.
  setsolve <- function(solve){
    s <<- solve
  }
    
  # Defines the function that gets the value of the inverse
  getsolve <- function(){
    s
  }
   
  # This is the special "matrix" which is actually a list 
  # This is the object that the makeCacheMatrix() returns
  list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)
}


## Secondly, a function is created to find the inverse of the cached matrix 
## using the solve function
## Computes the inverse of the object returned by makeCacheMatrix() (in this case, 
## a matrix). If the inverse has already been calculated, and the object has not 
## changed, it retrieves the inverse from the cache. The initial computation and 
## cache retrieval can be easily modified.

## The argument for this function is the list to be returned by makeCacheMatrix()
cacheSolve <- function(x, ...) {
  # This assigns the getsolve function which is a member of the special "matrix" 
  # to the s variable
  s <- x$getsolve()
  
  # This checks if the inverse is already calculated. If yes, it returns it.
  if(!is.null(s)) {
    message("getting inversed matrix")
    return(s)
  }
  #The inverse gets calculated if the if statement above evaluates to false.
  
  # Gets the matrix
  data <- x$get()
  
  #Assigns the matrix to be inverted and the special "matrix" to solve and estimates
  # the inverse.
  s <- solve(data, ...)
  
  #Sets the estimated inverse
  x$setsolve(s)
  
  #Returns s, the inverse
  s
}


## Testing the code
## Estimating the inverse of a matrix
invmat <- cacheSolve(makeCacheMatrix())
print(invmat)

## Retrieving the original matrix
origmat <- cacheSolve(makeCacheMatrix(invmat))
print(origmat)
