## This file contains two functions to help create a matrix object and cache its
## inverse in memory. This would save the computation time as inverse is usually uses
## heavy computation power and long time

## maeCacheMatrix creates a matrix object and initializes the inverse to NULL
## There is a list of four methods used to get & set the value, get & set the inverse

makeCacheMatrix <- function(x = matrix()) { # Create the new matrix object with NULL
  
  i <- NULL                                 # Set inverse to NULL
  
  set <- function(y) {                      # Define "Set" function
    x <<- y                     # Set the matrix value to what is passed as parameter
    i <<- NULL                  # Set the inverse of the matrix to NULL
  }
  
  get <- function() x           # "Get" function simply returns the matrix object x
  
  setinverse <- function(inverse) i <<- inverse
  
  # "setInverse" function sets the inverse to what is passed as parameter
  
  getinverse <- function() i    # "getinverse" function returns current value of inverse
  
  list(set = set, get = get,    # Creating a list helps to call the four methods defined
       setinverse = setinverse, # using every new matrix with a $ sign. These are
       getinverse = getinverse) # subsequently used in the cacheSolve function

}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been 
## calculated (and the matrix has not changed), then the 
## cachesolve should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {   # Function takes the matrix created through
                                   # makeCacheMatrix as an argument
  
  i <- x$getinverse()              # We first check the inverse is already set
                                   # previously by either setinverse or cacheSolve
  
  if(!is.null(i)) {                # if the inverse is NULL (not initialized yet)
    
    message("getting cached data") # print that we are getting cached data
    
    return(i)                      # Return the value already available without
  }                                # computing again
  
  
  ## The code below will only execute if the inverse variable is found to have NULL
                
  data <- x$get()                  # Get the data in the matrix object into a variable
  
  i <- solve(data, ...)            # Compute the inverse of the matrix object
  
  x$setinverse(i)                  # set the inverse value back into the object(cached)
  
  i                                # return the cached inverse of the matrix
}
