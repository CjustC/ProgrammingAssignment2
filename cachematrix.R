## The purpose of these two functions are to store the matrix and the
## inverse of a matrix for repeated use without having to 
## recalculate the matrix inversion multiple times.

## The 'makeCacheMatrix' function creates a special "matrix" 
## containg four nested functions: 'set', 'get', 'setInverse' & 'getInverse'. 
##   - 'set' is used to assign a matrix to the variable 
##   - 'get' is used to recall the existing matrix 
##   - 'setInverse' is used to assign an inverted matrix to the variable
##   - 'getInverse' is used to recall the stored inverted matrix.

## This function returns a list of values from the nested funcitons 
## that will be used by 'cacheSolve' to get or set the inverted matrix in cache.

makeCacheMatrix <- function(storeMatrix = matrix()) {

     invMatrix <- NULL ## initialize the variable to NULL. This variable will store the cached value.

     set <- function(inputMatrix) { ## initialize the 'set' function, so a matrix can be stored
          
          storeMatrix <<- inputMatrix ## assign the input matrix storage

          invMatrix <<- NULL ## remove any record of previous inverted matrix
     }

     get <- function() storeMatrix ## initialize the 'get' function, so the stored matrix can be recalled
     
     setInverse <- function(inverse) invMatrix <<- inverse  ## initialize the 'setInverse' function, so the matrix inversion can be stored
     
     getInverse <- function() invMatrix ## initialize the 'getinv' function, so the matrix inversion can be recalled
     
     list(set = set, ## specify the stored commands
          get = get,
          setInverse= setInverse,
          getInverse = getInverse)
}

## This function examines a cached matrix and runs the 'solve' command on it. 
## This function requires the matrix to be stored in the 'makeCacheMatrix' function prior to use.

## This function 1st checks to see if the cached matrix is square. If the matrix is not square, 
## an Error message, "Cannot invert matrix, matrix must be square to invert." is displayed.
## If the matrix is square the function will check to see if the inverse of the matrix is
## stored in 'inputMatrix$getInvers' (inverted matrix stored in cache).
## If the inverted matrix does not exist then the 'cacheSolve' function
## retrieves the inverse from 'inputMatrix$get' (matrix stored in cache before inversion).
## If the function cannot retrieve a cached value then the funciton will use the results from 
## the 'makeCacheMatrix' and compute the inverse using the 'solve()' function.
## The function will return the inverse of the matrix.

cacheSolve <- function(inputMatrix, ...) {
     
     if(nrow(inputMatrix$get()) == ncol(inputMatrix$get())) { ## check to make sure that the cached matrix is square (only square matrices can be inverted)

          currInverse <- inputMatrix$getInverse() ## check if there is a cached inverted matrix
          
          if(!is.null(currInverse)) { # if the inverted matrix is stored in cache, send a message "getting cached data"
               message("getting cached data")
               return(currInverse)
          } else {
               data <- inputMatrix$get() ## get the cached matrix (the cached Inverted matrix was not found)
               currentInverse <- solve(data, ...) ## invert the cached matrix using the solve() function
               inputMatrix$setInverse(currentInverse) ## set inverted matrix in the cached matrix
               currInverse ## display the inverted matrix
          }
     }else { ## If the cached matrix is not square, return an error
          message("ERROR: Cannot invert matrix, matrix must be square to invert.")
     }
}
