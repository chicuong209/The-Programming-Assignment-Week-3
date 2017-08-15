makeCacheMatrix (x = matrix()){
  matrix_inverse <- NULL
  set <- function(y){               ## Set the value of the matrix
    x <<- y
    matrix_inverse<<- NULL
  }
  get <- function() x               ## Get the value of the matrix
  setinverse <- function(inverse) matrix_inverse <- inverse  ## set the value of the inverse matrix
  getinverse <- function() matrix_inverse                    ## Get the value of the matrix inverse
  list(set = set, get = get, 
       setinverse = setinverse,
       getinverse = getinverse)
}
cacheinverse <- function(x,...){
  matrix_inverse <- x$getinverse()        ## Get the value of the matrix inverse by x$getinverse after assign this value to variable matrix_inverse 
  if (!is.null(matrix_inverse)){          ## If the matrix_inverse has already been calculated, so it returns the matrix inverse from the cache and skips the computation.   
    message("Getting cached data")
    return(matrix_inverse)
  }
  data <- x$get()                         ## If the matrix_inverse hasn't already been calculated, so it calculates the matrix_inverse of the data 
  matrix_inverse <- solve(data,...)       ## by function solve()
  x$setinverse(matrix_inverse)            ## After that it sets the value of the matrix inverse in the cache via the setinverse function
  matrix_inverse                          ## Finally, it returns the value of matrix inverse
}
