##makeCacheMatrix and cacheSolve together allow the inverse of a matrix, once
##calculated, to be retained in a cache to avoid recalculating
##the next time the matrix is called.

##This function stores four functions which permit a cache to be  
##created with the saved value of an inverted matrix. Set establishes
##the initial value of the original matrix and resets the inverted 
##matrix to emptly (NULL); get calls p the original matrix supplied
##as an argument; setinv stores the value of the inverted matrix, m, 
##for later recall, and getinv returns the value of m. The function
##returns these four sub-functions as a list.

makeCacheMatrix <- function(x=matrix()){ ##Takes a matrix as an argument
  m <- NULL           ##Creates blank variable m to receive inverted matrix
  set <- function(y){ ##Establishes value of original and inverted matrix
    x <<- y           ##Source matrix is set to argument passed in function 
    m <<- NULL        ##Cached matrix set/reset to empty
  }
  get <- function() x ##Gets function argument (the original matrix)
  setinv <-function(solve) m <<- solve   ##Stores value of inverse matrix
  getinv <- function() m                 ##Returns value of inverse matrix  
  list(set = set, get = get,             ##Stores functions to be called
       setinv = setinv, getinv = getinv) ##later/in cacheSolve.
}

##cacheSolve takes as an argument a variable assigned to makeCacheMatrix.
##When called for the first time the value of the inverse matrix will be
##empty. It will calculate the inverse of the matrix and store that value in
##a cache for future use. For subsequent iterations, the stored value of the
##inverse matrix will remain available and it can simply be returned as-is.

cacheSolve <- function(x, ...){##Takes as argument variable storing above cache
  m <- x$getinv                ##m is assigned the value set in MakeCacheMatrix
  if(!is.null(m)) {            ##If inverse matrix is calculated, it will be in cache
    message("Returning data from cache.")##and can be returned as-is.
    return(m)                            ##Returns a matrix that is the inverse of 'x'
  }                                      ##at the same time as it ends the function.
  data <- x$get()       ##Otherwise, the original matrix is called up,
  m <- solve(data, ...) ##the inverse matrix calculated,  
  x$setinv(m)           ##and the value stored for future use.  
  m                     ##Returns a matrix that is the inverse of 'x'  
}