## _makeCacheMatrix creates a variable inv to store the inverse of incoming matrix in it
## _cacheSolve checks if inverse calculated and just returns it; otherwise it calculates the inverse and transfer for storing 

## First function includes 3 inside: transferring matrix for calculations and two to store|return the inverse 

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  if (det(x) == 0) message("Warning! D = 0. The matrix is singular!") 
  get <- function() x                                         ## to transfer incoming matrix
  setinverse <- function(inverse) inv <<- inverse             ## NB! to store calculated inverse
  getinverse <- function() inv                                ## to return stored inverse 
  list(get=get, setinverse=setinverse, getinverse=getinverse) ## return as a whole function result 
}


## Check if inverse calculated | solve if not and store

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()             ## checking the inverse in cache 
  if(!is.null(inv)) {               ## return if found & skip
    message("getting cached data.")
    return(inv)
  }
  data <- x$get()                   ## getting matrix from incoming function
  if (det(data) == 0) message("Can't calculate singular matrix with D=0")
  else                              ## inverting if possible
    {inv <- solve(data)
    x$setinverse(inv)               ## stroring by means of incoming function 
    inv}                            ## return inverse 
}
