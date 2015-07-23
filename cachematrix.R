## This function creates a matrix and solves its inverse matrix
## The resulting inverse matrix will be cached so that the 
## same calculation will not have to be repeated over and over

## makeCacheMatrix initilaizes the matrix 

makeCacheMatrix <- function(x = matrix()) {
  
    ##Initialize the inverse not to be solved
    inv <- NULL
    
    ##Set the matrix
    set <- function(y){
      x <<- y
      inv <<- NULL
    }
    
    ##Get the matrix
    get <- function() x
    
    ##Set the inverse by solve()
    setInv <- function(solve) inv <<- solve
    
    ##Get the inverse matrix
    getInv <- function() inv
    
    
    list(set = set, get = get, setInv = setInv, getInv = getInv)
}


## cacheSolve solves the inverse matrix
##If it has never been calculated, it uses the solve() function and stores this answer in a cache
##If it has been previously calculated, it returns the cached inverse matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
    ##Get the inverse from the makeCacheMatrix function
    inv <- x$getInv()  
    
    ##If it has already been cached (inverse is not null)
    if(!is.null(inv)){
        message("Getting cached matrix data")
        return(inv)
    }
    
    ##Has not been cached yet
    
    ##Get the matrix
    data <- x$get()
    ##Calculate the inverse of the matrix
    inv <- solve(data,...)
    ##Cache the inverse matrix
    x$setInv(inv)
    ##Show the inverse matrix
    inv
}
