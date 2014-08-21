##In Summary both functions work together. makeCacheMatrix returns a list
## of functions that will be used by cacheSolve.


makeCacheMatrix <- function(x = matrix()) {
    ##This function store the matrix in a cache and provides the functions
    ## used by cacheSolve
    m <- NULL                   #Is the output variable (inverse matrix)
    set <- function(y) {        #It's used to assing new values to the matrix
        x <<- y
        m <<- NULL
    }
    get <- function() x        #Return the original matrix (not the inverse)
    setinv <- function(solve) m <<- solve #When called from cacheSolved, this function "superassing" the recived value to 'm'
    getinv <- function()       #Return the inverse matrix (not the original)
    list(set = set, get = get, #Return a list of the functions just created: set, get, getinv, setinv
         setinv = setinv,   
         getinv = getinv)
}


cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getinv()           #load the inverse matrix in 'm'. If no inverse has been calculates, m=NULL
    if(!is.null(m)) {         #Evaluate m, if it's not null, means that the inverse has already been calculated, 
        message("getting cached data") #then print the message and return m (the function cacheSolve ends after that).
        return(m)
    }
    data <- x$get()           #if m is null, load the original matrix in data
    m <- solve(data, ...)     #invoke the solve function and save the result in m
    x$setinv(m)               #then, it calls the setinv function in makeCacheMatrix in order to be able to use it when
    m	                      #cacheSolve or makeCacheMatrix are used again.
}