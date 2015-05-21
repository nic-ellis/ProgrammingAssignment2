## Calculating the inverse of a matrix can be comuptationally intensive
## Therefore this code calculates the inverse and cache's it to recall later

# makeCacheMatrix creates a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        
        #Clears the value in your variable m
        m <- NULL
        
        #Assigns the value of y to the variable x and clears the value of m
        setMatrix <- function(y) {
                x <<- y
                m <<- NULL}
        
        #getMatrix is a function that returns the matrix stored in makeCacheMatrix
        getMatrix <- function(){x}
        
        #Creates a variable called solve 
        setSolve <- function(solve){m <<- solve}
        
        #getSolve is a function that returns the inverse matrix stored in makeCacheMatrix
        getSolve <- function(){m}
        
        #sets the names in your list = to the functions defined above
        list(setMatrix = setMatrix, getMatrix = getMatrix, 
             setSolve = setSolve, getSolve = getSolve)  
}

# The following function returns the inverse of the matrix. 
cacheSolve <- function(x, ...) {
        
        #It first checks if the inverse has already been computed,
        #and if so, gets the result 
        m <- x$getSolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)}
        data <- x$getMatrix()
        
        # If not, it computes the inverse and resets the value in the cache
        m <- solve(data, ...)
        x$setSolve(m)
        m
}