# Solution for inverse matrix using caching
# The result is in the makeCashMatrix
#############

###################################
# The makeCashMartix function creates an environment
# The inverse matrix is placed inside the object
# The result of a function is a list of 5 named elements that are defined
# five functions
###################################

makeCacheMatrix <- function(x = matrix()) {
# Example: Insert a matrix, for example, x <-matrix (rnorm (64), 8,8)
## To check the cached values:
# xMat <-makeCacheMatrix (x) # Run the function
# parent.env (xMat $ getenv ()) $ m # Check the caching value
# environment (xMat $ getmean) # refers to the environment "m"
m<-NULL  # assigns a NULL variable to the current environment
evn <- environment()  # Save Environment
y<-NULL 

setmatrix<-function(y){  # Set matrix value
	x<<-y  # cache matrix-assigns a value of y from the parent environment
	m<<-NULL # search through the parent environment for the existing variable definition and set to NULL
	}
  
getmatrix<-function() x  # Get the matrix value cached with setmatrix
setinverse<-function(solve) m<<- solve # The stored value of the inverse matrix is stored in m
getinverse<-function() m  # Get the stored value of the inverse matrix m that was saved with setinverse
getenv<- function() environment()

list (setmatrix=setmatrix, getmatrix = getmatrix, # creates a list for placing four functions
setinverse = setinverse,
getinverse = getinverse,
getenv = getenv)

}

###################################
## The "cacheSolve" function returns an inverse matrix that
# returned by the makeCacheMatrix function, for example. xMat $ getmatrix ()
###################################

cacheSolve <- function(xMat= m(), ...) {
## Return the matrix inverse to 'x'
# Startup function, for example. for example: minv <-cacheSolve (xMat = m)
# Compare the matrix to what was before!
	m <- xMat$getinverse() # If the inversion has already been calculated, it gets it
	if(!is.null(m)){# Check if cacheSolve was running before
		if(xMat$setmatrix() == xMat$getmatrix()) { # check that the matrix has not changed, and if not, it sends a text message and returns the cached matrix
    	message("getting cached data")
    	matrix<-xMat$get()
    	m<-solve(matrix, ...)
    	xMat$setmatrix(m)
    	return(m) 
    	}    
    	y <- xMat$getmatrix() # run the getmatrix function to get the value of the input matrix
    	xMat$setmatrix(y)# run the setmatrix function on the input matrix for its caching
    	m <- solve(y, ...) # calculate the value of the inverse input matrix

    	xMat$setinverse(m) # run the setinverse function in reverse reverse caching
    	m 
    	}
    	
}
