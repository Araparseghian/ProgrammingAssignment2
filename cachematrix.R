## It can be helpful to write an R function that is able to cache potentially time-consuming computations. 
## For example, taking the mean of a numeric vector is typically a fast operation. 
## However, for a very long vector, it may take too long to compute the mean, especially if it has to be computed repeatedly (e.g. in a loop). 
## If the contents of a vector are not changing, it may make sense to cache the value of the mean so that when we need it again, it can be looked up in the cache rather than recomputed. 
## Scoping rules of the R language can be manipulated to preserve state inside of an R object.

## The first function, makeCacheMatrix creates a special "matrix", which is really a list containing a function to

## set the value of the matrix
## get the value of the matrix
## set the value of the inverse of the matrix
## get the value of the inverse of the matrix

# function 'makeCacheMatrix' will receive the argument 'x', a matrix
makeCacheMatrix <- function(x = matrix()) {
	
	# 'inv' takes the value of 'NULL' in the parent environment
	inv <- NULL 

		# function 'set' will receive argument any argument 'y'
		set <- function(y) {
		# input matrix 'y' will be assigned to 'x', 'x' will exist in the parent environment.											
		x <<- y 
		# 'NULL' will be assigned to 'inv', a reset 'inv' will exist in the parent environment
		inv <<- NULL   
		}

	# the matrix will be implied through any argument 'y' to 'x'
	get <- function() x   
	# 'inverse' will be assigned to (stored in) 'inv', 'inv' will exist in the parent environment.
	setinverse <- function(inverse) inv <<- inverse 
	# the inverse of the matrix - assigned to (stored in) 'inv' - will be implied 
	getinverse <- function() inv
	# constructs a list, using form tag=value, assigning makeCacheMatrix to an object 
	list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)

}

## The following function calculates the inverse of the special "matrix" created with the above function. 
## However, it first checks to see if the inverse has already been calculated. 
## If so, it gets the inverse from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the data and sets the value of the inverse in the cache via the setinverse function.

# function cacheSolve will receive argument 'x'
cacheSolve <- function(x, ...) { 

	# the 'getinverse' value of the makeCacheMatrix object will be assigned to 'inv' 
	inv <- x$getinverse() 

		# if the inverse has in fact been previously calculated the inverse will be returned
		if(!is.null(inv)) { 
			message("getting cached data.")
			return(inv)
		}

	# the 'get' value of the makeCacheMatrix object is assigned to mat
	mat <- x$get() 
	# solve calculates the inverse of mat and this value is assigned to 'inv'
	inv <- solve(mat, ...) 
	# the 'inv' value is stored in 'setinverse', previously created in makeCacheVector function
	x$setinverse(inv) 
	# the inverse matrix is printed
	inv 

}
