## This script enables user to per get/set operations on the a given matrix
## and also allows the inverse of the matrix to be cache for retrieval
## As long as the matrix is not changing, the inverse for the matrix would
## be calculated once

## This function makes a cache matrix that stores a matrix and its inverse

makeCacheMatrix <- function(x = matrix()) 
{
	inv <- NULL
	set <- function(y)
	{
		x <<- y
		inv <<- NULL
	}
	get <- function()
	{
		x
	}
	setinv <- function(data)
	{
		inv <<- data
	}
	getinv <- function()
	{
		inv
	}
	list(set=set,get=get,setinv=setinv,getinv=getinv)
}


## This function calculates the matrix inverse that stores it in matrix

cacheSolve <- function(x, ...)
{
        inv <- x$getinv()
        if(!is.null(inv))
        {
        	message("getting matrix inverse from cache")
        	return(inv)
        }
	mat <- x$get()
	inv <- solve(mat)
	x$setinv(solve(inv))
	inv
}
