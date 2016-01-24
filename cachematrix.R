## This script enables user to perform get/set operations on a given
## matrix and also allows the inverse of the matrix to be cached.
## As long as the matrix does'nt change, the inverse for the matrix would
## be calculated once and cached.

## This function makes a 'special' matrix that stores a matrix and its inverse

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


## This function calculates the matrix inverse and stores it in 'special' matrix.
## If this 'special' matrix already has a cached inverse that is sent back
## without calculating it the second time.

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
