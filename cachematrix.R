##Computes the inverse of a matrix, creates a cache if matrix already present. Message displayed if cached matrix is being retrieved

## Function makeCacheMatrix creates the matix
#Set function allows users to determine the matrix contents


makeCacheMatrix <- function(x = matrix()) {
	s<- NULL
	set <- function(y,nr){ #sets the items and num of rows in the matrixs
		x <<- matrix(c(y),nrow=nr)
		s<<- NULL
	}
	get<-function() x
	setinverse <- function(solve) s <<- solve
	getinverse <- function() s
	
	list( set = set, get=get, setinverse=setinverse, getinverse=getinverse)
	
}


## Function computes the inverse of the input matrix using Solve
## returns cache if the inverse of a matrix is already computed


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        s <- x$getinverse()
        if(!is.null(s)){
        	message("Getting the cached inverse of matrix")
        	return(s)
        }
        
        inverse<- x$get()
        s <- solve(inverse)
        x$setinverse(s)
        s
        
        
}
