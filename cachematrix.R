## This function creates a special "matrix" object that can cache its 
## inverse

makeCacheMatrix <- function(x = matrix()) {
inv<-NULL
	setmatrix<-function(y){
		x<<-y
		inv<<-NULL
	}
	getmatrix<-function()x
	setinverse<-function(inverse) inv<<-inverse
	getinverse<-function()inv
	list(setmatrix=setmatrix, getmatrix=getmatrix, setinverse=setinverse, getinverse=getinverse)
}


## This function computes inverse of matrix returned by above
##function. If inverse already exits, then it retrieves inverse 
## from cache

cacheSolve <- function(x, ...) {
        inv<-x$getinverse()
	if(!is.null(inv)){
		message("getting cached data")
		return(inv)
	}
	data<-x$getmatrix()
	inv<-solve(data,...)
	x$setinverse(inv)
	inv
}
