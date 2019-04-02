## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {

	## Initialize inverse
	inv <-NULL
	set <-function (y){
			x<<-y
			inv<<-NULL
	}
	## Calculate inverse and save it in a list
	get<-function() x
	setinv<- function (inverse) inv<<-inverse
	getinv<-function () inv
	list(set=set,get=get,setinv=setinv,getinv=getinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        ## Check if inverse already in cache
        if(!is.null(inv)){
        		message("getting cached data")
        		return(inv)
        }
        ## Get inverse and return
        data <-x$get()
        inv<-solve(data,...)
        x$setinv(inv)
        inv
}
