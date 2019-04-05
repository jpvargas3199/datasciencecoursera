## My function initializes the inverse of a matrix as NULL, 
## then calculates it and saves it in a list.
## After this in a separate function we check for it in cache,
## if not, it calculates it, but if it is, it just calls it and
## does not waste time calculating it.

## This function calculates the cache matrix inverse and saves
## it in a list

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


## This function solves the matrix, if it already has the inverse
## in cache, it calls it, but if not it calculates it and returns it

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

