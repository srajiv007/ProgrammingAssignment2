# Programming Assignment: 2 (cache matrix)

## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix: wrapper around the default matrix to read and save matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
        m<-NULL
        get<-function() x
        set<-function(y){
                x<<-y
                m<<-NULL
        }
        
        setInverse<-function(i){
                m<<-i
        }
        
        getInverse<-function() m
        
        list(getM=get, setM=set, setI=setInverse, getI=getInverse)
}


## cacheSolve returns cached-inverse of the input matrix, 
## if not found in cache it would create and save the calculated inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv<-x$getI()
        
        if(!is.null(inv)){
                print("cached matrix inverse")
                return (inv)
        }
        
        m<-x$getM()
        inv<-solve(m, ...)
        x$setI(inv)
        return(inv)
}
