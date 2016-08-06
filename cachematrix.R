## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function allows to create a special
## Matrice that can cache its inverse (i)
## The inner function get and set and are the getters and the setters of ther Matrice. 
## The functions setInverse and getInverse are the getters and the setters of the Inv Matrice.
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <-function(y){
    x<<-y
    i<<-NULL
  }
  get <- function()x
  setInverse <- function(inverse) i<-inverse
  getInverse <- function()i
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## This function is responsable for calculation of the inverse of the matrice
## at first we check if the inverse is already calculated. if so it retrieves it 
## from the cache  otherwise it calculates it and stores it.

cacheSolve <- function(x, ...) {
        i<-x$getInverse()
        if(!is.null(i)){
          message("getting cached matix")
          return(i)
        }
        data = x$get()
        i<-solve(x,...)
        x$setInverse(i)
        i
}
