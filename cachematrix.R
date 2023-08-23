## Put comments here that give an overall description of what your
## functions do


#We have to make two functions, 
#first will get and set matrix and its inverse 
#second will computes the inverse of matrix, 
#if inverse have been already calculated, the cached inverse is returned


makecachematrix <- function(x = matrix()){
  inver = NULL    #cached inverse of matrix
  get <- function()x    #get/set matrix
  set <- function(y){
    x <<- y
    inver = NULL
  }
  getinv <- function()inver    #get/set matrix inverse
  setinv <- function(inverse)inver <<- inverse
  #return
  list(get = get, set = set, getinv = getinv, setinv = setinv)
  
  
}

cachesolve <- function(x,...){
  inver <- x$getinv()
  if(!is.null(inver)){
    message("inverse is cached")
    return(inver)
    
  }
  m <- x$get()    #compute inverse of matrix
  inver <- (solve(m,...))
  x$setinv(inver)    #catch inverse
  return(inver)      #return inverse
  
}