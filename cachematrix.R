## The objective of the following functions is to avoid calculating the
## inverse of a matrix twice by allowing you to storage the inverse of
## the matrix on a special "vector". The first function creates this "vector",
## which is actually a list of functions that:
## -set the value of the matrix
## -return the value of the matrix
## -set the value of inverse (and therefore storage this value)
## -return the value of the inverse (in order to verify if it 
##  was already calculated)
## The second function calculates the inverse of the matrix, but first it
## verifies it the inverse was already calculated by calling one of the 
## functions from the list created before. If the inverse has not been 
## calculated, it does it and storags it on the special "vector".
## Disclamer: I changed the name of the variables as suggested in one of
## forums for the sake of my own understanding

## This function creates the special "vector"

makeCacheMatrix <- function(x = matrix()) {
               
  stored_inverse <- NULL

  set <- function(y) {          ##creates a function 'set' that changes the value of x
                 x <<- y        ## sets 'x' for 'y' in the parent environment 
    stored_inverse <<- NULL     ## reassings the value of the inverse to NULL in the parent
  }                             ## environment (the makeCacheMatrix environment)

  get <- function() {           ## creates a function that returns the value of x
          return(x)
  }
                                                 ## creates a function 'setinverse' that changes
  setinverse <- function(replacement_inverse){   ## the value of the stored inverse 
        stored_inverse <<- replacement_inverse   ## assigns the value 'replacement_inverse' to 
  }                                              ## the stored inverse in the parent environment

  getinverse <- function(){      ## creates a function that returns the value of 
        return(stored_inverse)   ## the inverse
  }
          
  list(set= set, get = get,      ## makes a list of the functions created, so 'cacheSolve'  
     setinverse = setinverse,    ## can acces to them
     getinverse = getinverse)
}


## This function calculates the inverse from the "vector" above.

cacheSolve <- function(cache_matrix, ...) {    ##Acces the cache_matrix environment and assigns 
   local_inverse <- cache_matrix$getinverse()  ##the value of the stored_inverse to the 
                                                 local_inverse
   if(!is.null(local_inverse)) {                
      message("getting cached data")      ##If the stored invere isn't NULL, returns its value
      return(local_inverse)
     }
   else {
      local_data <- cache_matrix$get()   ##If the inverse is null gets the value of the matrix 
      local_inverse <- solve(local_data, ...)   ##solves for the inverse
         
      cache_matrix$setinverse(local_inverse)   ##sets the value of the now calculated inverse 
      return(local_inverse)                    ##in the cache_matrix environment
   }

   ## Return a matrix that is the inverse of 'x'
}
