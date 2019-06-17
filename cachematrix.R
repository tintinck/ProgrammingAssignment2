## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

#####
#Creating the makeCacheMatrix function

makeCacheMatrix <- function(x = matrix()) {  #defines a matrix as the input of your function 
        i <- NULL                            #creates varibale i for inverse of matrix
        set <- function(y) {                 
                x <<- y                      #assigns y to x in the parent enviroment
                i <<- NULL
                
        }
        get <- function() x                              #gets x 
        setinverse <- function(inverse) i <<- inverse    #assigns i to inverse in the parent enviroment 
        getinverse <- function() i                       #gets i 
        list(set = set, get = get, setinverse = setinverse, 
             getinverse = getinverse)
        
}

###
#Creating the cacheSolve function 

cacheSolve <- function(x,...){
        i <- x$getinverse                               
        if(!is.na(i)){                                     #checks to see if inverse has aready been calculated
                message("getting inverse from cache")      #if true returns
                return(i)
        }
        
        data <- x$get()
        m <- inverse(data, ...)                           #if false it calculates the inverse of the matrix and caches it
        x$setinverse(i)
        i
}


