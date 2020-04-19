## Programming Assignment #2
## Alexandros Moissiadis, April 2020
## Coursera Account: amoiss1@hotmail.com

## For this assignment, I was asked to write two functions that allow me 
## if I use them together to cache the inverse of a square & invertible 
## matrix without computing it repeatadly.


## 1st function "MakeCacheMatrix". This function has a matrix as an argument
## and returns a "special" object which is a list of functions that are defined
## inside the function "MakeCacheMatrix". This means, that the MakeCacheMatrix
## is the parent environment to the defined functions (set,get,setinv, and 
## getinv). As a result, the argument of the makeCacheMatrix and variables
## that are defined there are being inherited to the individual functions.
## In other words, this is Lexical Scoping. Variables that are not defined 
## in the individual defined functions (get,set,etc) and are free variables, R 
## will attemp to find their values from the parent environment which is the one
## for the function "MakeCacheMatrix".

makeCacheMatrix <- function(x=matrix()){
## First, I define inv which is the variable for the inverse matrix 
## and it shall be initiated
    inv <- NULL
## The following function is useful when I already have defined once
## my object myMat<-makeCacheMatrix (ma1) where ma1 is a square matrix and I 
## want to put another square matrix ma2 so I do not have to
## create another object myMat from scratch
    set <- function(a){
        ##Passing the argument a (which is a matrix) through "superassignment" 
        ## so the x is modified in the parent enviroment, which is the 
        ## makeCacheMatrix function's environment
        x <<- a
        ##Re-iniatiating the inv since I am inputting a new matrix.
        ##I dont want to have the risk of remaining values from previous runs.
        inv <<- NULL
    }
    ## Getting the matrix based on how is defined in the parent environment
    get <- function() x
    ## Passing the argument b which is the calculated inverse matrix in 
    ## the parent environment's variable "inv" using "superassignment" 
    setinv <- function(b) inv <<- b
    ## Getting the inverse matrix based on how is defined in 
    ## the parent environment
    getinv <- function () inv
    ## Putting all the functions in a list and giving them names, so I can use
    ## the $ operator when calling them elsewhere
    list( set=set, get=get, 
          setinv=setinv, 
          getinv=getinv)
}

## 2nd function "cacheSolve". This function has an argument the special object
## that was created from the above function. In other words, I have this
## list as input which allows me to use any of the defined functions (get,etc.)
## by using the $ sign in the "cacheSolve". This function checks if for the 
## particular inputted object, the inverse matrix is already calculated. If it 
## is, it retrieves it and prints it back. If the inverse matrix is not 
## calculated,I used the function "solve" to find the inverse matrix, save it
## for this object, and then print it!
cacheSolve <- function (o,...){
    ## Getting the inverse matrix for the "o" object.
    ## If this is the first time that the inverse matrix is being calculated
    ## the inverse_temp shall be NULL.
    inverse_temp<- o$getinv()
    ## Checking if the inverse matrix has been calculated
    if (!is.null(inverse_temp)){
        ## If inverse_temp is NOT NULL
        ## it prints the following message and returns
        ## the inverse matrix
        message("getting cached data")
        return(inverse_temp)
    } 
    ## If inverse_temp is NULL meaning that inverse has not been calculated
    ## I am retrieving the matrix from the object "o" using the get function
    ## and saving it to a local variable "temp_matrix"
    temp_matrix<-o$get()
    ## Calculating the inverse matrix using the function "solve"
    inverse_temp<-solve(temp_matrix)
    ## Next, setting the inverse matrix for the o object. This way,if next time
    ## the inverse is being asked for o and the matrix has NOT changed, the 
    ## function cacheSolve will retrieve the inverse from the cache.
    o$setinv(inverse_temp)
    ## Printing the inverse matrix
    inverse_temp
}