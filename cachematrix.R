# makeCacheMatrix - creates a "matrix" object which will then have its
# matrix inverse computed using the solve() R function


makeCacheMatrix <- function(x = matrix()) {
        m<-NULL
        set<-function(y){
                x<<-y
                m<<-NULL
        }
        get<-function() x
        
        setmatrix<-function(solve) m<<- solve
        
        getmatrix<-function() m
        
        list(set=set, get=get, setmatrix=setmatrix, getmatrix=getmatrix)
        
# return the list of functions

}



# cacheSolve - determines whether the need exists to compute
# the matrix inverse via getting the value of the matrix inverse from
# the makeCacheMatrix routine. If it is NULL, the matrix inverse will be
# computed; if it is not NULL this implies that the matrix inverse was already
# computed and is "cached"

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        m <- x$getmatrix()
        #obtain the value of the matrix inverse as computed in makeCacheMatrix
        
        if(!is.null(m)){
                message("getting cached data")
                return(m)
        # If the matrix inverse in not NULL, it means that it 
        # has already been computed
        # return the cached data and exit the routine
        }
        datos<-x$get()
        # Get the matrix for which the inverse must be computed
        
        m<-solve(datos, ...)
        
        # Compute the matrix inverser using the "solve()" function
        
        x$setmatrix(m)
        m
        # return the newly computed matrix inverser
}
