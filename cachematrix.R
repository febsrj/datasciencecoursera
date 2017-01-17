## Matrix inversion is usually a costly computation and there may be some benefit 
## to caching the inverse of a matrix rather than compute it repeatedly.
## The following two functions are used to cache the inverse of a matrix.

## Portuguese: A inversão de matriz é um processo muito custoso computacionalmente. 
## A armazenagem em cache, corresponde a uma area de armazenamento, 
## onde dados ou processos frequentemente ou processo que exija muito da parte computacional
## são armazenados para utilização futura. Não precisando ser calculados novamente.


## makeCacheMatrix creates a list containing a function to
## i.   set the value of the matrix
## ii.  get the value of the matrix
## iii. set the value of inverse of the matrix
## iv.  get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    invm <- NULL
    set <- function(y){
        x <<- y
        invm <<-NULL
    }
    get <- function() x
    setinvm <- function(inverse) 
        invm <<- inverse
    getinvm <- function() invm
    list(set=set, 
         get=get, 
         setinvm=setinvm, 
         getinvm=getinvm)
}


## The following function returns the inverse of the matrix. 
## It first checks if the inverse has already been computed. 
## If so, it gets the result and skips the computation. 
## If not, it computes the inverse, sets the value in the cache via setinvm function.

## Portuguese: A próxima função retorna o inverso da matriz. 
## Primeiro é verificado se a matriz inversa já foi calculada.
## Se sim, é dado o resultado e a matriz inversa não é recalculada.
## Caso contrário, é calculada a matriz inversa através da função setinvm.

## vamos assumir que a matriz é inversível.

cacheSolve <- function(x, ...) {
    invm <- x$getinvm()
    if(!is.null(invm)) {
        message("dados em cache")
        return(invm)
    }
    data <- x$get()
    invm <- solve(data)
    x$setinvm(invm)
    invm
}

## Exemplo:
## x = rbind(c(1, 2), c(3, 4))
## invm = makeCacheMatrix(x)
## m$get()
##       [,1]  [,2]
## [1,]  1.00 -0.25
## [2,] -0.25  1.00


## > invm$get()
## [,1] [,2]
## [1,]    1    2
## [2,]    3    4

## Os dados aqui ainda nao estão em cache
## > cacheSolve(invm)
## [,1] [,2]
## [1,] -2.0  1.0
## [2,]  1.5 -0.5

## Na segunda rodada, os dados estão em cache
## > cacheSolve(invm)
## dados em cache
## [,1] [,2]
## [1,] -2.0  1.0
## [2,]  1.5 -0.5
 

