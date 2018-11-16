## Hola, en este pequeño script, trataré de desarrollar la tarea de la
## Semana 3 del curso 02 "R Programming" de Data Science ofrecido por
## John Hopkins University
##
## Lo que realizan las funciones solicitadas, es almacenar en caché una 
## determinada matriz y, posteriormente, invertirla. Dado que es un proceso
## que consumo bastantes recursos, es conveniente tener funciones que sean
## capaces de almacenar los resultados, sobre todo si hay matrices muy grandes,
## ya sea a través de iteraciones (loops) o a través de otras funciones, donde
## sea necesario rescatar su resultado
##
## Al final del script, se muestran 2 ejemplos, con distinta complejidad numérica,
## para mostrar los resultados esperados


## 1º función: makeCacheMatrix
## esta función permite almacenar en el caché , la matriz solicitada para
## posteriormente ser usada por la siguiente función

makeCacheMatrix <- function(x = matrix()) {
    ## Establecemos inicialmente todo NULL, que cambia cuando 
    ## el usuario define el valor
    alpha <- NULL
    
    ## Definimos la matriz, sin invertirla
    set <- function(y){
        x <<- y
        alpha <<- NULL
    }
    
    ## Conseguimos la matriz
    get <- function() x
    
    ## Se invierte la matriz manualmente
    setinverse <- function(inverse) alpha <<- inverse
    
    ## Se obtiene el inverso
    getinverse<- function() alpha
    
    ## Se compila
    list(set=set, get= get,
         setinverse = setinverse,
         getinverse= getinverse)
}


## La 2da función de este script, permite obtener desde la caché la matriz almacenada
## y la invierta, asignándola como objeto que, posteriormente, puede ser rescatada
## sin realizar nuevamente el cálculo. Además, posee un mensaje en el cual se menciona
## que la matriz, previamente analizada, ha sido retornada.
## Se muestra el primer ejemplo con un objeto y, que contiene una matriz de 
## 2 filas y 2 comunas, con los valores del 1 al 4. Se almacena la matriz en cache
## y posteriormente se invierte en el objeto "inversion", quedando una matriz
##     [,1] [,2]
##[1,]   -2  1.5
##[2,]    1 -0.5
##
## Se muestran 2 ejemplos más, con números más complejos

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    
    ## Establecemos inicialmente todo NULL, que cambia cuando 
    ## el usuario define el valor
    alpha <- x$getinverse()
    
    ## Qué pasa si ...
    if(!is.null(alpha)) {
        ## Señalar que ya se ha hecho la inversión
        message("Recuperando matriz invertida ...")
        return(alpha)
    }
    
    ## Consiguiendo la matriz
    datos <- x$get()
    
    ## Encontrar la matriz inversa
    alpha <-solve(datos, ...)
    
    ## Almacendo en caché el nuevo objeto
    x$setinverse(alpha)
    
    ## Retornar el nuevo valor
    alpha
}

## 1st Example

y <- matrix (1:4,2,2)
almanac <- makeCacheMatrix(y)
inversion <- cacheSolve(almanac)
print (y) && print(inversion)

#2nd Example

x <-matrix(runif(4,0,100),2,2)
prueba<-makeCacheMatrix(x)
matrizinv<-cacheSolve(prueba)
print(x) && print(matrizinv)


#3rd Example

x1 <- matrix(runif(25,0,1500),5,5)
segprueba<-makeCacheMatrix(x1)
matrizinv2<-cacheSolve(segprueba)
print(x1) && print(matrizinv2)