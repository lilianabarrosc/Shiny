
#función que retorna una conexion con la base de datos especificada.
conexionbd <- function(drv){
  #Abrir una conexion
  return (dbConnect(drv, host='localhost', port='5432', dbname='Guinia',
                   user='liliana', password='1234'))
}

#funcion encargada de cerrar la conexion con la bd
desconexionbd <- function(con, drv){
  #Cerrar la conexion
  dbDisconnect(con)
  #liberar la conexion con el driver
  dbUnloadDriver(drv)
}

#directorio donde se alojan los data set
outputDir <- "file"

#funcion encargada de guardar el data set en la carpeta file, y asignarle un
#nombre unico
saveData <- function(data) {
  #data <- t(iris)
  # Create a unique file name
  fileName <- sprintf("%s_%s.csv", as.integer(Sys.time()), digest::digest(data))
  # Write the file to the local system
  write.csv(
    x = data,
    file = file.path(outputDir, fileName), 
    row.names = FALSE, quote = TRUE
  )
  return(fileName)
}