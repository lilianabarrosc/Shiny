#install.packages("RPostgreSQL")
library(RPostgreSQL) #Libreria para postgress

#función que retorna una conexion con la base de datos especificada.
conexionbd <- function(){
  #Lectura del driver para postgress
  drv <- dbDriver("PostgreSQL")
  dbListConnections(drv)
  dbGetInfo(drv)
  
  #Abrir una conexion
  return (dbConnect(drv, host='localhost', port='5432', dbname='Guinia_bd',
                   user='postgres', password='liliana10'))
}

#funcion encargada de cerrar la conexion con la bd
desconexionbd <- function(){
  #Cerrar la conexion
  dbDisconnect(con)
  #liberar la conexion con el driver
  dbUnloadDriver(drv)
}