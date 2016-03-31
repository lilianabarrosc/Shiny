
#función que retorna una conexion con la base de datos especificada.
conexionbd <- function(drv){
  #Lectura del driver para postgress
  dbListConnections(drv)
  dbGetInfo(drv)
  
  #Abrir una conexion
  return (dbConnect(drv, host='localhost', port='5432', dbname='Guinia_bd',
                   user='postgres', password='liliana10'))
}

#funcion encargada de cerrar la conexion con la bd
desconexionbd <- function(con, drv){
  #Cerrar la conexion
  dbDisconnect(con)
  #liberar la conexion con el driver
  dbUnloadDriver(drv)
}