read_obj_from_rda <- function(path, obj_nm) {
  e <- new.env(parent = emptyenv())
  load(path, envir = e)
  
  if (!exists(obj_nm, envir = e)) {
    warning(obj_nm, " not found in: ", basename(path))
    return(NULL)
  }
  
  e[[obj_nm]]
}