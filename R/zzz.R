.First.lib <- function(lib, pkg)
{
    require(boot)
    library.dynam("energy", pkg, lib) 
}
