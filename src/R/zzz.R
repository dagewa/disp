.First.lib <- function(libname, pkgname)
{
  if (version$major<=2 && version$minor < 11.1) warning("This package includes code that depends on R version 2.11.1 or later")
  library.dynam("DISP", pkgname, libname)
  ver <- read.dcf(file.path(libname ,pkgname,"DESCRIPTION"), "Version")
  msg <- sprintf("Diffraction Image Statistics Package (version %s)\n", as.character(ver))
  packageStartupMessage(msg)
}
