
.onAttach <- function(libname, pkgname) {
  packageStartupMessage(
    "Welcome to the netbase package!\n",
    "To use this package you need to authenticate yourself with nb_setup(user, password)\n\n",
    "See more information here: https://api.netbase.com/explorer/api/netbase\n",
    "And here: https://nb360.netbase.com/Enterprise/Insight_API"
  )
}
