candidates <- c( Sys.getenv("R_PROFILE"),
             file.path(Sys.getenv("R_HOME"), "etc", "Rprofile.site"),
             Sys.getenv("R_PROFILE_USER"),
             file.path(getwd(), ".Rprofile"),
             file.path(Sys.getenv("HOME"), ".Rprofile"))

Filter(file.exists, candidates)
#returns [1] "C:/PROGRA~1/R/R-42~1.1/etc/Rprofile.site"    "C:/Users/Giles/anest.repo/ijf2305/.Rprofile"

#Rprofile.site: this has only one harmless entry :  options(help_type="html")

#the one in home dir has more
getwd()
