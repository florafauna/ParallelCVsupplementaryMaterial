## print R and system information
pkg <- c(system("grep library *.R -hr", intern=TRUE),
         system("grep require *.R -hr", intern=TRUE))
eval(parse(text=pkg))

sessionInfo()

system("cat /proc/cpuinfo")

system("cat /proc/meminfo")
