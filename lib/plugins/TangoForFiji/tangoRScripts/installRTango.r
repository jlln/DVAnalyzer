#Get script location information
args <- commandArgs(trailingOnly = F)
script.directory <- normalizePath(dirname(sub("^--file=", "", args[grep("^--file=", args)])))

#locate R directories
rhome = R.home()

#Install dependencies
officialdependencies=c("grid","ggplot2","plyr","iplots","rJava","JavaGD")
localpackages=c("rmongodb","rmpo","rtango")
type=getOption("pkgType")
repos="http://cran.r-project.org"
installed=as.data.frame(installed.packages())
for(p in officialdependencies) {
  if(is.na(charmatch(p, installed[,1]))) {
    install.packages(p, repos=repos)
  }
}
for(p in localpackages){
  if(is.na(charmatch(p, installed[,1]))) {
    install.packages(p, repos = 'http://download.tuxfamily.org/tango/r')
  }
}
rm(installed)
rm(localpackages)
rm(type)
rm(repos)
rm(p)
rm(officialdependencies)

#system independent commands
setwd(script.directory)
template.directory <- file.path(script.directory,'RTemplates')
setwd("..") #imagej/plugins directory
plugins.directory <- getwd()
setwd("..") #imagej directory
imagej.directory <- getwd()
require(rJava)
require(JavaGD)
jripath = file.path(path.package("rJava", quiet = FALSE), "jri")
javagdpath = path.package("JavaGD", quiet = FALSE)

#R environment variables
values = list('JRI_HOME'=jripath,'JAVAGD_HOME'=javagdpath,'R_HOME'=rhome)

#build launchers from templates
buildLauncher = function(filename, values, directory){
  print(paste('building file ',filename,' from ', file.path(template.directory,filename),' into ',directory,sep=' '))
  x <- readLines(file.path(template.directory,filename))
  for(field in names(values)){
    x <- gsub( paste('\\$',field,sep=''), values[[field]], x )
  }
  print(x)
  cat(x, file=file.path(directory,filename), sep="\n")
}

#system dependent commands
if(Sys.info()[['sysname']]=='Linux'){
  #build run file in imagej directory
  buildLauncher('run',values,imagej.directory)
}
if(Sys.info()['sysname']=='Darwin'){
  #set environment variables for info.plist
  buildLauncher('Info.plist',values,file.path(file.path(imagej.directory,'ImageJ64.app'),'Contents'))
  #buildLauncher('run.command',values,imagej.directory)
}
if(Sys.info()['sysname']=='Windows'){
  buildLauncher('imagej.bat',values,imagej.directory)
  buildLauncher('ImageJ.cfg',values,imagej.directory)
}