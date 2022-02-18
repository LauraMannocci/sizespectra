#create compendium

devtools::install_github("benmarwick/rrtools")

rrtools::use_compendium("../sizespectra", open = FALSE)

#need to edit DESCRIPTION file

#create directories
dir.create("R")

dir.create("outputs")

dir.create("data")


#to automatically add/remove dependencies
rcompendium::add_dependencies(here::here())




#update NAMESPACE and add .Rd file for each function in man folder
devtools::document()

#load all functions
devtools::load_all()


#command to stage files for git commit
git add -A
ls -al ~/.ssh
