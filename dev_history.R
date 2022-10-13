
# make new compendium
#create new project in rstudio

rcompendium::new_compendium(create_repo = FALSE) # run this courtesie of Nicolas Casajus

#link compendium to github repo
#1 make empty github repo
#2 copy second paragraph into rstudio terminal (not console!)
#refresh environment or restart rstudio (should show git tab)
#refresh github - should show first commit and push

#need to edit DESCRIPTION file

#to automatically add/remove dependencies
rcompendium::add_dependencies(here::here())

##load all dependencies (same as above)
remotes::install_deps(upgrade = "never") ### install packages in DESCRIPTION


#update NAMESPACE and add .Rd file for each function in man folder
devtools::document()

#load all functions
devtools::load_all()



#command to stage files for git commit
git add -A
ls -al ~/.ssh


#command to stash files followed by pull/ the statsh pop

$git status
$git stash
$git pull
$git stash pop
$git stash clear


#shortcuts 

𝗖𝘁𝗿𝗹 + 𝗦𝗵𝗶𝗳𝘁 + 𝗖 to (un)comment
𝗖𝘁𝗿𝗹 + 𝗦𝗵𝗶𝗳𝘁 + 𝗠 to build your pipes quicker.
Instead of copy/pasting code chunks you can simply move them up and down by pressing 𝗔𝗹𝘁_𝗹𝗲𝗳𝘁 + 𝗨𝗽/𝗗𝗼𝘄𝗻
Find in files - An absolute killer feature of RStudio when working on larger projects with multiple files. Use  𝗖𝘁𝗿𝗹 + 𝗦𝗵𝗶𝗳𝘁 + 𝗙 to search (and even replace) keywords across your project.
Rename in scope - Sometimes you want to use a different variable name or call a different function in certain code sections. Search and Replace can be dangerous in these situations. It's safer to use 𝗖𝘁𝗿𝗹 + 𝗦𝗵𝗶𝗳𝘁 + 𝗔𝗹𝘁_𝗹𝗲𝗳𝘁 + 𝗠 to rename in scope


