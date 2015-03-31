sourceDir <- function()
#convenince function to source all .R files in the directory    
    for (nm in 1:length(list.files(pattern = "\\.R$"))) {
        source(list.files(pattern = "\\.R$")[nm])
    }
