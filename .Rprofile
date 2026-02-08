source("renv/activate.R")
if (Sys.getenv("TERM_PROGRAM") == "tmux") {
    Sys.setenv("TERM_PROGRAM" = "vscode")
    source("~/.vscode-R/init.R")
    message("Running in VSCode")
} else if (Sys.getenv("RSTUDIO") == "1") {
    message("Running in RStudio")
} else {
    message("Running in another environment")
}
