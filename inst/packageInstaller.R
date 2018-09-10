if (!require("batchtools",character.only = TRUE))
{
    install.packages("batchtools",dep=TRUE)
}
if (!require("devtools",character.only = TRUE))
{
    install.packages("devtools",dep=TRUE)
}
devtools::install_github("frehbach/ideaBatch")
