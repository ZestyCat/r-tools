# Randomly generate letters until you get the input

find_word <- function(word = "hey") {
    l <- nchar(word)
    print(l)
    i <- 0
    while (TRUE) {
        set.seed(i)
        w <- paste0(sample(letters, l), collapse = "")
        print(paste(w, "seed:", i))
        if (w == word) {
            print("Match")
            print(paste("The seed is:", i))
            return(i)
        } else {
            i <- i + 1
        }
    }
}
