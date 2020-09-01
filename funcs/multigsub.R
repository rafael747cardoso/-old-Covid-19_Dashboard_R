
### Function to vectorize the gsub

multigsub = function(x_char, patterns, subs){
    for(i in 1:length(patterns)){
        x_char = gsub(pattern = paste0("\\", patterns[i]),
                      replacement = subs[i],
                      x = x_char)
    }
    return(x_char)
}

