checkSlots <- function(x,y,...) {
    for (i in slotNames(x)) {
        cat(i,"\n")
        print(all.equal(slot(x,i),slot(y,i),...))
    }
}

checkElements <- function(x,y,...) {
    for (i in names(x)) {
        cat(i,"\n")
        print(all.equal(x[[i]],y[[i]],...))
    }
}
