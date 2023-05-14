putrd <-
function(x, desc = deparse(substitute(x)), i = idxrd() + 1,usedesc=FALSE) {
    if(usedesc && 'desc'%in%names(attributes(x))) { desc <- attr(x,'desc') }
    n <- numtotxt(i) #formatC(i, width = 5, format = "d", flag = "0")
    fnam <- paste(c(n, as.character(as.Date(Sys.time())), abbrev(desc,len=35)), collapse = rddelim())
    if (i == 0) {
        save(x, file = paste0(rdroot(), "/rd/", fnam, ".RData"))
    } else {
        i0 <- idxrd()
        save(x, file = paste0(rdroot(), "/rd/", fnam, ".RData"))
        i1 <- idxrd()
        ifelse(i1 == i0 + 1, i1, NA)
    }
}
getrd <-
function(i = idxrd()) {
    n <- formatC(i, width = 5, format = "d", flag = "0")
    fnam <- paste0(paste0(dirrd()[n], collapse = rddelim()), ".RData")
    load(file = paste0(rdroot(), "/rd/", fnam))
    x
}
dirrd <-
function() {
    dd <- paste0(rdroot(), "/rd")
    l1 <- lapply(lapply(lapply(dir(dd), strsplit, split = "\\."), "[[", 1), "[", 1)
    num <- unlist(lapply(lapply(lapply(l1, strsplit, split = rddelim()), "[[", 1), "[", 1))
    dat <- unlist(lapply(lapply(lapply(l1, strsplit, split = rddelim()), "[[", 1), "[", 2))
    des <- unlist(lapply(lapply(lapply(l1, strsplit, split = rddelim()), "[[", 1), "[", 3))
    if(is.null(num)) shell(rdroot()) #this is a means to get rdroot() echoed from server
    setkeyv(data.table(data.frame(num = num, dat = dat, des = des)), "num")[]
}
