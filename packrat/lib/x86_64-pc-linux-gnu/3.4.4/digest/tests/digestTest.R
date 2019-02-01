
## tests for digest, taken from the examples in the manual page

stopifnot(require(digest))

## Standard RFC 1321 test vectors
md5Input <-
    c("",
      "a",
      "abc",
      "message digest",
      "abcdefghijklmnopqrstuvwxyz",
      "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789",
      paste("12345678901234567890123456789012345678901234567890123456789012",
            "345678901234567890", sep=""))
md5Output <-
    c("d41d8cd98f00b204e9800998ecf8427e",
      "0cc175b9c0f1b6a831c399e269772661",
      "900150983cd24fb0d6963f7d28e17f72",
      "f96b697d7cb7938d525a2f31aaf161d0",
      "c3fcd3d76192e4007dfb496cca67e13b",
      "d174ab98d277d9f5a5611c2c9f419d9f",
      "57edf4a22be3c955ac49da2e2107b67a")

for (i in seq(along.with=md5Input)) {
    md5 <- digest(md5Input[i], serialize=FALSE)
    stopifnot(identical(md5, md5Output[i]))
    cat(md5, "\n")
}

## md5 raw output test
for (i in seq(along.with=md5Input)) {
    md5 <- digest(md5Input[i], serialize=FALSE, raw=TRUE)
    md5 <- gsub(" ","",capture.output(cat(md5)))
    stopifnot(identical(md5, md5Output[i]))
    cat(md5, "\n")
}

sha1Input <-
    c("abc",
      "abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq",
      NULL)
sha1Output <-
    c("a9993e364706816aba3e25717850c26c9cd0d89d",
      "84983e441c3bd26ebaae4aa1f95129e5e54670f1",
      "34aa973cd4c4daa4f61eeb2bdbad27316534016f")

for (i in seq(along.with=sha1Input)) {
    sha1 <- digest(sha1Input[i], algo="sha1", serialize=FALSE)
    stopifnot(identical(sha1, sha1Output[i]))
    cat(sha1, "\n")
}

## sha1 raw output test
for (i in seq(along.with=sha1Input)) {
    sha1 <- digest(sha1Input[i], algo="sha1", serialize=FALSE, raw=TRUE)
    print(sha1)
    sha1 <- gsub(" ","",capture.output(cat(sha1)))
    print(sha1)
    print(sha1Output[i])
    stopifnot(identical(sha1, sha1Output[i]))
    cat(sha1, "\n")
}


## sha512 test
sha512Input <-c(
    "",
    "The quick brown fox jumps over the lazy dog."
    )
sha512Output <- c(
    "cf83e1357eefb8bdf1542850d66d8007d620e4050b5715dc83f4a921d36ce9ce47d0d13c5d85f2b0ff8318d2877eec2f63b931bd47417a81a538327af927da3e",
    "91ea1245f20d46ae9a037a989f54f1f790f0a47607eeb8a14d12890cea77a1bbc6c7ed9cf205e67b7f2b8fd4c7dfd3a7a8617e45f3c463d481c7e586c39ac1ed")

for (i in seq(along.with=sha512Input)) {
    sha512 <- digest(sha512Input[i], algo="sha512", serialize=FALSE)
    stopifnot(identical(sha512, sha512Output[i]))
    cat(sha512, "\n")
}

## sha512 raw output test
for (i in seq(along.with=sha512Input)) {
    sha512 <- digest(sha512Input[i], algo="sha512", serialize=FALSE, raw=TRUE)
    print(sha512)

    sha512 <- gsub(" ","",capture.output(cat(sha512)))
    print(sha512)
    print(sha512Output[i])
    stopifnot(identical(sha512, sha512Output[i]))
    cat(sha512, "\n")
}



crc32Input <-
    c("abc",
      "abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq",
      NULL)
crc32Output <-
    c("352441c2",
      "171a3f5f",
      "2ef80172")

for (i in seq(along.with=crc32Input)) {
    crc32 <- digest(crc32Input[i], algo="crc32", serialize=FALSE)
    stopifnot(identical(crc32, crc32Output[i]))
    cat(crc32, "\n")
}

## one of the FIPS-
sha1 <- digest("abc", algo="sha1", serialize=FALSE)
stopifnot(identical(sha1, "a9993e364706816aba3e25717850c26c9cd0d89d"))

## This one seems to give slightly different output depending on the R version used
##
##                                      # example of a digest of a standard R list structure
## cat(digest(list(LETTERS, data.frame(a=letters[1:5],
##                                     b=matrix(1:10,
##                                     ncol=2)))), "\n")

## these outputs were calculated using xxh32sum
xxhash32Input <-
    c("abc",
      "abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq",
      "")
xxhash32Output <-
    c("32d153ff",
      "89ea60c3",
      "02cc5d05")

for (i in seq(along.with=xxhash32Input)) {
    xxhash32 <- digest(xxhash32Input[i], algo="xxhash32", serialize=FALSE)
    cat(xxhash32, "\n")
    stopifnot(identical(xxhash32, xxhash32Output[i]))
}

## these outputs were calculated using xxh64sum
xxhash64Input <-
    c("abc",
      "abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq",
      "")
xxhash64Output <-
    c("44bc2cf5ad770999",
      "f06103773e8585df",
      "ef46db3751d8e999")

for (i in seq(along.with=xxhash64Input)) {
    xxhash64 <- digest(xxhash64Input[i], algo="xxhash64", serialize=FALSE)
    cat(xxhash64, "\n")
    stopifnot(identical(xxhash64, xxhash64Output[i]))
}

## these outputs were calculated using mmh3 python package
## the first two are also shown at this StackOverflow question on test vectors
##   https://stackoverflow.com/questions/14747343/murmurhash3-test-vectors
murmur32Input <-
    c("abc",
      "abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq",
      "")
murmur32Output <-
    c("b3dd93fa",
      "ee925b90",
      "00000000")

for (i in seq(along.with=murmur32Input)) {
    murmur32 <- digest(murmur32Input[i], algo="murmur32", serialize=FALSE)
    cat(murmur32, "\n")
    stopifnot(identical(murmur32, murmur32Output[i]))
}
## test 'length' parameter and file input
##fname <- file.path(R.home(),"COPYING")  ## not invariant across OSs
fname <- system.file("GPL-2", package="digest")
x <- readChar(fname, file.info(fname)$size) # read file
xskip <- substring(x, first=20+1)
for (alg in c("sha1", "md5", "crc32", "sha256", "sha512",
              "xxhash32", "xxhash64", "murmur32")) {
                                        # partial file
    h1 <- digest(x    , length=18000, algo=alg, serialize=FALSE)
    h2 <- digest(fname, length=18000, algo=alg, serialize=FALSE, file=TRUE)
    h3 <- digest(substr(x,1,18000)  , algo=alg, serialize=FALSE)
    stopifnot( identical(h1,h2), identical(h1,h3) )
    cat(h1, "\n", h2, "\n", h3, "\n")
                                        # whole file
    h4 <- digest(x    , algo=alg, serialize=FALSE)
    h5 <- digest(fname, algo=alg, serialize=FALSE, file=TRUE)
    stopifnot( identical(h4,h5) )

    ## Assert that 'skip' works
    h6 <- digest(xskip, algo=alg, serialize=FALSE)
    h7 <- digest(fname, algo=alg, serialize=FALSE, skip=20, file=TRUE)
    stopifnot( identical(h6, h7) )
}

## compare md5 algorithm to other tools
library(tools)
##fname <- file.path(R.home(),"COPYING")  ## not invariant across OSs
fname <- system.file("GPL-2", package="digest")
h1 <- as.character(md5sum(fname))
h2 <- digest(fname, algo="md5", file=TRUE)
stopifnot( identical(h1,h2) )

## Make sure we don't core dump with unreadable files.
fname <- tempfile()
cat("Hello World, you won't have access to read me", file=fname)
on.exit(unlink(fname))
Sys.chmod(fname, mode="0000")
try(res <- digest(fname, file=TRUE), silent=TRUE)
