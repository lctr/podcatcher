# PodCatcher
This is a code-along for chapter 22 of the book [Real World
Haskell](http://book.realworldhaskell.org/read/) by Bryan O'Sullivan,
Don Stewart, and John Goerzen.

This application is a podcast downloader. Given a list of URLs to
process, it will download each of the URLs results in an XML file in
RSS format, as well as downloading the audio files whose URLs are
referenced in the downloaded XML file and haven't already been
downloaded by the user.

# Building
First clone this repo, and then `cd` into the directory
```
git clone https://github.com/lctr/podcatcher.git
cd podcatcher
```
Once in the directory, it's a few simple `cabal` commands away.
```
cabal configure
cabal build
cabal install
```