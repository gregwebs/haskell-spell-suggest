# Thimk and spelling-suggest
Spelling word suggestion tool and library  
Copyright © 2010 Bart Massey and Greg Weber

This software is licensed under the "3-clause ('new')
BSD License".  Please see the file COPYING provided with
this distribution for license terms.

This package is a newer version of the original package called "thimk".

"thimk" (an old joke) is a command-line spelling word
suggestion tool.  You give it a possibly-misspelled word,
and it spits out one or more properly-spelled words in order
of likelihood of similarity.

Thimk is structured as a command-line interface to its
`spelling-suggest` library, originally split out by Greg
Weber. You can use this library for other applications
also. There is sufficient Haddock to work out how to use
it. It is packaged on Hackage as `spelling-suggest`.

There is little documentation of the thimk command as-of
yet, but the usage message from the program should tell
everything needed to get started with it.

The idea and name for thimk came from an old program that
used to hang around Reed College, probably written by Graham
Ross and now apparently lost in the mists of time.  See this
[Usenet
post](http://groups.google.com/group/net.sources/msg/8856593862fe22bd)
for the one very vague reference I've found on the web (in
the SEE ALSO section of the referenced manpage). I
originally re-implemented thimk in [Nickle](http://nickle.org) some years ago,
but that implementation has been slow, clunky, and
non-portable.

The current implementation is a bit more sophisticated
than I recall the original being. By
default it uses a prefilter that discards words with
large edit distances from the target, then filters words
with a different phonetic code than the target, then
presents the top result sorted by edit distance.

The Soundex and Phonix phonetic codes are designed for
names, but seem to work about the same with other words.
I follow the common practice of not truncating the codes
for greater precision, although Phonix does truncate its
final "sound" for greater recall.

The latest change to the implementation is an addition
of an optional precompiled SQlite database of phonetic
codes for the entire dictionary, created with
"thimk-makedb".  This greatly speeds lookup, permitting
reasonable performance on enormous dictionaries.

Building thimk and spelling-suggest requires my `parseargs`
and `phonetic-code` packages from hackage, as well as
`edit-distance` and
[sqlite-0.5.1](http://hackage.haskell.org/cgi-bin/hackage-scripts/package/sqlite)
or newer if you want to build and use the optional phonetic
codes database. It is probably easiest to build using cabal-install,
which should take care of most everything for you.

--Bart Massey 2012-08-26
