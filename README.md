wordexp
=======
[![Hackage](https://budueba.com/hackage/wordexp)](http://hackage.haskell.org/package/wordexp)
[![Build Status](https://secure.travis-ci.org/supki/wordexp.png?branch=master)](http://travis-ci.org/supki/wordexp)

Apart from [manpage][1] and [haddocks][2] the best description of what `wordexp` is doing is simple ghci session:

```haskell
>>> import System.Wordexp.Simple
>>> wordexp "~"
["/home/maksenov"]
>>> wordexp "$SHELL"
["/usr/bin/zsh"]
>>> wordexp "~/[s-t]*"
["/home/maksenov/sandbox","/home/maksenov/svn","/home/maksenov/temp"]
>>> wordexp "~/<"
*** Exception: IllegalCharacterOccurence
```

 [1]: http://linux.die.net/man/3/wordexp
 [2]: http://supki.github.com/wordexp/
