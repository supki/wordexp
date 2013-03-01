wordexp
=======

The best description of what `wordexp` is doing, apart from [manpage][1] and [haddocks][2] is simple ghci session:

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
