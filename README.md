To install:

```
cabal build
cabal install
```

To test, run:

```
funcons-repl
```

and produce the following interaction:

```
#1> bind("input", read)
> "Hello world"
#2> print(bound("input"))
Hello world
#3> 
```
