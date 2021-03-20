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
{ "input" |-> "Hello world" }
#2> print-line(bound("input"))
Hello world
#3> 
```
