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
#2> string-append(bound("input"),"!")
{ "it" |-> "Hello world!" }
#3> print-line(bound("it"),"\n")
Hello world!
#4>
```
