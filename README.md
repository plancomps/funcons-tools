## The `runfct` executable

This executable provides a funcon term interpreter for all funcons in Funcons-beta and Unstable-Funcons-beta

To install:

```
cabal build
cabal install
```

## Installing and running the REPL

To install:

```
cabal build
cabal install
```

To test, run:

```
funcons-repl
```

observe:

```
Available commands:
  :environment :env    show the active bindings from identifiers to values
  :store :sto          show the store with assignments to references
  :mutable :mut <ENT>  show the mutable entity with name <ENT>
  :session             displays the explored traces in the form of a tree
                       with nodes labelled by state identifiers
  :revert <INT>        revert to the state with id <INT>
  :debug <FCT>         start step-by-step execution of funcon term <FCT>
  :step                perform the next step in a step-by-step execution
  :finish              perform all remaining steps of a step-by-step execution
  :help :h             show these commands
  :quit :q             end the exploration
  or just type a funcon term
#1 > 
```

and produce the following interaction:

```
#1> bind("input", read)
> "Hello world"
{ "input" |-> "Hello world" }
#2> string-append(bound("input"),"!")
{ "it" |-> "Hello world!" }
#3> print(bound("it"),"\n")
Hello world!
#4>
```

to take advantage of the non-determinism features, use the --non-deterministic flag with all or a subset of the given sources of non-determinism

```
funcons-repl --non-deterministic value-operations,rules,pattern-matching,interleaving-of-args
```

to (possibly) see the following interaction (in this caused by the `value-operations` source):

```
#1 > some-element {1,2,3}
{"it" |-> 1}
#2 > some-element {1,2,3}
{"it" |-> 2}
#3 > some-element {1,2,3}
{"it" |-> 1}
#4 > some-element {1,2,3}
{"it" |-> 3}
```
