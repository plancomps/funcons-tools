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
