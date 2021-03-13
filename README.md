# Hasking

A very very simple Turing Machine simulator and interpreter written in Haskell.

## Compile and run

You will need [Stack](https://www.haskellstack.org/) to compile this project.

You can get started by doing
```
git clone http://github.com/micheleberetta98/hasking
cd hasking
stack setup
stack build
stack run example.txt
```

There are some tests that you can execute with `stack test`.

## The language

The file you pass to the machine contains the machine definition. There are some rules:
* Each line is one instruction
* *Atoms* are strings of alphanumeric characters
* *Identifiers* are just atoms that start with a letter
* The input and machine alphabet are comprised of ***symbols***, which can be either an *atom* or the special *blank symbol* `.` (dot)
* The ***state*** of the machine is an *identifier*
* The ***direction*** is one of `L`, `R` or `S` (left, right or stay)
* The initial tape is comprised of non-blank symbols
* ***Comments*** are everything that follows a `#`

Given these, there are only 3 types of lines: state transitions, control instructions and the initial tape value.

You can see a complete example in `example.txt`.

### State transitions

State transitions are in the form of
```
(<state> <symbol> <state> <symbol> <direction>)
```

For example, `(s 1 q 0 R)` means
* If the machine is in the state `s` and is reading `1` off the tape
* Go to state `q`, write `0` onto the tape and then move `R`ight

### Control instructions

These are used to specify options about the machine. They are written as
```
[<command name> <state> <state> ...]
```

Right now, only two are supported
* `[BEGIN s]`, to specify the *one and only* initial state
* `[FINAL s r ... ]` to specify at least one final state

### Initial tape value

This is simply a list of symbols wrapped in curly brackets
```
{<symbol> <symbol> ...}
```

The initial tape has to
* Contain *at least* one symbol
* Do not contain any *blank* symbols
