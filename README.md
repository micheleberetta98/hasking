# Hasking

```
      __ __         __    _          
     / // /__  ___ / /__ (_)__  ___ _
    / _  / _ `(_-</  '_// / _ \/ _ `/
   /_//_/\_,_/___/_/\_\/_/_//_/\_, / 
                              /___/  
```

A very very simple Turing Machine simulator and interpreter written in Haskell.

## Compile and run

You will need [Stack](https://www.haskellstack.org/) to compile this project, and then you can just do the following

```
git clone http://github.com/micheleberetta98/hasking
cd hasking
stack setup
stack build
```

Use `stack run -- [options]` to run the project.

There are some tests that you can execute with `stack test` (currently borked).

## Command line options

The options are as follow
```
hasking ((-v | --version) | COMMAND (--stdin | INPUT))
```

* `-v`/`--version` prints the version
* `-h`/`--help` shows the help page
* `INPUT` is the name of a Hasking file (use `--stdin` to read from standard input)
* `COMMAND` is one between
  * `run` to execute a Hasking file
  * `sim` that runs a simulation of a machine (`-m`/`--machine`) on a tape (`-t`/`--tape`)

## Simulation

When you are in the simulation mode, you have to specify both what machine in the file to simulate
and what tape to use (or just leave it empty).

## The language

In any single file, you can define both *machines* and *simulations*: machines have a name and some more properties, while simulations specify
a machine and a tape that is feed into it.

The general form is:
```
; Comments are what follows a ;

(machine <name>
  initial <initial state>
  finals (<list of final states>)
  rules (<list of rules>))

(simulation <machine name> (<tape symbols>))
```

Symbols are any single char, excluding one of `; .[]()`, and `.` is the special blank symbol.
State names are just like variables in other languages: alphanumeric strings that start with a letter.

You can see some examples in `examples`.

### Rules

State transitions are in the form of

```
(<state> <symbol> <state> <symbol> <direction>)
```

Where `direction` is one of `L` (go left), `R` (go right) or `S` (stay.)

For example, `(s 1 q 0 R)` means
* If the machine is in the state `s` and is reading `1` off the tape
* Go to state `q`, write `0` onto the tape and then move `R`ight