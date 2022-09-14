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

The file you pass to the machine contains the machine definition. There are some rules:
* The input and machine alphabet are comprised of ***symbols***, which can be letters, numbers or any characters but one of `; ()[]{}`, and the special *blank symbol* is identified by `.` (dot)
* The ***state*** of the machine is a series of alphanumeric characters that starts with a letter
* The ***direction*** is one of `L`, `R` or `S` (left, right or stay)
* The initial tape is comprised of symbols (even blank ones)
* ***Comments*** are everything that follows a `;`

Given these, there are only 3 types of lines: state transitions, control instructions and the initial tape value.

You can see some examples in `examples`, such as:
```
; This is a machine that transforms 0s into 1s

(machine zeros-to-ones
  initial s       ; The initial state
  finals (q)      ; The final states (here just 1)
  rules
    ((s 0 s 1 R)     ; If you encounter 0, write 1 and repeat till
     (s . x . L)     ; the end, i.e. when you find the Blank. Now switch direction,
     (x 1 x 1 L)     ; and keep everything the same, until
     (x . q . R)))   ; you get back to the beginning

(simulate zeros-to-ones (0 0 0 0))
(simulate zeros-to-ones (0 0 1 0))
```

Which when run gives the output:
```
> zeros-to-ones on (0 0 0 0) : (1 1 1 1)
! zeros-to-ones reached an invalid state: (s, 1)
```

### State transitions

State transitions are in the form of
```
(<state> <symbol> <state> <symbol> <direction>)
```

For example, `(s 1 q 0 R)` means
* If the machine is in the state `s` and is reading `1` off the tape
* Go to state `q`, write `0` onto the tape and then move `R`ight