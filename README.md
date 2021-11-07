# Hasking

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

There are some tests that you can execute with `stack test`.

## Command line options

The options are as follow
```
hasking [-i] [-s FILE] [-o FILE] [-t TAPE] [-v] [-h]
```

| Short | Long            | Meaning                                                 |
| ----- | --------------- | ------------------------------------------------------- |
| `-v`  | `--version`     | Prints the version                                      |
| `-h`  | `--help`        | Prints the help page                                    |
| `-s`  | `--script`      | The file containing the instructions (default `stdin`)  |
| `-o`  | `--output`      | The output file (default `stdout`)                      |
| `-t`  | `--tape`        | The initial tape to use                                 |
| `-i`  | `--interactive` | Run in interactive mode (terminal must support Unicode) |

The initial tape will overwrite any tape definitions in your file.
If you provide no initial tape using the `-t` option, its value will be searched in the input file.

## The language

The file you pass to the machine contains the machine definition. There are some rules:
* The input and machine alphabet are comprised of ***symbols***, which can be letters, numbers or any characters but one of `; ()[]{}`, and the special *blank symbol* is identified by `.` (dot)
* The ***state*** of the machine is a series of alphanumeric characters that starts with a letter
* The ***direction*** is one of `L`, `R` or `S` (left, right or stay)
* The initial tape is comprised of symbols (even blank ones)
* ***Comments*** are everything that follows a `;`

Given these, there are only 3 types of lines: state transitions, control instructions and the initial tape value.

You can see a complete example in `example.txt`.

The structure is as follows:
```
(machine
  (initial <state>)
  (finals (<state> <state> ...))
  (rules (
    (<state> <symbol> <state> <symbol> <direction>)
    ...
  ))
)

(simulate-on (<symbol> ...))
(simulate-on (<symbol> ...))
...
```

### State transitions

State transitions are in the form of
```
(<state> <symbol> <state> <symbol> <direction>)
```

For example, `(s 1 q 0 R)` means
* If the machine is in the state `s` and is reading `1` off the tape
* Go to state `q`, write `0` onto the tape and then move `R`ight

### Simulations

You can add multiple (or no) tapes on which to simulate the machine.
The instruction is `simulate-on` and *must follow the definition*.

### Tapes

The initial tape has to
* Contain *at least* one symbol