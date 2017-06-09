# smallcov README

`smallcov` is a small utility that determines which tests execute certain
methods.  It is written in OCaml and relies on the CIL library for
parsing/manipulating C programs.

## Limitations

`smallcov` is primarily intended for use with ManyBugs.  In theory, it's relatively
general, but I've only really tested it the ManyBugs context. 

Additionally, `smallcov` probably only works on single-file scenarios for now,
because of assumptions it makes about the structure of the `diffs/` directory.

## Caveat

Sometimes, because it is heuristic, `smallcov` does weird things, so smell-check
your output.

## Usage

At a high level, a user provides `smallcov` information on how to compile a
program and run tests.  While the user can also specify the function names of
interest, the more normal usage specifies diff files between two versions.
`smallcov` parses the diff file to determine modified lines, then parses the
program input files and processes them to identify the functions containing the
modified lines. 

`./smallcov --help` is relatively informative.  However, the `smallcov/` src dir
contains a script, `mkconfig.sh`, which munges MB `configuration-default` files for
arbitrary scenarios to produce an appropriate `smallcov` config file, and then
calls `smallcov` on it.  Given that it actually calls `smallcov` it probably
should be named something else.  Whatever.

For example, on php:2012-03-22-3efc9f2f78-2e19cccad7, `mkconfig.sh` produces the
following cov.config: 

```--program manifest.txt
--pos-tests 7947
--neg-tests 4
--testscript ./test.sh
--prefix preprocessed
--compcmd ./compile.sh __EXE_NAME__
--diffs ext/spl/spl_directory.c-diff
```

This, plus the defaults, suffices.  

## Real Usage

What you really want to do is run this all in `repairbox` automatically and in
headless mode.  There are two ways to do this, one that definitely works and one
that should work soon:

### Definitely works

There is a Dockerfile in clg-scratch/ that will make a container with both
`smallcov` and `mkconfig.sh` installed in `/opt/smallcov`:

`cd ~/clg-scratch
docker build -t clg/smallcov .`

Then, from RepairBox:

`cd ~/RepairBox
./repairbox execute manybugs:php:2011-11-11-fcbfbea8d2-c1e510aea8 ~/clg-scratch/smallcov/mkconfig.sh --with smallcov`

the `execute` argument tells `repairbox` to launch the container in headless mode
and then run the script specified as the third argument
(`~/clg-scratch/smallcov/mkconfig.sh`).  The second argument is the scenario to
run (standard rbx procedure).  The `--with <tool>` specifies that the container
should include the `smallcov` tool install (and you made that available when you
built `smallcov`, above).

### Will work soon

I'm working to export clg/smallcov to the squaresLab dockerHub account, at which
point you won't need to build the container that contains the `smallcov` tool yourself.

## Workflow and output

`smallcov` parses the diff files to identify which functions to instrument,
instruments those functions, compiles the code, and then runs all the tests on
it.

Given the vagaries of MB, it is likely that not all positive test cases will
pass.  Make sure most of them do, but if it's just an occassional failure, don't
worry about it, it's probably "good enough."

`smallcov` first outputs the functions it will instrument, and finishes by
spitting out the test identifiers that touch the instrumented functions.
