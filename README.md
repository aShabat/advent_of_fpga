This is solutions for Jane Street's Advent of FPGA event. The structure of the project is heavily inspired by [advent of hardcaml 2024](https://github.com/asinghani/advent-of-hardcaml-2024).

## Repo Structure

```
├── fpga
│   ├── bin
|   |   ├── main.ml - Executable that run simulations. 
│   ├── input_parsers - Parsing of inputs for use in simulations
│   └── src - FPGA designs and bench
```

## Implemtation
The implementations don't target real-world hardware. The input is read from fifo. The bench adds fifo wrapper to circuits and submits the input. Syntax of the binary is `main day0x (sample|input)`.

## Days implemented
### Day 1
I implemented division similar to long division. Use it to track the dial and count the rotations.

### Day 3
Circuit implements dynamic programming algorythm for both parts. After reading each digit check which of new `n+1` digits can be removed.

### Day 5
WIP. Plan is to store intervals in linked list in ram, sort by left border then collapse intersecting intervals. Check freshness in linear time.
### Day 7
Keep both the state of the row and splitters in registers. Update and count number of splits for part1 in 1 cycle. WIP.
