# SysY Optimizing Compiler

This is a toy compiler project, which compiles **SysY** language
and targeting at Raspberry Pi 4B (32-bit).
The compiler focus on obtaining performance as high as possible,
in comparison with **gcc -O3**.

### Features

If possible, the compiler may include the following optimization
passes (in my hope).

#### Front-end
- [ ] Top-down parser

#### Mid-end & Back-end

- [ ] Array SSA
- [ ] Dominator Tree
- [ ] Sparse conditional constant propagation
- [ ] Dead code elimination
- [ ] Partial redundancy elimination
- [ ] Strength reduction
- [ ] Data layout transformation
- [ ] Polyhedral compilation
- [ ] Auto vectorization
- [ ] Multithreading
- [ ] If conversion
- [ ] Hoisting
- [ ] Jump table optimization
- [ ] Peephole optimization
- [ ] Software pipelining
- [ ] Graph coloring register allocation / optimal register allocation
- [ ] Instruction selection over DAG
- [ ] Superblock instruction scheduling

#### Utility
- [ ] Machine code analyzer

### Checklist

- [x] Create the project
- [ ] Finish the parser
- [ ] Definition of IR
- [ ] Finish the pass manager

