# Pipelined GCN-Accelerator
- Computes sparse-dense matrix multiplication in a pipelined manner
- Decouples access execute but dependency checking not implemented in the hardware
- Core executes 5 loads and then a compute instruction and a store instruction
- Global output scratchpad with output value queue for each PE
## Todo
- masked writes to and from scratchpad
- Tiling
- dependence checking

## Configuration
- **Compression:** CSR
- **Loop Order :** K-> N -> M for computation of O[K][N] += I[K][M] * W[M][N]

## Overall Structure
![alt text](https://github.com/GandhamSanjay/GCN-Accelerator/blob/main/GCN.png)
