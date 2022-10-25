# Proof-of-concept GCN-Accelerator
- Computes sparse-dense matrix multiplication
- Decouples access execute but dependency checking not implemented in the hardware
- Core executes 4 loads and then a compute instruction
- Output buffer/scratchpad and store instruction not supported yet
- Compute PE state machine has minor bugs. Can read row pointer twice and start MAC. Probably doesn't know when to stop the execution
- Supports only 1 PE

## Configuration
- **Compression:** CSR
- **Loop Order :** K-> N -> M for computation of O[K][N] += I[K][M] * W[M][N]

## Overall Structure
![alt text](https://github.com/GandhamSanjay/GCN-Accelerator/blob/main/GCN Acclerator.png)
