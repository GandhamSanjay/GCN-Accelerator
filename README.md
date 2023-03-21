# GCN-Accelerator For DAC
- Uses resources efficiently
- Balances workload perfectly
## Configuration
- **Compression:** CSR
- **Loop Order :** K-> N -> M for computation of O[K][N] += I[K][M] * W[M][N]

## Overall Structure
![alt text](https://github.com/GandhamSanjay/GCN-Accelerator/blob/DACGCN/GCN.png)

## ISA
<br>
1. Load

| Bits | Name | Bit Index | Description | 
| --- | --- | --- | --- |
|2 |opcode |[1:0] |0 – Load|
|3 |Load Type |[4:2] |0 - Column Array<br>1 - Row Array<br>2 - Value Array<br>3 - Dense Matrix |
|64 |DRAM Offset |[68:5] |DRAM offset of the data to load |
|26 |SRAM Offset |[94:69] |SRAM offset where the data will be stored |
|32 | Size |[126:95] | Size of the array to load
|1 |Final Load |[127] | Flag to indicate if this is the final load instruction before the SpMM computation
<br>

2. Store

| Bits | Name | Bit Index | Description | 
| --- | --- | --- | --- |
|2 |opcode |[1:0] |1 – Store|
|3 |Store Type |[4:2] |0 - Column Array<br>1 - Row Array<br>2 - Value Array<br>3 - Dense Matrix<br>4 - Output Matrix<br>5 - Partial Sum Matrix |
|64 |DRAM Offset |[68:5] |DRAM offset where the data will be stored |
|26 |SRAM Offset |[94:69] |SRAM offset of the data to store |
|32 | Size |[126:95] | Size of the array to store
<br>

3. SpMM (CSR) 
 
| Bits | Name | Bit Index | Description | 
| --- | --- | --- | --- |
|2 |opcode |[1:0] |2 – SpMM|
|26 |SRAM Column Offset |[27:2] |SRAM offset of the CSR column array |
|26 |SRAM Pointer Offset |[53:28] |SRAM offset of the CSR row pointer array |
|26 |SRAM Dense Offset|[79:54] |SRAM offset of the dense matrix |
|26 |SRAM Value Offset |[105:80] |SRAM offset of the CSR value array |
|26 |Size (DEN) |[131:106] |Dense matrix size|
|26 |Size (Sparse Values) |[157:132] |Number of nonzero values in the sparse matrix |
|26 |Size (Sparse Rows) |[183:158] |Size of the CSR row pointer |
|26 |Size (Sparse Values) |[157:132] |Number of nonzero values in the sparse matrix |
|2 |Partial row valid |[159:158] |Outdated |
|26 |SRAM Partial Sum Offset |[185:160] |SRAM offset of the partial sum matrix |
|1 |Add partial sum |[186] |Flag to indicate if there is a partial sum that needs to be added |
|1 |Scratchpad or Global Buffer |[187] |Flag to indicate where the partial sum matrix is located, 1 means it is in the output scratchpad, 0 means it is in the global buffer |
|26 |NNZ Per Group |[213:188] |Number of nonzero elements per group |
|1 |Dense loaded |[214] |Flag to indicate if the dense matrix needs to be copied from the global buffer to the local buffers |
