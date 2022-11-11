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

## ISA
1. Mem:Loads/Stores data from/to DRAM to/from scratchpad memory 

|Bits|Name|Bit Index|Description|
| --- | --- | --- | --- |
|2|opcode|[1:0]|0 - Load <br> 1 – Store <br> 2 – SpMM <br> 3 - ? |
|4|Dep Flag|[5:2]|Encodes dependence between load/store/compute modules |
|3|Mem Id|[8:6]|0 – Column <br> 1 – Row Pointer <br> 2 – Value <br> 3 – Dense <br> 4 - Output |
|32|Dram Base|[40:9]|Can access 2^32 bytes = 4 GB of address space |
|16|Sram Base|[56:41] |Each SRAM size = 2^16 bytes = 64 KB |
|7|X Size|[63:57] |Each block is 4 Bytes <br> Each mem access = 64 byte <br> Maximum data transfer per load = 2^7 64-bytes = 8 KB |


2. SpMM (CSR) 
 
| Bits | Name | Bit Index | Description | 
| --- | --- | --- | --- |
|2 |opcode |[1:0] |0 - Load <br> 1 – Store <br> 2 – SpMM <br> 3 - ?|
|4 |Dep flag |[5:2] |Encodes dependence between load/store/compute modules |
|16 |SRAM offset <br> (COL & VAL) |[21:6] |SRAM offset of col_idx and values array |
|16 |SRAM offset <br>(PTR) |[37:22] |SRAM offset of row_ptr array |
|16 |SRAM offset <br>(DEN) |[53:38] |SRAM offset of dense matrix |
|7 |X Size (DEN) |[60:54] |X size of dense matrix|
|7 |Y Size (Sparse) |[67:61] |Y size of sparse matrix (Number of rows) |
 
