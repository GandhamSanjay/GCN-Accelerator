from numpy import binary_repr
import numpy as np
import logging
from inst import inst
from scipy.sparse import csr_matrix
import math
import random
random.seed()

class data:       
    ### Generates data for GCN-Accelerator
    def __init__(self):
        logging.debug("Data generator created")
    def matrixToBinary(self, A):
        string_A = ''
        for i in A:
            for j in i:
                string_A = string_A + binary_repr(j, 32)[::-1]
        return string_A

    def arrayToBinary(self, A):
        string_A = ''
        for i in A:
                string_A = string_A + binary_repr(i, 32)[::-1]
        return string_A

    def randSP(self,spDim, denDim, numPEs, numSparseValues, colTileNum = 0, nTiles = 1, W=0):
        # I = np.random.randint(4000, size = spDim)
        # for i in range(I.shape[0]):
        #     for j in range(I.shape[1]):
        #         if I[i][j] != 1:
        #             I[i][j] = 0
        # print(sum(sum(I)))
        # while sum(sum(I))%numPEs != 0 or (sum(sum(I))/numPEs) % denDim[1] != 0 or (math.log2(sum(sum(I))/numPEs)) != int(math.log2(sum(sum(I))/numPEs)):
        #     for i in range(spDim[0]):
        #         for j in range(spDim[1]):
        #             if sum(sum(I))%numPEs != 0 or (sum(sum(I))/numPEs) % denDim[1] != 0 or math.log2(sum(sum(I))) != int(math.log2(sum(sum(I)))):
        #                 I[i][j] = 1
        #             else:
        #                 break
        #         if sum(sum(I))%numPEs == 0 and (sum(sum(I))/numPEs) % denDim[1] == 0 and math.log2(sum(sum(I))) == int(math.log2(sum(sum(I)))):
        #             break
        tileCols = int(spDim[1]/nTiles)
        I_padded = np.zeros(shape = spDim, dtype=int)
        I = np.zeros(shape = (spDim[0],tileCols), dtype=int)
        for i in range(numSparseValues):
            v = 1 + random.randrange(1000)
            r = random.randrange(spDim[0])
            c = random.randrange(tileCols)
            while (I[r][c] != 0):
                r = random.randrange(spDim[0])
                c = random.randrange(tileCols)
            I[r][c] = int(v)
            I_padded[r][c + colTileNum*tileCols] = int(v)
        print(sum(sum(I)))
        print("Out of while loop\n")
        print(f"\nSparse matrix is = \n{I}")

        Sparse = csr_matrix(I)
        Sparse_padded = csr_matrix(I_padded)
        val = Sparse.data
        print(f"\nVal is =\n {val}")
        print(f"\nSize of val = {val.shape}")
        col = Sparse_padded.indices
        print(f"\nCol_Idx  is = \n{col}")
        row = Sparse.indptr
        print(f"\nRow_ptr is = \n{row}")
        if colTileNum == 0:
            W = np.random.randint(100, size = denDim)
        print(f"\nDense matrix is = \n{W}")
        O = np.matmul(I_padded,W)
        print(f"\nOutput matrix is = \n{O}")
        np.set_printoptions(formatter={'int':lambda x:hex(int(x))})
        print(f"\nOutput matrix (hex) is = \n{O}")
        return ((val,col,row), I, I_padded, W, O)


np.random.seed(0)
dataGen = data()
I = np.array([[0, 11, 0, 0],
            [22, 0, 0, 33],
            [0, 0, 0, 0],
            [0, 44, 0, 0]])
W = np.array([[2, 2],
            [1, 3],
            [3, 1],
            [1, 2]])
O = np.matmul(I,W)
val = np.array([11,22,33,44])
col = np.array([1,0,3,1])
row = np.array([0,1,3,3,4])
den = np.array([[2, 2],
            [1, 3],
            [3, 1],
            [1, 2]])
nPEs = 4

O = np.loadtxt('output_matrix_0.txt').astype(np.int64)
row = np.loadtxt('rowPtr_tile_0.txt').astype(np.int64)
col = np.loadtxt('colIdx_tile_0.txt').astype(np.int64)
val = np.loadtxt('val_tile_0.txt').astype(np.int64)
den = np.loadtxt('den_tile_0.txt').astype(np.int64)
den1 = np.loadtxt('den_tile_1.txt').astype(np.int64)

k = 0
hex_str = ''
colFile = open('col_data.txt','w')
for i in col:
    if (k == 8):
        #print(str(hex(col[i])) + " ")
        colFile.write(hex_str + '\n')
        k = 0
        hex_str = ''
    hex_str = str(hex(i)) + '\t' + hex_str
    k = k + 1
colFile.close()

rowPtrFile = open('rowPtrs.txt','w')
for i in row:
    rowPtrFile.write(str(i) + '\n')
rowPtrFile.close()

(_,x) = den.shape
y = len(row)-1
rowBin = dataGen.arrayToBinary(row)
colBin = dataGen.arrayToBinary(col)

# k = 0
# bin_str = ''
# colBinStr = ''
# colComparisonFile = open('col_comp_file.txt','w')
# print("Len = " + str(len(colBin)))
# for i in range(int(len(colBin)/32)):
#     if (k == 8):
#         #print(str(hex(col[i])) + " ")
#         colComparisonFile.write(bin_str[::-1] + '\n')
#         colComparisonFile.write(colBinStr + "\n\n")
#         #print(bin_str)
#         #print(colBinStr + "\n\n")
#         k = 0
#         bin_str = ''
#         colBinStr = ''
#     binDigit = str(bin(col[i]))[2:]
#     while (len(binDigit) < 32):
#         binDigit = '0' + binDigit
#     bin_str = binDigit + bin_str
#     colBinStr = colBinStr + colBin[i*32:((i+1)*32)]
#     k = k + 1
# colComparisonFile.close()

print("colBin length = " + str(len(colBin)))
valBin = dataGen.arrayToBinary(val)
denBin = dataGen.matrixToBinary(den)
den1Bin = dataGen.matrixToBinary(den1)
rowAddr = pow(2,10)
colAddr = 1024 * pow(2,10)
valAddr = 4096 * pow(2,10)
denAddr = 8192 * pow(2,10)
den1Addr = 12288 * pow(2,10)
outAddr = 16384 * pow(2,10)
sumAddr = 20480 * pow(2,10)
instGen = inst()
instr = ''
instr = instr + instGen.load(xsize = row.size, id = 'row', dram_offset = rowAddr, sram_offset = rowAddr)
instr = instr + instGen.load(xsize = col.size, id = 'col', dram_offset = colAddr, sram_offset = colAddr)
instr = instr + instGen.load(xsize = val.size, id = 'val', dram_offset = valAddr, sram_offset = valAddr)
instr = instr + instGen.load(xsize = den.size, id = 'den', dram_offset = denAddr, sram_offset = 0, final_load = 0, denGroup = 0)
instr = instr + instGen.load(xsize = den1.size, id = 'den', dram_offset = den1Addr, sram_offset = 0, final_load = 1, denGroup = 1)
# instr = instr + instGen.load(xsize = P.size, id = 'den', dram_offset = sumAddr, sram_offset = sumAddr, final_load = 1)
instr = instr + instGen.spMM(sram_offset_col = colAddr, sram_offset_ptr = rowAddr, sram_offset_den = denAddr, sram_offset_val = valAddr, den_size = den.size, col_size = col.size, row_size = row.size, pr_valid = 1, sram_offset_partial_sum = sumAddr, add_partial_sum = 0, scratchpad_n_global_buffer = 0, nnz_per_group = int(col.size/nPEs), dense_loaded = 1)
instr = instr + instGen.store(xsize = O.size, dram_offset = outAddr, sram_offset = 0)


instCount = len(instr)/256
while(instCount%2 != 0):
    instr = instr + '0'*256
    instCount = len(instr)/256
dram = instr
print("Instruction padded")
while((len(dram)/8) != rowAddr):
    dram = dram + '0'*32
dram = dram + rowBin
print("Padded to Row")
while((len(dram)/8) != colAddr):
    dram = dram + '0'*32
print("Padded to Col")
dram = dram + colBin
while((len(dram)/8) != valAddr):
    dram = dram + '0'*32
dram = dram + valBin
print("Padded to Val")
while((len(dram)/8) != denAddr):
    dram = dram + '0'*32
dram = dram + denBin
while((len(dram)/8) != den1Addr):
    dram = dram + '0'*32
dram = dram + den1Bin
while((len(dram)/8) != sumAddr):
    dram = dram + '0'*32
# dram = dram + sumBin
print("Dense matrix padded")
f = open('ram.txt','w')
f.write(dram)

# Save output metadata
metaDataF = open('metaData.txt','w')
metaDataF.write(str(outAddr)+'\n')      # Where to store the result data for comparison
metaDataF.write(str(len(row)-1) + '\n') # Number of rows in the output matrices
metaDataF.write(str(den.shape[1])+'\n') # Number of columns in the output matrices
metaDataF.write(str(nPEs) +'\n')        # Number of PEs in the accelerator
metaDataF.write('1\n')                  # Number of multiplications
metaDataF.write(str(int(instCount))+'\n')                 # Number of instructions
metaDataF.close()