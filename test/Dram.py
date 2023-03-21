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
nPEs = 16
# ((val,col,row), I, den, O) = dataGen.randSP(spDim = (256, 64), denDim = (64,8), numPEs = nPEs, numSparseValues = 512)
((A_val,A_col,A_row), A, A_padded, den, A_O) = dataGen.randSP(spDim = (2048, 128), denDim = (128,8), numPEs = nPEs, numSparseValues = 5*8*16, colTileNum = 0, nTiles = 2)
((B_val,B_col,B_row), B, B_padded, den, B_O) = dataGen.randSP(spDim = (2048, 128), denDim = (128,8), numPEs = nPEs, numSparseValues = 3*8*16, colTileNum = 1, nTiles = 2, W=den)




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
for i in np.vstack((A_row,B_row)):
    for j in i:
        rowPtrFile.write(str(j) + '\n')
rowPtrFile.close()

(_,x) = den.shape
(y,_) = I.shape
# rowBin = dataGen.arrayToBinary(row)
# colBin = dataGen.arrayToBinary(col)
A_rowBin = dataGen.arrayToBinary(A_row)
B_rowBin = dataGen.arrayToBinary(B_row)
rowBin = A_rowBin + B_rowBin
A_colBin = dataGen.arrayToBinary(A_col)
B_colBin = dataGen.arrayToBinary(B_col)
colBin = A_colBin + B_colBin

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


# Partial sums
P = np.ones(dtype = int, shape = O.shape)
for i in range(P.shape[0]):
    for j in range(P.shape[1]):
        P[i][j] = (i*P.shape[1] + j)*2


# Save result matrix
# O = np.vstack( (O + P, O + (O + P)) )
O = np.vstack((A_O, A_O + B_O))
print(f"\nCombined output matrix (hex) is = \n{O}")
np.savetxt('output_matrix.txt',O)

print("Result data saved")
# Load result matrix
#infile = open('output_matrix.txt','r')
#O1 = np.loadtxt(infile)
#infile.close()


print("colBin length = " + str(len(colBin)))
#valBin = dataGen.arrayToBinary(val)
A_valBin = dataGen.arrayToBinary(A_val)
B_valBin = dataGen.arrayToBinary(B_val)
valBin = A_valBin + B_valBin
denBin = dataGen.matrixToBinary(den)
sumBin = dataGen.matrixToBinary(P)
rowAddr = pow(2,10)
colAddr = 1024 * pow(2,10)
valAddr = 1536 * pow(2,10)
denAddr = 2048 * pow(2,10)
outAddr = 3072 * pow(2,10)
sumAddr = 4096 * pow(2,10)
instGen = inst()
instr = ''
instr = instr + instGen.load(xsize = A_row.size, id = 'row', dram_offset = rowAddr, sram_offset = rowAddr)
instr = instr + instGen.load(xsize = A_col.size, id = 'col', dram_offset = colAddr, sram_offset = colAddr)
instr = instr + instGen.load(xsize = A_val.size, id = 'val', dram_offset = valAddr, sram_offset = valAddr)
instr = instr + instGen.load(xsize = den.size, id = 'den', dram_offset = denAddr, sram_offset = denAddr, final_load = 1)
# instr = instr + instGen.load(xsize = P.size, id = 'den', dram_offset = sumAddr, sram_offset = sumAddr, final_load = 1)
instr = instr + instGen.spMM(sram_offset_col = colAddr, sram_offset_ptr = rowAddr, sram_offset_den = denAddr, sram_offset_val = valAddr, den_size = den.size, col_size = A_col.size, row_size = A_row.size, pr_valid = 1, sram_offset_partial_sum = sumAddr, add_partial_sum = 0, scratchpad_n_global_buffer = 0, nnz_per_group = int(A_col.size/nPEs), dense_loaded = 0)
instr = instr + instGen.store(xsize = O.size, dram_offset = outAddr, sram_offset = 0)
instr = instr + instGen.load(xsize = B_row.size, id = 'row', dram_offset = rowAddr + int(len(A_rowBin)/8), sram_offset = rowAddr)
instr = instr + instGen.load(xsize = B_col.size, id = 'col', dram_offset = colAddr + int(len(A_colBin)/8), sram_offset = colAddr)
instr = instr + instGen.load(xsize = B_val.size, id = 'val', dram_offset = valAddr + int(len(A_valBin)/8), sram_offset = valAddr)
instr = instr + instGen.load(xsize = B_val.size, id = 'val', dram_offset = valAddr + int(len(A_valBin)/8), sram_offset = valAddr, final_load = 1)
#instr = instr + instGen.load(xsize = den.size, id = 'den', dram_offset = denAddr, sram_offset = denAddr)
#instr = instr + instGen.load(xsize = P.size, id = 'den', dram_offset = sumAddr, sram_offset = sumAddr, final_load = 1)
instr = instr + instGen.spMM(sram_offset_col = colAddr, sram_offset_ptr = rowAddr, sram_offset_den = denAddr, sram_offset_val = valAddr, den_size = den.size, col_size = B_col.size, row_size = B_row.size, pr_valid = 1, sram_offset_partial_sum = sumAddr, add_partial_sum = 1, scratchpad_n_global_buffer = 1, nnz_per_group = int(B_col.size/nPEs), dense_loaded = 1)
instr = instr + instGen.store(xsize = O.size, dram_offset = outAddr+512*pow(2,10), sram_offset = 0)

# Save output metadata
metaDataF = open('metaData.txt','w')
metaDataF.write(str(outAddr)+'\n')
metaDataF.write(str(A.shape[0]) + '\n')
metaDataF.write(str(den.shape[1])+'\n')
metaDataF.write(str(nPEs) +'\n')
metaDataF.close()

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
while((len(dram)/8) != sumAddr):
    dram = dram + '0'*32
dram = dram + sumBin
print("Dense matrix padded")
f = open('ram.txt','w')
f.write(dram)