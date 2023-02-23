from numpy import binary_repr
import numpy as np
import logging
from inst import inst
from scipy.sparse import csr_matrix

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

    def randSP(self,spDim, denDim, numPEs):
        I = np.random.randint(2, size = spDim)
        print(sum(sum(I)))
        while sum(sum(I))%numPEs != 0 or (sum(sum(I))/numPEs) % denDim[1] != 0:
            for i in range(spDim[0]):
                for j in range(spDim[1]):
                    if sum(sum(I))%numPEs != 0 or (sum(sum(I))/numPEs) % denDim[1] != 0:
                        I[i][j] = 0
                    else:
                        break
                if sum(sum(I))%numPEs == 0 and (sum(sum(I))/numPEs) % denDim[1] == 0:
                    break
        print(sum(sum(I)))
        print("Out of while loop\n")
        print(f"\nSparse matrix is = \n{I}")

        Sparse = csr_matrix(I)
        val = Sparse.data
        print(f"\nVal is =\n {val}")
        print(f"\nSize of val = {val.shape}")
        col = Sparse.indices
        print(f"\nCol_Idx  is = \n{col}")
        row = Sparse.indptr
        print(f"\nRow_ptr is = \n{row}")
        W = np.random.randint(100, size = denDim)
        print(f"\nDense matrix is = \n{W}")
        O = np.matmul(I,W)
        print(f"\nOutput matrix is = \n{O}")
        return ((val,col,row), I, W, O)


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
((val,col,row), I, den, O) = dataGen.randSP(spDim = (8,2), denDim = (2,2), numPEs = 4)
(_,x) = den.shape
(y,_) = I.shape
rowBin = dataGen.arrayToBinary(row)
colBin = dataGen.arrayToBinary(col)
valBin = dataGen.arrayToBinary(val)
denBin = dataGen.matrixToBinary(den)
rowAddr = pow(2,10)
colAddr = 32 * pow(2,10)
valAddr = 1024 * pow(2,10)
denAddr = 2048 * pow(2,10)
outAddr = 3072 * pow(2,10)
instGen = inst()
instr = ''
instr = instr + instGen.load(xsize = row.size, id = 'row', dram_offset = rowAddr, sram_offset = rowAddr)
instr = instr + instGen.load(xsize = col.size, id = 'col', dram_offset = colAddr, sram_offset = colAddr)
instr = instr + instGen.load(xsize = val.size, id = 'val', dram_offset = valAddr, sram_offset = valAddr)
instr = instr + instGen.load(xsize = den.size, id = 'den', dram_offset = denAddr, sram_offset = denAddr)
instr = instr + instGen.spMM(sram_offset_col = colAddr, sram_offset_ptr = rowAddr, sram_offset_den = denAddr, sram_offset_val = valAddr, den_size = den.size, col_size = col.size, row_size = row.size, pr_valid = 1)
instr = instr + instGen.store(xsize = O.size, dram_offset = outAddr, sram_offset = 0)
#instr = instr + instGen.store(xsize = O.size, dram_offset = outAddr, sram_offset = 0)

# Save result matrix
np.savetxt('output_matrix.txt',O)

# Save output metadate
metaDataF = open('metaData.txt','w')
metaDataF.write(str(outAddr)+'\n')
metaDataF.write(str(I.shape[0]) + '\n')
metaDataF.write(str(den.shape[1])+'\n')
metaDataF.close()

print("Result data saved")

# Load result matrix
#infile = open('output_matrix.txt','r')
#O1 = np.loadtxt(infile)
#infile.close()

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
print("Dense matrix padded")
f = open('ram.txt','w')
f.write(dram)