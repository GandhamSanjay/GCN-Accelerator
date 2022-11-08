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

    def randSP(self,spDim, denDim):
        I = np.random.randint(2, size = spDim)
        print(f"\nSparse matrix is = \n{I}")
        Sparse = csr_matrix(I)
        val = Sparse.data
        print(f"\nVal is =\n {val}")
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
((val,col,row), I, den, O) = dataGen.randSP(spDim = (8,8), denDim = (8,4))
psum = np.zeros(O.shape, dtype =int)
rowBin = dataGen.arrayToBinary(row)
colBin = dataGen.arrayToBinary(col)
valBin = dataGen.arrayToBinary(val)
denBin = dataGen.matrixToBinary(den)
psumBin = dataGen.matrixToBinary(psum)
rowAddr = pow(2,10)
colAddr = 2 * pow(2,10)
valAddr = 3 * pow(2,10)
denAddr = 4 * pow(2,10)
psumAddr = 5 * pow(2,10)
outAddr = 6 * pow(2,10)
(_,x) = den.shape
(y,_) = I.shape
instGen = inst()
instr = ''
instr = instr + instGen.load(xsize = row.size, id = 'row', dram_offset = rowAddr, sram_offset = 0)
instr = instr + instGen.load(xsize = col.size, id = 'col', dram_offset = colAddr, sram_offset = 0)
instr = instr + instGen.load(xsize = val.size, id = 'val', dram_offset = valAddr, sram_offset = 0)
instr = instr + instGen.load(xsize = den.size, id = 'den', dram_offset = denAddr, sram_offset = 0)
instr = instr + instGen.load(xsize = psum.size, id = 'psum', dram_offset = psumAddr, sram_offset = 0)
instr = instr + instGen.spMM(ysize = y, xsize = x)
instr = instr + instGen.store(xsize = O.size, dram_offset = outAddr)
instCount = len(instr)/128
while(instCount%4 != 0):
    instr = instr + '0'*128
    instCount = len(instr)/128
dram = instr
while((len(dram)/8) != rowAddr):
    dram = dram + '0'*32
dram = dram + rowBin
while((len(dram)/8) != colAddr):
    dram = dram + '0'*32
dram = dram + colBin
while((len(dram)/8) != valAddr):
    dram = dram + '0'*32
dram = dram + valBin
while((len(dram)/8) != denAddr):
    dram = dram + '0'*32
dram = dram + denBin
while((len(dram)/8) != psumAddr):
    dram = dram + '0'*32
dram = dram + psumBin
f = open('ram.txt','w')
f.write(dram)