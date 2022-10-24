from numpy import binary_repr
import numpy as np
import logging
import random

class inst:
    def __init__(self):
        logging.debug("Instruction generator created")
        
### Generates load instructions for GCN-Accelerator

    def load(self,  dep = '0000', id = 'col', sram_offset = 0, dram_offset = 0, xsize = 0, ysize = 0):
        idMap = {'col': '000','row': '001', 'val': '010', 'den': '011', 'out': '100'}
        op = '00'
        logging.debug(f"Creating load with \ndep = {dep}\nid = {id}\nsram offset = {sram_offset}\ndram offset = {dram_offset}\
        \nxsize = {xsize}\nysize = {ysize}")
        inst = op[::-1] + dep[::-1] + idMap.get(id)[::-1] + binary_repr(dram_offset, 32)[::-1]+ binary_repr(sram_offset, 16)[::-1]  
        logging.debug(f"inst[63:0] = {inst}")
        inst =  inst + binary_repr(xsize, 7)[::-1] + binary_repr(ysize, 0)[::-1]
        inst = inst + '0'*(128-len(inst))
        logging.debug(f"inst[127:0] = {inst}")
        return inst

    def spMM(self,  dep = '0000', sram_offset_col = 0, sram_offset_ptr = 0, sram_offset_den = 0, xsize = 0, ysize = 0):
        op = '10'
        logging.debug(f"Creating load with \ndep = {dep}\nid = {id}\nsram_offset_col = {sram_offset_col}\nsram_offset_ptr = {sram_offset_ptr}\
        \nsram_offset_den = {sram_offset_den}\nxsize = {xsize}\nysize = {ysize}")
        inst = op[::-1] + dep[::-1] + binary_repr(sram_offset_col, 16)[::-1] + binary_repr(sram_offset_ptr, 16)[::-1] + binary_repr(sram_offset_den, 16)[::-1]
        inst =  inst + binary_repr(xsize, 7)[::-1] + binary_repr(ysize, 7)[::-1]
        inst = inst + '0'*(128-len(inst))
        logging.debug(f"inst[127:0] = {inst}")
        return inst

    def randLoad(self, count = 1):
        instSeq = ""
        for num in range(count):
            instSeq = instSeq + "000000000"
            for i in range (119):
                instSeq = instSeq + str(random.randint(0,1))
        return instSeq
    
    def randSpMM(self, count = 1):
        instSeq = ""
        for num in range(count):
            instSeq = instSeq + "010000000"
            for i in range (119):
                instSeq = instSeq + str(random.randint(0,1))
        return instSeq


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
dataGen = data()
rowBin = dataGen.arrayToBinary(row)
colBin = dataGen.arrayToBinary(col)
valBin = dataGen.arrayToBinary(val)
denBin = dataGen.matrixToBinary(den)
rowAddr = pow(2,8)
colAddr = 2 * pow(2,8)
valAddr = 3 * pow(2,8)
denAddr = 4 * pow(2,8)
(_,x) = den.shape
(y,_) = I.shape
instGen = inst()
instr = ''
instr = instr + instGen.load(xsize = row.size, id = 'row', dram_offset = rowAddr, sram_offset = 0)
instr = instr + instGen.load(xsize = col.size, id = 'col', dram_offset = colAddr, sram_offset = 0)
instr = instr + instGen.load(xsize = val.size, id = 'val', dram_offset = valAddr, sram_offset = 0)
instr = instr + instGen.load(xsize = den.size, id = 'den', dram_offset = denAddr, sram_offset = 0)
instr = instr + instGen.spMM(ysize = y, xsize = x)
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
f = open('ram.txt','w')
f.write(dram)