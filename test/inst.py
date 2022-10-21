from numpy import binary_repr
import logging
import random

logging.basicConfig(level=logging.DEBUG)

class inst:
    def __init__(self):
        logging.debug("Instruction generator created")
        
### Generates load instructions for GCN-Accelerator

    def load(self,  dep = '0000', id = 'col', sram_offset = 0, dram_offset = 0, xsize = 0, ysize = 0):
        idMap = {'col': '000','idx': '001', 'val': '010', 'den': '011', 'out': '100'}
        op = '00'
        logging.debug(f"Creating load with \ndep = {dep}\nid = {id}\nsram offset = {sram_offset}\ndram offset = {dram_offset}\
        \nxsize = {xsize}\nysize = {ysize}")
        inst = op[::-1] + dep[::-1] + idMap.get(id)[::-1] + binary_repr(dram_offset, 32)[::-1]+ binary_repr(sram_offset, 16)[::-1]  
        logging.debug(f"inst[63:0] = {inst}")
        inst =  inst + binary_repr(xsize, 7)[::-1] + binary_repr(ysize, 0)[::-1]
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
    
    def randSpmm(self, count = 1):
        instSeq = ""
        for num in range(count):
            instSeq = instSeq + "010000000"
            for i in range (119):
                instSeq = instSeq + str(random.randint(0,1))
        return instSeq
        
### Create txt file to be loaded into the memory for vta. 
     
f = open('ram.txt','w')
instGen = inst()
f.write(instGen.load(xsize = 0))
f.write(instGen.randSpmm())
f.write(instGen.load(xsize = 0))
f.write(instGen.randSpmm())
# for I in range(4):
#     f.write(instGen.load(xsize = 64))
