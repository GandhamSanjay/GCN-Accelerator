from numpy import binary_repr
import logging
import random

logging.basicConfig(level=logging.DEBUG)

class inst:
    def __init__(self):
        logging.debug("Instruction generator created")
        
### Generates load instructions for GCN-Accelerator

    def load(self,  dep = '0000', id = 'col', sram_offset = 0, dram_offset = 0, xsize = 0, ysize = 0):
        idMap = {'col': '000','ptr': '001', 'val': '010', 'den': '011', 'out': '100'}
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
        
### Create txt file to be loaded into the memory for vta. 
     
f = open('ram.txt','w')
instGen = inst()
f.write(instGen.load(xsize = 16))
f.write(instGen.load(xsize = 16, id = 'val'))
f.write(instGen.load(xsize = 16, id = 'ptr'))
f.write(instGen.load(xsize = 16, id = 'den'))
f.write(instGen.spMM())
# for I in range(4):
#     f.write(instGen.load(xsize = 64))
