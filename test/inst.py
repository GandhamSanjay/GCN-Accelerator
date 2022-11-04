from numpy import binary_repr
import logging
import random

logging.basicConfig(level=logging.DEBUG)

class inst:
    def __init__(self):
        logging.debug("Instruction generator created")
        
### Generates load instructions for GCN-Accelerator

    def load(self,  dep = '0000', id = 'col', sram_offset = 0, dram_offset = 0, xsize = 0, ysize = 0):
        idMap = {'col': '000','row': '001', 'val': '010', 'den': '011', 'out': '100'}
        op = '00'
        inst = op[::-1] + dep[::-1] + idMap.get(id)[::-1] + binary_repr(dram_offset, 32)[::-1]+ binary_repr(sram_offset, 16)[::-1]  
        inst =  inst + binary_repr(xsize, 7)[::-1] + binary_repr(ysize, 0)[::-1]
        inst = inst + '0'*(128-len(inst))
        return inst

    def spMM(self,  dep = '0000', sram_offset_col = 0, sram_offset_ptr = 0, sram_offset_den = 0, xsize = 0, ysize = 0):
        op = '10'
        inst = op[::-1] + dep[::-1] + binary_repr(sram_offset_col, 16)[::-1] + binary_repr(sram_offset_ptr, 16)[::-1] + binary_repr(sram_offset_den, 16)[::-1]
        inst =  inst + binary_repr(xsize, 7)[::-1] + binary_repr(ysize, 7)[::-1]
        inst = inst + '0'*(128-len(inst))
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
    
    def store(self,  dep = '0000', id = 'out', sram_offset = 0, dram_offset = 0, xsize = 0, ysize = 0):
        idMap = {'col': '000','ptr': '001', 'val': '010', 'den': '011', 'out': '100'}
        op = '01'
        inst = op[::-1] + dep[::-1] + idMap.get(id)[::-1] + binary_repr(dram_offset, 32)[::-1]+ binary_repr(sram_offset, 16)[::-1]  
        inst =  inst + binary_repr(xsize, 7)[::-1] + binary_repr(ysize, 0)[::-1]
        inst = inst + '0'*(128-len(inst))
        return inst