from numpy import binary_repr
import logging
import random

logging.basicConfig(level=logging.DEBUG)

class inst:
    def __init__(self):
        logging.debug("Instruction generator created")
        
### Generates load instructions for GCN-Accelerator

    def load(self, id = 'col', sram_offset = 0, dram_offset = 0, xsize = 0, ysize = 0, final_load = 0):
        idMap = {'col': '000','row': '001', 'val': '010', 'den': '011', 'out': '100', 'psum': '101'}
        op = '00'
        inst = op[::-1] + idMap.get(id)[::-1] + binary_repr(dram_offset, 64)[::-1]+ binary_repr(sram_offset, 26)[::-1]  
        inst =  inst + binary_repr(xsize, 32)[::-1] 
        # print("Ysize at bit " + str(len(inst)))
        #inst = inst + binary_repr(ysize, 0)[::-1]
        # print("Final load at bit " + str(len(inst)))
        inst = inst + binary_repr(final_load, 1)
        inst = inst + '0'*(256-len(inst))
        return inst

    def spMM(self, sram_offset_col = 0, sram_offset_ptr = 0, sram_offset_den = 0, sram_offset_val = 0, den_size = 0, col_size = 0, row_size = 0, pr_valid = 0, sram_offset_partial_sum = 0, add_partial_sum = 0, scratchpad_n_global_buffer = 0, pSum_size = 0, dense_loaded = 0):
        op = '10'
        inst = op[::-1] + binary_repr(sram_offset_col, 26)[::-1] + binary_repr(sram_offset_ptr, 26)[::-1] + binary_repr(sram_offset_den, 26)[::-1]
        inst = inst + binary_repr(sram_offset_val, 26)[::-1] + binary_repr(den_size, 26)[::-1] + binary_repr(col_size, 26)[::-1] + binary_repr(row_size, 26)[::-1] + binary_repr(pr_valid, 2)[::-1]
        inst = inst + binary_repr(sram_offset_partial_sum, 26)[::-1] + binary_repr(add_partial_sum, 1)[::-1] + binary_repr(scratchpad_n_global_buffer, 1)[::-1]
        inst = inst + binary_repr(pSum_size, 26)[::-1]
        inst = inst + binary_repr(dense_loaded, 1)[::-1]
        inst = inst + '0'*(256-len(inst))
        return inst

    def randLoad(self, count = 1):
        instSeq = ""
        for num in range(count):
            instSeq = instSeq + "000000000"
            for i in range (119+128):
                instSeq = instSeq + str(random.randint(0,1))
        return instSeq
    
    def randSpMM(self, count = 1):
        instSeq = ""
        for num in range(count):
            instSeq = instSeq + "010000000"
            for i in range (119+128):
                instSeq = instSeq + str(random.randint(0,1))
        return instSeq
    
    def store(self, id = 'out', sram_offset = 0, dram_offset = 0, xsize = 0, ysize = 0):
        idMap = {'col': '000','ptr': '001', 'val': '010', 'den': '011', 'out': '100', 'psum': '101'}
        op = '01'
        inst = op[::-1] + idMap.get(id)[::-1] + binary_repr(dram_offset, 64)[::-1]+ binary_repr(sram_offset, 26)[::-1]  
        inst =  inst + binary_repr(xsize, 32)[::-1] + binary_repr(ysize, 0)[::-1]
        inst = inst + '0'*(256-len(inst))
        return inst