import cocotb
from cocotb_bus.drivers.amba import AXI4Slave
from cocotb.clock import Clock
from cocotb.triggers import RisingEdge, Timer
import logging, mmap
from cocotbext.axi import AxiBus, AxiLiteBus, AxiLiteMaster, AxiRam
import numpy as np

class TB(object):
    def __init__(self, dut):
        self.dut = dut
        self.log = logging.getLogger('cocotb_tb')
        self.log.setLevel(logging.WARNING)
        self.axi_master = AxiLiteMaster(AxiLiteBus.from_prefix(self.dut, "s_axi_control"), self.dut.ap_clk)

        # Slave for mutiple outstanding r and single w requests. Can read/write in parallel 
        
        self.memory = mmap.mmap(-1, 2**32)
        self.init_ram()
        self.axi_slave = AXI4Slave(self.dut, "m_axi_gmem", self.dut.ap_clk, self.memory)
        
        # Slave for single outstanding r/w requests. No parallel read/write
         
        # self.axi_ram = AxiRam(AxiBus.from_prefix(self.dut, "m_axi_gmem"), dut.ap_clk, size=2**16)
        # self.init_ram()
        self.outputPtr = 0
        self.nDenseCols = 0
        self.sparseRows = 0
        self.nPEs = 0
        self.load_result_matrix()

	#start the clock as a parallel process.
        cocotb.start_soon(Clock(self.dut.ap_clk, 4, units="ns").start())
        cocotb.start_soon(self.cycle_reset())
	    
    async def cycle_reset(self):
        self.dut.ap_rst_n.setimmediatevalue(0)
        await RisingEdge(self.dut.ap_clk)
        await RisingEdge(self.dut.ap_clk)
        self.dut.ap_rst_n = 0 #This is how cocotb lets you control the value of any signal inside the design
        await RisingEdge(self.dut.ap_clk)
        await RisingEdge(self.dut.ap_clk)
        self.dut.ap_rst_n = 1
        await RisingEdge(self.dut.ap_clk)
        await RisingEdge(self.dut.ap_clk)

    def init_ram(self):
        addr = 0
        f = open("ram.txt","r")
        try:
            str = f.read(8)
            while str != "":
                # print(f"str is {str}\n")
                byte = int(str[::-1],2).to_bytes(1,'little')
                self.memory.seek(addr)
                self.memory.write(byte)
                addr = addr + 1
                str = f.read(8)
        finally:
            f.close()

    def load_result_matrix(self):
        infile = open('output_matrix.txt','r')
        O = np.loadtxt(infile)
        infile.close()
        metaDataF = open('metaData.txt','r')
        self.outputPtr = int(metaDataF.readline(),10)
        self.sparseRows = int(metaDataF.readline(),10)
        self.nDenseCols = int(metaDataF.readline(),10)
        self.nPEs = int(metaDataF.readline(),10)
        metaDataF.close()
        addr = self.outputPtr
        self.memory.seek(addr)
        for row in O:
            for value in np.flip(row):
                self.memory.write(int(value).to_bytes(4,'big'))

    async def launch(self, inst_cnt, b_addr = 0x00000000):
        await Timer(20, units='ns')
        await self.axi_master.write_dwords(0x0004, [b_addr], byteorder = 'little')
        await self.axi_master.write_dwords(0x0008, [inst_cnt], byteorder = 'little')
        await self.axi_master.write_dwords(0x0000, [1], byteorder = 'little')

#@cocotb.test()
#async def verify_output(dut, tb):
#    await RisingEdge(dut.core_clk)
#    if (dut.core.outputScratchpad_io_writeEn and dut.core.outputScratchpad_io_spWrite_addr >= tb.outputPtr):
#        tb.memory.seek(dut.core.outputScratchpad_io_spWrite_addr)
#        numpyVal = tb.memory.read(16)
#        assert(numpyVal == tb.memory.seek)

class QueueEntryMonitor(object):
    def __init__(self, nPEs):
        self.nPEs = nPEs
        self.entries = [0]*self.nPEs
        self.maxEntries = [0]*self.nPEs
    
    def eval(self, dut):
        for i in range(self.nPEs):
            push_val = int(eval('dut.core.compute.groupArray_' + str(i) + '.outBuff.io_enq_valid'))
            pop_val = int(eval('dut.core.compute.groupArray_' + str(i) + '.outBuff.io_deq_ready')) and int(eval('dut.core.compute.groupArray_' + str(i) + '.outBuff.io_deq_valid'))
            self.entries[i] = self.entries[i] + push_val - pop_val
            if (self.entries[i] > self.maxEntries[i]):
                self.maxEntries[i] = self.entries[i]
        
    def report(self):
        results = "Largest number of entries = " + str(max(self.maxEntries)) + '\n'
        for i in range(self.nPEs):
            results = results + 'PE' + str(i) + ' had a maximum of ' + str(self.maxEntries[i]) + ' entries\n'
        return results


@cocotb.test()
async def my_first_test(dut):
    """Try accessing the design."""
    tb = TB(dut)
    await tb.launch(inst_cnt = 12)
    addr = 0x000c

    #cocotb.start(verify_output(dut, tb))
    # while((await tb.axi_master.read_dwords(addr,1))[0] == 0):
    #     await Timer(1, units='ns')
    # print("*********************************Execution Metrics*******************************")
    # addr = addr + 0x0004
    # print(f"Total time = {await tb.axi_master.read_dwords(addr,1)}")
    # addr = addr + 0x0004
    # print(f"Load time = {await tb.axi_master.read_dwords(addr,1)}")
    # addr = addr + 0x0004
    # print(f"Compute time = {await tb.axi_master.read_dwords(addr,1)}")
    # addr = addr + 0x0004
    # print(f"Store time = {await tb.axi_master.read_dwords(addr,1)}")
    # addr = addr + 0x0004
    # for i in range(2):
    #     print(f"D1 time = {await tb.axi_master.read_dwords(addr,1)}")
    #     addr = addr + 0x0004
    #     print(f"D2 time = {await tb.axi_master.read_dwords(addr,1)}")
    #     addr = addr + 0x0004
    #     print(f"MAC time = {await tb.axi_master.read_dwords(addr,1)}")
    #     addr = addr + 0x0004
    #     print(f"PE time = {await tb.axi_master.read_dwords(addr,1)}")
    #     addr = addr + 0x0004

    numWritten = 0
    errorCount = 0    
    lowestAddress = 174848

    queueMonitor = QueueEntryMonitor(tb.nPEs)

    for i in range(25000):
        await RisingEdge(dut.core_clock)

        queueMonitor.eval(dut)
        #print(str(eval('dut.core.compute.groupArray_0.clock')))

        # Monitor Output Scratchpad
        if (dut.core.outputScratchpad_io_writeEn.value):
            numWritten = numWritten + 1
            tb.memory.seek(dut.core.outputScratchpad_io_spWrite_addr.value + tb.outputPtr)
            numpyVals = tb.memory.read(tb.nDenseCols * 4)
            bitStr = ''
            for num in numpyVals:
                bitStr =  bitStr + format(num, '08b')
                #print(format(num, '08b'))
            if bitStr != str(dut.core.outputScratchpad_io_spWrite_data.value):
                print("Address: " + str(int(dut.core.outputScratchpad_io_spWrite_addr.value)))
                print("NumPy:\t" + str(bitStr) + "\nDut:\t"+str(dut.core.outputScratchpad_io_spWrite_data.value))
                errorCount = errorCount + 1
                # if (int(dut.core.outputScratchpad_io_spWrite_addr.value) < lowestAddress):
                #     lowestAddress = int(dut.core.outputScratchpad_io_spWrite_addr.value)
            assert(bitStr == str(dut.core.outputScratchpad_io_spWrite_data.value))

    #print("There were " + str(errorCount) + " invalid outputs\n")

    print(queueMonitor.report())

    # print("First address to error was: " + str(lowestAddress))
    if (numWritten != tb.sparseRows):
        print(str(numWritten) + " rows were written. The output should have had " + str(tb.sparseRows) + " rows\n")
    assert(numWritten == tb.sparseRows)


    #await Timer(10, units='us')