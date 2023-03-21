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
        self.nSparseRows = 0
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
        self.nSparseRows = int(metaDataF.readline(),10)
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
        self.doneDelayCounter = 0
        self.doneDelayLength = 10
        self.empty = True
    
    def eval(self, dut):
        empty = True
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
    
    def done(self):
        if(sum(self.entries) == 0):
            if (self.doneDelayCounter >= self.doneDelayLength):
                return True
            else:
                self.doneDelayCounter = self.doneDelayCounter + 1
                return False
        else:
            self.doneDelayCounter = 0
            return False

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
    lowestAddress = 6160000
    tileNumber = 0
    outputTileOffset = 0
    outputMatrixSize = tb.nDenseCols * tb.nSparseRows

    addressOutputCounter = [0]*tb.nSparseRows

    computeStateTimeCumulative = [0]*10
    computeStateTimeTile = [0]*10
    computeStateLookup = ['Idle','Row Pointer','Col','Val','Dense','Partial Sum','Compute','Combine Group','Combine','Done']
    computeTimeFile = open('compute_cycle_data.csv','w')
    computeTimeFile.write('Tile #')
    for state in computeStateLookup:
        computeTimeFile.write(',')
        computeTimeFile.write(state)
    computeTimeFile.write('\n')

    coreStateTimeCumulative = [0]*10
    coreStateTimeTile = [0]*10
    coreStateLookup = ['Idle','Load','Compute','Store','Finish']
    coreTimeFile = open('core_cycle_data.csv', 'w')
    coreTimeFile.write('Tile #')
    coreLoadTime = 0
    for state in coreStateLookup:
        coreTimeFile.write(',')
        coreTimeFile.write(state)
    coreTimeFile.write('\n')

    queueMonitor = QueueEntryMonitor(tb.nPEs)

    # for i in range(64000):
    while dut.core.state != 4 or (not queueMonitor.done()):
        await RisingEdge(dut.core_clock)

        queueMonitor.eval(dut)
        #print(str(eval('dut.core.compute.groupArray_0.clock')))

        # Update the tile number and address offset
        if (dut.core.compute.start == True and dut.core.state == 2):
            outputTileOffset = tileNumber * outputMatrixSize * 4
            print('New outputTileOffset = ' + str(outputTileOffset))
            if (tileNumber > 0):

                # Print missing outputs from this tile, if any exist
                if (numWritten != tb.nSparseRows*tileNumber):
                    print("Tile #" + str(tileNumber) + " failed to output the following rows:\n")
                    for i in range(len(addressOutputCounter)):
                        if (addressOutputCounter[i] != tileNumber):
                            print(str(i*32)+'\n')

                # Output tile compute state cycles
                computeTimeFile.write(str(tileNumber))
                for i in range(len(computeStateLookup)):
                    computeTimeFile.write(',')
                    computeTimeFile.write(str(computeStateTimeTile[i]))
                computeTimeFile.write('\n')

                # Output tile core state cycles
                coreTimeFile.write(str(tileNumber))
                for i in range(len(coreStateLookup)):
                    coreTimeFile.write(',')
                    if i != 1:
                        coreTimeFile.write(str(coreStateTimeTile[i]))
                    else:
                        coreTimeFile.write(str(coreLoadTime))
                coreTimeFile.write('\n')

            tileNumber = tileNumber + 1

            coreLoadTime = coreStateTimeTile[1]

            # Reset compute tile state cycles
            computeStateTimeTile = [0]*10
            # Reset core tile state cycles
            coreStateTimeTile = [0]*10


        # Monitor Output Scratchpad
        if (dut.core.outputScratchpad_io_writeEn.value):
            addressOutputCounter[int(int(dut.core.outputScratchpad_io_spWrite_addr.value)/32)] = addressOutputCounter[int(int(dut.core.outputScratchpad_io_spWrite_addr.value)/32)] + 1
            numWritten = numWritten + 1
            tb.memory.seek(dut.core.outputScratchpad_io_spWrite_addr.value + tb.outputPtr + outputTileOffset)
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
                    # lowestAddress = int(dut.core.outputScratchpad_io_spWrite_addr.value)
            assert(bitStr == str(dut.core.outputScratchpad_io_spWrite_data.value))

        # Monitor Compute State
        computeStateTimeTile[int(dut.core.compute.state)] = computeStateTimeTile[int(dut.core.compute.state)] + 1
        computeStateTimeCumulative[int(dut.core.compute.state)] = computeStateTimeCumulative[int(dut.core.compute.state)] + 1

        # Monitor Core State
        coreStateTimeTile[int(dut.core.state)] = coreStateTimeTile[int(dut.core.state)] + 1
        coreStateTimeCumulative[int(dut.core.state)] = coreStateTimeCumulative[int(dut.core.state)] + 1

    #print("There were " + str(errorCount) + " invalid outputs\n")

    # Output final tile compute state cycles
    computeTimeFile.write(str(tileNumber))
    for i in range(len(computeStateLookup)):
        computeTimeFile.write(',')
        computeTimeFile.write(str(computeStateTimeTile[i]))
    computeTimeFile.write('\n')

    # Output cumulative compute state cycles
    computeTimeFile.write('Total')
    for i in range(len(computeStateLookup)):
        computeTimeFile.write(',')
        computeTimeFile.write(str(computeStateTimeCumulative[i]))
    computeTimeFile.write('\n')
    computeTimeFile.close()

    # Output final tile core state cycles
    coreTimeFile.write(str(tileNumber))
    for i in range(len(coreStateLookup)):
        coreTimeFile.write(',')
        if i != 1:
            coreTimeFile.write(str(coreStateTimeTile[i]))
        else:
            coreTimeFile.write(str(coreLoadTime))
    coreTimeFile.write('\n')

    # Output cumulative core state cycles
    coreTimeFile.write('Total')
    for i in range(len(coreStateLookup)):
        coreTimeFile.write(',')
        coreTimeFile.write(str(coreStateTimeCumulative[i]))
    coreTimeFile.write('\n')
    coreTimeFile.close()

    print(queueMonitor.report())
# 
    # print("First address to error was: " + str(lowestAddress))
    if (numWritten != tb.nSparseRows*tileNumber):
        print(str(numWritten) + " rows were written. The output should have had " + str(tb.nSparseRows*tileNumber) + " rows\n")
        print("Tile #" + str(tileNumber) + " failed to output the following rows:")
        for i in range(len(addressOutputCounter)):
            if (addressOutputCounter[i] != tileNumber):
                print(str(i*32))
        print('\n')

    assert(numWritten == tb.nSparseRows*tileNumber)


    #await Timer(10, units='us')