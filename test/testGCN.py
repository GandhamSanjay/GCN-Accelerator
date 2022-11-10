import cocotb
from cocotb_bus.drivers.amba import AXI4Slave
from cocotb.clock import Clock
from cocotb.triggers import RisingEdge, Timer
import logging, mmap
from cocotbext.axi import AxiBus, AxiLiteBus, AxiLiteMaster, AxiRam

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

    async def launch(self, inst_cnt, b_addr = 0x00000000):
        await Timer(20, units='ns')
        await self.axi_master.write_dwords(0x0004, [b_addr], byteorder = 'little')
        await self.axi_master.write_dwords(0x0008, [inst_cnt], byteorder = 'little')
        await self.axi_master.write_dwords(0x0000, [1], byteorder = 'little')

@cocotb.test()
async def my_first_test(dut):
    """Try accessing the design."""
    tb = TB(dut)
    await tb.launch(inst_cnt = 4)
    addr = 0x000c
    while((await tb.axi_master.read_dwords(addr,1))[0] == 0):
        await Timer(1, units='ns')
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
    await Timer(10, units='ns')