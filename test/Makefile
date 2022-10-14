# Makefile

# defaults
SIM ?= icarus
TOPLEVEL_LANG ?= verilog

#VERILOG_SOURCES += $(PWD)/mux.v
#VERILOG_SOURCES += /home/$(USER)/research/atom/hardware/verilog/Template.v
VERILOG_SOURCES += /home/$(USER)/research/GCN-Accelerator/hardware/verilog/*.v
# use VHDL_SOURCES for VHDL files

# EXTRA_ARGS += --trace --trace-structs

# TOPLEVEL is the name of the toplevel module in your Verilog or VHDL file
TOPLEVEL = Wrapper

# MODULE is the basename of the Python test file
MODULE = testGCN

# include cocotb's make rules to take care of the simulator setup
include $(shell cocotb-config --makefiles)/Makefile.sim
