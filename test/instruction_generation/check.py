import sys
from buffer_size import buffer_size
buffSize = buffer_size()

(globalBuff, peBuff) = buffSize.set(spRow=int(sys.argv[1]), spCol=int(sys.argv[2]), denCol=int(sys.argv[3]), nnz=int(sys.argv[4]), nPE=int(sys.argv[5]))
# print("Total = " + str((globalBuff+peBuff)/(2**23)) + ' MB')
# print("Global Buffer = " + str((globalBuff)/(2**23)) + ' MB')
# print("Local Buffers = " + str((peBuff)/(2**23)) + ' MB')
output = buffSize.report_text()
buffSize.create_csv()
buffSize.report_csv()

print(output)