import logging
import random

logging.basicConfig(level=logging.DEBUG)

class buffer_size:
    def __init__(self, spRow=512, spCol=32, denCol=8, nnz=512, nPE=16, blockSize=32):
        #            row ptr    col, val  dense matrix   pSum matrix
        self.globalSize = (spRow+1 +  nnz*2 + spCol*denCol + spRow*denCol) * blockSize
        self.peSize = (nPE * (spRow+1 + spCol*denCol) + nnz*2) * blockSize
        self.spRow = spRow
        self.spCol = spCol
        self.denCol = denCol
        self.nnz = nnz
        self.nPE = nPE
        self.blockSize = blockSize
        self.csvFile = 'none'

    def __del__(self):
        if (self.csvFile != 'none'):
            self.csvFile.close

    # Returns size in bits (if blocksize is set to 32) or bytes (if blocksize is set to 4)
    def set(self, spRow, spCol, denCol, nnz, nPE, blockSize=32):
        #                  row ptr    col, val  dense matrix   pSum matrix
        self.globalSize = (spRow+1 +  nnz*2 + spCol*denCol + spRow*denCol) * blockSize
        self.peSize = (nPE * (spRow+1 + spCol*denCol) + nnz*2) * blockSize
        self.spRow = spRow
        self.spCol = spCol
        self.denCol = denCol
        self.nnz = nnz
        self.nPE = nPE
        self.blockSize = blockSize

        return (self.globalSize, self.peSize)

    def create_csv(self, fileName = 'results.csv'):
        self.csvFile = open(fileName,'w')
        self.csvFile.write('Sparse Rows,Sparse Cols/Dense Rows,Dense Cols/MACs Per PE,Sparse Values,PEs,Block Size,Total Buffer Size (MB),Global Buffer Size (MB),Local Buffer Size (MB)\n')

    def report_csv(self):
        if self.csvFile == 'none':
            print('There is no csv file')
            return
        self.csvFile.write(str(self.spRow)+','+str(self.spCol)+','+str(self.denCol)+','+str(self.nnz)+','+str(self.nPE)+','+str(self.blockSize)+','+str((self.globalSize+self.peSize)/(2**23))+','+str((self.globalSize)/(2**23))+','+str((self.peSize)/(2**23))+'\n')
        
    def report_text(self):
        out = 'Total \t\t\t= ' + str((self.globalSize+self.peSize)/(2**23)) + ' MB\n'
        out = out + 'Global Buffer \t\t= ' + str((self.globalSize)/(2**23)) + ' MB\n'
        out = out + 'Local Buffers \t\t= '+ str((self.peSize)/(2**23)) + ' MB\n'
        out = out + '\nGlobal Breakdown:\n'
        out = out + 'Row Pointer \t\t= ' + str((self.spRow+1)*self.blockSize/(2**13)) + ' kB\n'
        out = out + 'Sparse Columns \t\t= ' + str((self.nnz)*self.blockSize/(2**13)) + ' kB\n'
        out = out + 'Sparse Values \t\t= ' + str((self.nnz)*self.blockSize/(2**13)) + ' kB\n'
        out = out + 'Dense Matrix \t\t= ' + str((self.spCol*self.denCol)*self.blockSize/(2**13)) + ' kB\n'
        out = out + 'Partial Sum Matrix \t= ' + str((self.spRow*self.denCol)*self.blockSize/(2**13)) + ' kB\n'
        out = out + '\nLocal Breakdown:\n'
        out = out + 'Row Pointer \t\t= ' + str((self.spRow+1)*self.blockSize*self.nPE/(2**13)) + ' kB\n'
        out = out + 'Sparse Columns \t\t= ' + str((self.nnz)*self.blockSize/(2**13)) + ' kB\n'
        out = out + 'Sparse Values \t\t= ' + str((self.nnz)*self.blockSize/(2**13)) + ' kB\n'
        out = out + 'Dense Matrix \t\t= ' + str((self.spCol*self.denCol)*self.nPE*self.blockSize/(2**13)) + ' kB\n'
        

        return out