import numpy as np
from scipy.sparse import csr_matrix
import random
random.seed()

name = input("Matrix Name: ")
rows = int(input("Rows: "))
cols = int(input("Columns: "))
print(str(rows*cols))
density = float(input("Density (%): "))
numSparseValues = int(density/100 * rows * cols + 0.5)

if density > 99.9:
    rowF = open('sample_graphs/'+name+'_rowptr.txt', 'w')
    rowF.write('0\n')
    nnz = cols
    for i in range(rows):
        rowF.write(str(nnz) + '\n')
        nnz = nnz + cols
    rowF.close()

    colF = open('sample_graphs/'+name+'_colidx.txt', 'w')
    for i in range(rows):
        for j in range(cols):
            colF.write(str(j) + '\n')
    colF.close()
elif rows*cols > 200000000:
    colF = open('sample_graphs/'+name+'_colidx.txt', 'w')
    rowF = open('sample_graphs/'+name+'_rowptr.txt', 'w')
    rowF.write('0\n')
    nnzRemaining = numSparseValues
    rowsWritten = 0
    nnzWritten = 0
    for i in range(rows):
        rowsRemaining = rows - i
        nnzRemainingPerRow = int(nnzRemaining / rowsRemaining + 0.5)
        if nnzRemainingPerRow > 0:
            rowSize = random.randrange(4*nnzRemainingPerRow)
        else:
            rowSize = 0
        if rowSize > nnzRemaining:
            rowSize = nnzRemaining
        nnzRemaining = nnzRemaining - rowSize
        row = set()
        while len(row) < rowSize:
            row.add(random.randrange(cols))
        nnzWritten = nnzWritten + rowSize
        rowF.write(str(nnzWritten)+'\n')
        for c in row:
            colF.write(str(c)+'\n')
        
    colF.close()
    rowF.close()
else:
    I = np.zeros(shape = (rows,cols), dtype=int)
    for i in range(numSparseValues):
        v = 1
        r = random.randrange(rows)
        c = random.randrange(cols)
        if (i == 0):
            c = cols-1
        while (I[r][c] != 0):
            r = random.randrange(rows)
            c = random.randrange(cols)
        I[r][c] = int(v)
    Sparse = csr_matrix(I)
    np.savetxt('sample_graphs/'+name+'_colidx.txt',Sparse.indices.astype(int),fmt = '%i')
    np.savetxt('sample_graphs/'+name+'_rowptr.txt',Sparse.indptr.astype(int),fmt = '%i')
    