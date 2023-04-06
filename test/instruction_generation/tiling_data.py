from buffer_size import buffer_size
import sys
import math

dataset = sys.argv[1]
if len(sys.argv) > 2:
    denCol=int(sys.argv[2])
else:
    denCol=8
if len(sys.argv) > 3:
    nPEs=int(sys.argv[3])
else:
    nPEs=16

# Find max column number
colF = open('sample_graphs/'+dataset+'_colidx.txt','r')
maxCol = 0
numValues = 0
colFlines = colF.readlines()
for line in colFlines:
    if int(line) > maxCol:
        maxCol = int(line)
    numValues = numValues + 1
colF.close()

colF = open('sample_graphs/'+dataset+'_colidx.txt','r')
rowF = open('sample_graphs/'+dataset+'_rowptr.txt','r')
rowPtrEnd =int(rowF.readline())
outF = open(dataset + '_results_nPE_' + str(nPEs)+'_denCol_'+str(denCol)+'.csv','w')
outF.write('Sparse Rows,Sparse Cols,Mean Sparse Values,Max Sparse Values,Min Sparse Values,# of Tiles,Global Buffer Size (MB),Local Buffer Size (MB),Total Buffer Size (MB)\n')
# outF.write('Sparse Rows,Sparse Cols/Dense Rows,Dense Cols/MACs Per PE,Mean Sparse Values,Max Sparse Values,Min Sparse Values\n')
# result = buffer_size()
# result.create_csv(dataset + '_results.csv')

spRow = int(input('Sparse Rows (enter -1 to quit): '))
spCol=0
colidx = 0
buffSize = buffer_size()
while spRow != -1:
    spCol = int(input('Sparse Cols: '))
    # denCol = int(input ('Dense Cols: '))
    # outF.write(str(spRow) + ','+ str(spCol) + ',' + str(denCol)+',')
    outF.write(str(spRow) + ','+ str(spCol) + ',')

    colF = open('sample_graphs/'+dataset+'_colidx.txt','r')
    rowF = open('sample_graphs/'+dataset+'_rowptr.txt','r')
    rowPtrEnd =int(rowF.readline())
    maxCount = 0
    minCount = -1
    meanCount = -1
    meanTiles = 1
    rowCount = 0

    while(rowPtrEnd < numValues):
        colCounter = [0]*math.ceil((maxCol+1)/spCol)

        for i in range(spRow):
            if(rowPtrEnd >= numValues):
                break
            rowPtrEndPrev = rowPtrEnd
            rowPtrEnd = int(rowF.readline())
            rowCount = rowCount + 1
            for j in range(rowPtrEnd-rowPtrEndPrev):
                colidx = int(colF.readline())
                colCounter[int(colidx/spCol)] = colCounter[int(colidx/spCol)] + 1 
        maxCount = max((maxCount,max(colCounter)))

        if minCount == -1:
            minCount = min(colCounter)
        else:
            minCount = min((minCount,min(colCounter)))
    numTiles = math.ceil((maxCol+1)/spCol)*math.ceil(rowCount/spRow)
    outF.write(str(numValues/numTiles)+','+str(maxCount)+','+str(minCount)+','+str(numTiles)+',')
    (globalBuff, peBuff) = buffSize.set(spRow,spCol,denCol,maxCount,nPEs)
    outF.write(str((globalBuff)/(2**23))+','+str((peBuff)/(2**23))+','+str((peBuff+globalBuff)/(2**23))+'\n')
    print(str((globalBuff)/(2**23))+'\t\t'+str((peBuff)/(2**23))+'\t\t'+str((peBuff+globalBuff)/(2**23)))

    spRow = int(input('Sparse Rows (enter -1 to quit): '))

outF.close()