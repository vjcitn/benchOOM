# benchOOM
benchmarking out-of-memory strategies for R

Purpose is to allow comparison of timings of operations
on NRxNC matrix: wr is write to store, ingFull is ingest
from store, ing1K is ingest 1000 rows.
```
> benchOOM(times=2)
    NR  NC times       meth        wr    ingFull     ing1K    units
1 5000 100     2       hdf5  17.07860   5.084726  6.266323 microsec
2 5000 100     2         ff 189.67263  66.584151  2.393100 microsec
3 5000 100     2     sqlite 125.93066 134.882089 24.409271 microsec
4 5000 100     2 data.table  25.58902   8.639789  4.963109 microsec
5 5000 100     2  bigmemory  29.09477   1.592431  0.756485 microsec
```

