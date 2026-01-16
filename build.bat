del target/*.d81
del target/*.lst
del target/*.lbl
del target/plus4
64tass --cbm-prg -a src\main.asm -l target\main.lbl -L target\main.lst -o target\plus4
cd target
c1541 -format "plus4,01" d81 plus4.d81
c1541 -attach plus4.d81 -write plus4 plus4
c1541 -attach plus4.d81 -write ../src/kernal.318004-05.bin kernal.bin
c1541 -attach plus4.d81 -write ../src/basic.318006-01.bin basic.bin
cd ..