#!/bin/bash

rm -f ./tests/*.ll ./tests/*.bc ./tests/*.j ./tests/*.class &&
make &&
for f in ./tests/*.ins
do
    ff=${f%.ins}
    fff=${ff##*/}

    ./insc_llvm $f && lli ${ff}.bc > ${ff}.out.ll && diff ${ff}.output ${ff}.out.ll && echo "$fff: llvm passed" || echo "$fff: llvm failed"
    ./insc_jvm $f && java -cp ./tests/ $fff > ${ff}.out.j && diff ${ff}.output ${ff}.out.j && echo "$fff: jvm passed" || echo "$fff: jvm failed"
done
