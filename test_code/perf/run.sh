#!/bin/bash

ASSEMBLER="/home/caoyf/code/rust/asm/target/debug/assembler"
LINKER="/home/caoyf/code/rust/asm/target/debug/linker"

# 测量程序执行时间的次数
NUM_RUNS=5

# 遍历所有汇编源文件
for file in ./*.s; do
  echo "-----"  $file "-----"
  # 获取文件名和扩展名
  filename=$(basename "$file")
  extension="${filename##*.}"
  filename="${filename%.*}"

  # 使用gcc生成程序并测量执行时间
  echo "Measuring execution time for $filename with as and ld..."
  as --32 -o "$filename"_gcc.o "$file"
  ld -m elf_i386 -o "$filename"_gcc "$filename"_gcc.o
  total_time_gcc=0
  for (( i=1; i<=$NUM_RUNS; i++ )); do
    time_gcc=$( { time ./"$filename"_gcc >/dev/null; } 2>&1 | grep real | awk '{print $2}' )
    # 从 0mxxxs (0 min xxx sec) 取出 xxx
    time_gcc=$( echo ${time_gcc#0m} )
    time_gcc=$( echo ${time_gcc%s} )
    total_time_gcc=$(echo "$total_time_gcc+$time_gcc" | bc)
    echo "Execution time for $filename with gcc (run $i): $time_gcc seconds"
  done
  avg_time_gcc=$(echo "scale=3; $total_time_gcc/$NUM_RUNS" | bc)
  echo "Average execution time for $filename with as and ld: $avg_time_gcc seconds"

  # 使用aseembler生成程序并测量执行时间
  echo "Measuring execution time for $filename with assembler and linker..."
  "$ASSEMBLER" -o "$filename"_assembler.o "$file"
  "$LINKER" -o "$filename"_assembler "$filename"_assembler.o
  total_time_assembler=0
  for (( i=1; i<=$NUM_RUNS; i++ )); do
    time_assembler=$( { time ./"$filename"_assembler >/dev/null; } 2>&1 | grep real | awk '{print $2}' )
    time_assembler=$( echo ${time_assembler#0m} )
    time_assembler=$( echo ${time_assembler%s} )
    total_time_assembler=$(echo "$total_time_assembler+$time_assembler" | bc)
    echo "Execution time for $filename with assembler (run $i): $time_assembler seconds"
  done
  avg_time_assembler=$(echo "scale=3; $total_time_assembler/$NUM_RUNS" | bc)
  echo "Average execution time for $filename with assembler and linker: $avg_time_assembler seconds"

  # 输出执行时间对比
#   if (( $(echo "$avg_time_gcc < $avg_time_assembler" | bc -l) )); then
#     echo "assembler is faster than gcc by $(echo "$avg_time_gcc-$avg_time_assembler" | bc) seconds."
#   elif (( $(echo "$avg_time_gcc > $avg_time_assembler" | bc -l) )); then
#     echo "gcc is faster than assembler by $(echo "$avg_time_assembler-$avg_time_gcc" | bc) seconds."
#   else
#     echo "assembler and gcc have the same execution time."
#   fi

  echo
done

rm *.o
rm *_assembler
rm *_gcc