#!/bin/bash

files=()
for i in {1..38}; do
  file="Assn1-2023-${i}.scala"
  files+=("$file")
done

# Assn1-2023-1.scala: scala.math.pow. It is a floating-point number, so it is not supported.
unsupported=("Assn1-2023-1.scala")
unsafe=()

exclude=()
exclude+=("${unsupported[@]}")
exclude+=("${unsafe[@]}")

filtered_files=()

for file in "${files[@]}"; do
  skip=false
  for ex in "${exclude[@]}"; do
    if [ "$file" == "$ex" ]; then
      skip=true
      break
    fi
  done
  if [ "$skip" == false ]; then
    filtered_files+=("$file")
  fi
done

IFS=',' # 设置内部字段分隔符为逗号
joined_files="${filtered_files[*]}"
unset IFS # 重置内部字段分隔符

# 打印连接后的字符串
# echo "$joined_files"

time python3 ../cluster.py --filenames=Assn1Solution.scala,$joined_files --extract=p --assn1=true --output=exercise1.json