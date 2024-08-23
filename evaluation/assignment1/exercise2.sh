#!/bin/bash

cluster_path="../cluster.py"
python_cmd="python3"
sample_solution="Assn1Solution.scala"

files=()
for i in {1..38}; do
  file="Assn1-2023-${i}.scala"
  files+=("$file")
done

# Assn1-2023-17.scala: Floating-point literal 0.5
wrong=()
# double,for,for
unsupported=(Assn1-2023-17.scala Assn1-2023-38.scala Assn1-2023-24.scala)
# These assignments will not terminate
# unsafe=(Assn1-2023-6.scala Assn1-2023-14.scala Assn1-2023-15.scala Assn1-2023-19.scala Assn1-2023-21.scala Assn1-2023-25.scala Assn1-2023-36.scala)
unsafe=()

exclude=()
exclude+=("${wrong[@]}")
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

IFS=','
joined_files="${filtered_files[*]}"
unset IFS

time $python_cmd $cluster_path --filenames=$sample_solution,$joined_files --extract=sum --assn1=true  --output=exercise2.json
