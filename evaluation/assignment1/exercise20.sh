#!/bin/bash

cluster_path="../cluster.py"
python_cmd="python3"
extract_target="election"
sample_solution="Assn1Solution.scala"

files=()
for i in {1..38}; do
  file="Assn1-2023-${i}.scala"
  files+=("$file")
done

unsupported=(Assn1-2023-4.scala Assn1-2023-10.scala Assn1-2023-15.scala Assn1-2023-17.scala Assn1-2023-22.scala Assn1-2023-25.scala Assn1-2023-29.scala Assn1-2023-36.scala)
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

IFS=','
joined_files="${filtered_files[*]}"
unset IFS

# 40m40.263s
time $python_cmd $cluster_path --filenames=$sample_solution,$joined_files --extract=$extract_target --assn1=true --output=exercise20_listmap.json
