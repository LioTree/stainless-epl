#!/bin/bash

cluster_path="../cluster.py"
python_cmd="python3"
extract_target="tyOf"
sample_solution="Assn2Solution_fixed.scala"

files=()
for i in {1..30}; do
  file="Assn2-2023-${i}.scala"
  files+=("$file")
done

wrong=(Assn2-2023-1.scala Assn2-2023-13.scala Assn2-2023-30.scala)
unsupported=(Assn2-2023-10.scala Assn2-2023-18.scala Assn2-2023-20.scala Assn2-2023-22.scala) 
unsafe=(Assn2-2023-7.scala)

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

mkdir -p exercise3_exprs

exprs=("Num" "Plus" "Minus" "Times" "Bool" "Eq" "IfThenElse" "Str" "Length" "Index" "Concat" "Var" "Let" "LetPair" "LetFun" "LetRec" "Pair" "First" "Second" "Lambda" "Rec" "Apply")
for expr in "${exprs[@]}"; do
  $python_cmd $cluster_path --filenames=$sample_solution,$joined_files --extract=$extract_target --assn2=true --gen-subfns=true --compare=tyOf_$expr --output=exercise3_exprs/exercise3_$expr.json
done
