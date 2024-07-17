import json
import subprocess
import argparse
import os

def parse_args():
    parser = argparse.ArgumentParser(description="Run stainless-dotty with various parameters.")
    parser.add_argument('--filenames', type=str, required=True, help='Comma-separated list of filenames')
    parser.add_argument('--assn2', action='store_true', help='Boolean flag for assn2')
    parser.add_argument('--externpure', type=str, help='Comma-separated list of externpure')
    parser.add_argument('--extract', type=str, required=True, help='Comma-separated list of extract')
    parser.add_argument('--pubclasses', type=str, help='Comma-separated list of pubclasses')
    parser.add_argument('--pubclasses-pn', type=str, help='String for pubclasses-pn')
    parser.add_argument('--comparefuns', type=str, required=True, help='Comma-separated list of comparefuns')
    parser.add_argument('--models', type=str, required=True, help='Comma-separated list of models')
    parser.add_argument('--output', type=str, default='result.json', help='Output JSON file name')
    return parser.parse_args()

def run_dotty(filenames, params):
    command = ['./frontends/dotty/target/universal/stage/bin/stainless-dotty']
    command.extend(filenames.split(','))
    for key, value in params.items():
        if value:
            if isinstance(value, bool):
                command.append(f'--{key}={str(value).lower()}')
            else:
                command.append(f'--{key}={value}')
    command.append('--equivchk=true')
    command.append('--transformation=true')
    command.append('--timeout=3')
    command.append('--equivchk-output=temp.json')

    subprocess.run(command)

def read_temp_json():
    with open('temp.json', 'r') as f:
        return json.load(f)

def extract_function_name(full_name):
    """Remove the part of the function name before the first dot."""
    return full_name.split('.', 1)[1] if '.' in full_name else full_name

def update_comparefuns(comparefuns, temp_data):
    comparefuns = set(comparefuns.split(','))
    for item in temp_data.get('unsafe', []):
        comparefuns.discard(extract_function_name(item['function']))
    for item in temp_data.get('equivalent', []):
        functions = [extract_function_name(func) for func in item['functions']]
        comparefuns.difference_update(functions)
    unequivalent_functions = [extract_function_name(item['function']) for item in temp_data.get('unequivalent', [])]
    unknown_equivalence_functions = [extract_function_name(func) for func in temp_data.get('unknownEquivalence', [])]
    unequivalent_functions.extend(unknown_equivalence_functions)
    if unequivalent_functions:
        first_unequivalent = unequivalent_functions[0]
        comparefuns.discard(first_unequivalent)
        return list(comparefuns), first_unequivalent
    return list(comparefuns), None

def main():
    args = parse_args()
    
    filenames = args.filenames
    extract = args.extract
    comparefuns = args.comparefuns
    models = args.models

    required_params = {
        'extract': extract,
        'comparefuns': comparefuns,
        'models': models,
    }

    optional_params = {
        'assn2': args.assn2,
        'externpure': args.externpure,
        'pubclasses': args.pubclasses,
        'pubclasses-pn': args.pubclasses_pn,
    }

    temp_json_results = []
    comparefuns_list = comparefuns.split(',')
    models_list = models.split(',')

    while comparefuns_list:
        models_list = []
        run_dotty(filenames, {**required_params, **optional_params})
        temp_data = read_temp_json()
        temp_json_results.append(temp_data)

        comparefuns_list, first_unequivalent = update_comparefuns(','.join(comparefuns_list), temp_data)

        if first_unequivalent:
            models_list.append(first_unequivalent)

        required_params['comparefuns'] = ','.join(comparefuns_list)
        required_params['models'] = ','.join(models_list)

        print("[*] new comparefuns:", comparefuns_list)
        print("[*] new models:", models_list)

    with open(args.output, 'w') as f:
        json.dump(temp_json_results, f, indent=4)
    os.remove('temp.json')

if __name__ == "__main__":
    main()