import json
import subprocess
import argparse
import os
import re


def parse_args():
    parser = argparse.ArgumentParser(description="Run stainless-dotty with various parameters.")
    parser.add_argument('--filenames', type=str, required=True, help='Comma-separated list of filenames')
    parser.add_argument('--assn1', type=bool, default=False, help='Boolean flag for assn1')
    parser.add_argument('--assn2', type=bool, default=False, help='Boolean flag for assn2')
    parser.add_argument('--subfns-equiv', type=bool, default=False, help='Boolean flag for sub functions equivalence')
    parser.add_argument('--gen-subfns', type=bool, default=False, help='Boolean flag for generating sub functions')
    parser.add_argument('--fake-exs', type=str, required=False, help='Comma-separated list of fake exercises')
    parser.add_argument('--extract', type=str, required=True, help='Comma-separated list of extract')
    parser.add_argument('--compare', type=str, required=False, help='Comma-separated list of compare funs')
    parser.add_argument('--output', type=str, default='result.json', help='Output JSON file name')
    parser.add_argument('--debug', type=bool, default=False, help='Show debug messages of transformation')
    return parser.parse_args()


def generate_comparefuns_models(filenames, extract):
    comparefuns = set()
    models = set()

    # Extract the first element from filenames for models
    if filenames:
        packagename = filenames[0].split('/')[-1].replace('.scala', '')
        models.add(f"{packagename}.{extract}")

    # Extract the remaining elements from filenames for comparefuns
    if len(filenames) > 1:
        comparefuns.update(
            f"{filename.replace('.scala', '').split('/')[-1]}.{extract}"
            for filename in filenames[1:])

    return comparefuns, models


def run_dotty(filenames, params):
    command = [
        os.path.dirname(os.path.abspath(__file__)) + '/frontends/dotty/target/universal/stage/bin/stainless-dotty']
    command.extend(filenames.split(','))
    for key, value in params.items():
        if value:
            if isinstance(value, bool):
                command.append(f'--{key}={str(value).lower()}')
            else:
                command.append(f'--{key}={value}')
    command.append('--equivchk=true')
    command.append('--transformation=true')
    command.append('--timeout=2')
    command.append('--equivchk-output=temp.json')

    print("[*] Running command:", ' '.join(command))
    subprocess.run(command)


def read_json(filename):
    with open(filename, 'r') as f:
        return json.load(f)


def update_comparefuns(comparefuns, models, temp_data):
    models.clear()
    for item in temp_data.get('unsafe', []):
        comparefuns.discard(item['function'])
    for item in temp_data.get('unknownSafety', []):
        comparefuns.discard(item['function'])
    for item in temp_data.get('wrong', []):
        comparefuns.discard(item)
    for item in temp_data.get('equivalent', []):
        functions = [func for func in item['functions']]
        comparefuns.difference_update(functions)

    unequivalent_functions = [item['function'] for item in temp_data.get('unequivalent', [])]
    unknown_functions = [func for func in temp_data.get('unknownEquivalence', [])]
    unequivalent_functions.extend(unknown_functions)
    print("[*] unequivalent or unknown functions:", unequivalent_functions)
    if unequivalent_functions:
        models.add(unequivalent_functions[0])
        comparefuns.discard(unequivalent_functions[0])


def normalize_id(obj):
    if isinstance(obj, dict):
        return {normalize_id(key): normalize_id(value) for key, value in obj.items()}
    elif isinstance(obj, list):
        return [normalize_id(item) for item in obj]
    elif isinstance(obj, str):
        return re.sub(r'\..*\$package\.', '.', obj)
    else:
        return obj


def main():
    args = parse_args()
    if (args.compare):
        comparefuns, models = generate_comparefuns_models(args.filenames.split(','), args.compare)
    else:
        comparefuns, models = generate_comparefuns_models(args.filenames.split(','), args.extract.split(',')[0])
    print("[*] comparefuns:", comparefuns)
    print("[*] models:", models)

    temp_json_results = []

    while comparefuns and models:
        params = {
            'extract': args.extract,
            'comparefuns': ','.join(comparefuns),
            'models': ','.join(models),
            'assn1': args.assn1,
            'assn2': args.assn2,
            'subfns-equiv': args.subfns_equiv,
            'gen-subfns': args.gen_subfns,
        }
        if (args.debug):
            params['debug'] = "transformation"
        if (args.fake_exs):
            params['fake-exs'] = args.fake_exs

        run_dotty(args.filenames, params)
        if (os.path.exists('temp.json')):
            temp_data = normalize_id(read_json('temp.json'))
            if (len(temp_data['equivalent']) == 0):
                temp_data['equivalent'].append({'model': list(models), 'functions': []})
            else:
                temp_data['equivalent'][0]['model'] = list(models)
            temp_json_results.append(temp_data)

            update_comparefuns(comparefuns, models, temp_data)

            print("[*] new comparefuns:", comparefuns)
            print("[*] new models:", models)
            os.remove('temp.json')
        else:
            break

    if models:
        temp_json_results.append({'equivalent': [{"models": list(models), "functions": []}]})

    if comparefuns:
        temp_json_results.append({"equivalent": [{'specific function not found': list(comparefuns)}]})

    with open(args.output, 'w') as f:
        json.dump(temp_json_results, f, indent=4)


if __name__ == "__main__":
    main()
