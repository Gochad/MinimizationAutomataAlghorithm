import subprocess

def run_test_case(input_file, expected_output_file):
    command = ['./automat_minimalny', input_file]
    result = subprocess.run(command, stdout=subprocess.PIPE, text=True)

    expected_output = open(expected_output_file).read().strip()
    if result.stdout.strip() == expected_output:
        print(f'Test case {input_file}: Passed')
    else:
        print(f'Test case {input_file}: Failed')

def run_all_test_cases():
    test_cases = [
        # ('1.in', '1.out'),
        ('2.in', '2.out'),
    ]

    for input_file, expected_output_file in test_cases:
        run_test_case(input_file, expected_output_file)

if __name__ == "__main__":
    run_all_test_cases()
