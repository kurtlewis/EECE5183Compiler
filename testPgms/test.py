#! python 3.x
# this is a relatively rigid testing framework for running the compiler against
# test files and verifying their output

# I hope to return to this test runner and make it better, but it's not a
# priority for me

# Intended to be run via `make test` from the project root directory
import os
import subprocess

kjlc = './out/kjlc'

def run_tests():
    fails = 0
    fails += run_correct_tests()
    fails += run_incorrect_tests()
    
    assert fails == 0

def run_correct_tests():
    correct_path = './testPgms/correct'
    files = os.listdir(correct_path)
    fail_count = 0
    # iterate through the tests in the correct directory and check they have
    # no output
    for f in files:
        # run the compile against the tests
        proc = subprocess.run([kjlc, os.path.join(correct_path, f)],
                              stdout=subprocess.PIPE)
        out = proc.stdout.decode('utf-8')
        # check that the file has no output
        if out != '':
            fail_count += 1
            print('Correct program failed to parse: ' + str(f))
            print(out)

    return fail_count

def run_incorrect_tests():
    incorrect_path = './testPgms/incorrect/'
    fail_count = 0
    dirs = os.listdir(incorrect_path)
    for d in dirs:
        if len(d.split('.')):
            continue
        dir_path = os.path.join(incorrect_path, d)
        files = os.listdir(dir_path)
        for f in files:
            split = f.split('.')
            if split[1] == '.out':
                continue
            proc = subprocess.run([kjlc, os.path.join(dir_path, f)],
                                   stdout=subprocess.PIPE)
            out = proc.stdout.decode('utf-8')
            # read the out file
            with open(os.path.join(dir_path, split[0] + '.out')) as f2:
                desired = f2.read()
                if out != desired:
                    fail_count += 1
                    print('Incorrect program had wrong output for: ' + str(f))
                    print(out)

    return fail_count

if __name__ == "__main__":
    run_tests()
