# -*- coding: utf-8 -*-
from argparse import ArgumentParser
import subprocess
from ast import literal_eval
import sys
from tqdm import tqdm


# -*- coding: utf-8 -*-

import subprocess
import sys
from argparse import ArgumentParser

def run_script(arg_value):
    command = ['python', 'ED_score_for_hpc.py', str(arg_value)]
    process = subprocess.Popen(command, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    output, error = process.communicate()
    print(output.decode('utf-8').strip())

if __name__ == "__main__":
    parser = ArgumentParser()
    parser.add_argument('--arg1', type=float, required=True, help='Value to pass to ED_score_for_hpc.py')
    args = parser.parse_args()
    #print(args.arg1)
    #run_script(1)
    run_script(int(args.arg1))
    
