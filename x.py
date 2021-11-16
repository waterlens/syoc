#! /usr/bin/env python
import os
import subprocess
import argparse

test_dir = './Test/'
default_syoc_path = './build/syoc'

def check(args):
    if not os.path.exists(default_syoc_path):
        print(f'Error: {default_syoc_path} not found')
        return
    for root, dirs, files in os.walk(test_dir):
        for name in sorted(files):
            if name.endswith('.sy'):
                name = os.path.join(root, name)
                print('Running test: ' + name)
                if subprocess.run([default_syoc_path, name], stdout=subprocess.DEVNULL).returncode != 0:
                    print(f'Error: {name}')
                    open('current.txt', 'wb').write(name.encode())
                    return

def compile(args):
    cc = args.cc
    cc_path = args.path
    if cc_path is None:
        if cc == 'syoc':
            cc_path = default_syoc_path
        elif subprocess.run([cc, '-v'], stderr=subprocess.DEVNULL).returncode == 0:
            cc_path = cc
    
    if not subprocess.run([cc_path, '-v'], stderr=subprocess.DEVNULL).returncode == 0:
        print(f'Error: {cc_path} not found')
        return
    
    if cc == 'syoc':
        cc_args = []
    else:
        cc_args = ['-x', 'c', '-O3', './Test/Runtime/sylib.c', '-include', './Test/Runtime/sylib.h']
    
    for root, dirs, files in os.walk(test_dir):
            for name in sorted(files):
                if name.endswith('.sy'):
                    full = os.path.join(root, name)
                    bin_root = root.replace('Test', f'Test/Binary/{cc}')
                    bin_path = os.path.join(bin_root, name.replace('.sy', ''))
                    if not os.path.isdir(bin_root):
                        os.makedirs(bin_root)
                    new_arg = [cc_path] + cc_args + [full, '-o', bin_path]
                    new_arg_text = ' '.join(new_arg)
                    print(f'Compiling ({cc}): {new_arg_text}')
                    if subprocess.run(new_arg, stdout=subprocess.DEVNULL).returncode != 0:
                        print(f'Error: {new_arg}')
                        open('current.txt', 'wb').write(full.encode())
                        return
    return 

def clean(args):
    if os.path.isfile('./current.txt'):
        os.remove('./current.txt')
    for dir in ['./Test/Binary', './Test/Output']:
        for root, dirs, files in os.walk(dir, topdown=False):
            for name in files:
                os.remove(os.path.join(root, name))
            for name in dirs:
                os.rmdir(os.path.join(root, name))

def filepath(path):
    if os.path.isfile(path):
        return path
    else:
        raise argparse.ArgumentTypeError(f"{path} is not a valid binary")

def main():
    parser = argparse.ArgumentParser(description='SysY Optimizing Compiler Test Script')
    subparser = parser.add_subparsers(dest='command') 
    
    cc =  subparser.add_parser('compile', help='run the testsuite with given compiler and save the results')
    cc.add_argument('cc', choices=['gcc', 'clang', 'syoc'], help='select one compiler')
    cc.add_argument('--path', dest='path', type=filepath, help='path to the compiler')

    subparser.add_parser('clean', help='clean the generated files')
    subparser.add_parser('check', help='check if syoc will crash')

    args = parser.parse_args()

    if args.command == 'compile':
        compile(args)
    elif args.command == 'check':
        check(args)
    elif args.command == 'clean':
        clean(args)

if __name__ == "__main__":
    main()