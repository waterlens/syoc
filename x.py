#! /usr/bin/env python
import os
import subprocess
import argparse
import tomli

test_dir = './Test/'
output_dir = './Output/'
binary_dir = './Binary/'
default_syoc_path = './build/syoc'


def get_test_source(name):
    return os.path.join(test_dir, os.path.splitext(name)[0] + ".sy")


def get_test_output(cc, name):
    return os.path.join(output_dir, cc, os.path.splitext(name)[0] + ".out")


def get_test_input(name):
    return os.path.join(test_dir, os.path.splitext(name)[0] + ".in")


def get_test_binary(cc, name):
    return os.path.join(binary_dir, cc, os.path.splitext(name)[0])


def check_test_name_valid(name):
    if not os.path.isfile(get_test_source(name)):
        print(f'Error: not a valid test name: {get_test_source(name)}')
        exit(1)


def check_test_binary_valid(cc, name):
    if not (os.path.isfile(get_test_binary(cc, name)) or os.path.isfile(get_test_binary(cc, name) + '.exe')):
        print(f'Error: not a binary: {get_test_binary(cc, name)}')
        exit(1)


def create_father_dir(path):
    dir = os.path.dirname(path)
    if not os.path.isdir(dir):
        os.makedirs(dir)


def compile_single(cc_path, cc_args, src_path, bin_path):
    args = [cc_path] + cc_args + [src_path, '-o', bin_path]
    if subprocess.run(args, stdout=subprocess.DEVNULL).returncode != 0:
        args_text = ' '.join(args)
        print(f'Error: {args_text}')


def run_single(bin_path, input_path, output_path):
    with open(output_path, 'wb') as out_file:
        if os.path.isfile(input_path):
            in_file = open(input_path, 'rb')
        else:
            in_file = None
        args = [bin_path]
        code = subprocess.run(args, stdin=in_file, stdout=out_file).returncode
        if code != 0:
            args_text = ' '.join(args)
            print(f'Warning ({code}): {args_text}')
        if os.path.isfile(input_path):
            in_file.close()


def get_cc_path(cc, cc_path):
    if cc_path is None:
        if cc == 'syoc':
            cc_path = default_syoc_path
        else:
            try:
                subprocess.run([cc, '-v'], stdout=subprocess.DEVNULL)
            except FileNotFoundError:
                print(f'Error: {cc} not found')
                exit(1)
            return (cc, cc)
    try:
        subprocess.run([cc_path, '-v'], stdout=subprocess.DEVNULL)
    except FileNotFoundError:
        print(f'Error: {cc_path} not found')
        exit(1)
    return (cc, cc_path)


def get_cc_args(cc):
    if cc == 'syoc':
        cc_args = []
    else:
        cc_args = ['-x', 'c', '-O3', './Test/Runtime/sylib.c',
                   '-include', './Test/Runtime/sylib.h']
    return cc_args


def check(args):
    cc, cc_path = get_cc_path('syoc', None)
    if args.test is None:
        for root, dirs, files in sorted(os.walk(test_dir)):
            for name in sorted(files):
                if name.endswith('.sy'):
                    name = os.path.join(root, name)
                    print(f'Checking {name}')
                    result = subprocess.run(
                        [cc_path, name], capture_output=True, text=True)
                    if args.output is True:
                        print(result.stdout)
                    if result.returncode != 0:
                        print(f'Error: {name}')
                        open('current.txt', 'wb').write(name.encode())
                        return
    else:
        for path in args.test:
            check_test_name_valid(path)
            print(f'Checking {path}')
            path = get_test_source(path)
            result = subprocess.run(
                [cc_path, path], capture_output=True, text=True)
            if args.output is True:
                print(result.stdout)
            if result.returncode != 0:
                print(f'Error: {path}')
                open('current.txt', 'wb').write(path.encode())
                return


def run(args):
    cc = args.cc
    if args.test is None:
        for root, dirs, files in sorted(os.walk(test_dir)):
            for name in sorted(files):
                if name.endswith('.sy'):
                    bin_root = root.replace(test_dir, f'{binary_dir}{cc}/')
                    bin_path = os.path.join(bin_root, name.removesuffix('.sy'))
                    if not (os.path.isfile(bin_path) or os.path.isfile(bin_path + '.exe')):
                        continue
                    out_root = root.replace(test_dir, f'{output_dir}{cc}/')
                    out_path = os.path.join(out_root, name.removesuffix('.sy'))
                    in_path = os.path.join(root, name.replace('.sy', '.in'))
                    if not os.path.isdir(out_root):
                        os.makedirs(out_root)
                    print(f'Running ({cc}): {bin_path}')
                    run_single(bin_path, in_path, out_path)
    else:
        for path in args.test:
            check_test_binary_valid(cc, path)
            print(f'Running ({cc}): {path}')
            create_father_dir(get_test_output(cc, path))
            run_single(get_test_binary(cc, path), get_test_input(
                path), get_test_output(cc, path))


def compile(args):
    cc, cc_path = get_cc_path(args.cc, args.path)
    cc_args = get_cc_args(cc)
    if args.flags is not None:
        cc_args += args.flags.split()

    print(f'Flags: {cc_args}')

    if args.test is None:
        for root, dirs, files in sorted(os.walk(test_dir)):
            for name in sorted(files):
                if name.endswith('.sy'):
                    full = os.path.join(root, name)
                    bin_root = root.replace(test_dir, f'{binary_dir}{cc}/')
                    bin_path = os.path.join(bin_root, name.removesuffix('.sy'))
                    if not os.path.isdir(bin_root):
                        os.makedirs(bin_root)
                    print(f'Compiling ({cc}): {full}')
                    compile_single(cc_path, cc_args, full, bin_path)
    else:
        for path in args.test:
            check_test_name_valid(path)
            print(f'Compiling ({cc}): {path}')
            create_father_dir(get_test_binary(cc, path))
            compile_single(cc_path, cc_args, get_test_source(
                path), get_test_binary(cc, path))


def clean(args):
    if os.path.isfile('./current.txt'):
        os.remove('./current.txt')
    for dir in [binary_dir, output_dir]:
        for root, dirs, files in os.walk(dir, topdown=True):
            for name in files:
                os.remove(os.path.join(root, name))
            for name in dirs:
                os.rmdir(os.path.join(root, name))


def filepath(path):
    if os.path.isfile(path):
        return path
    else:
        raise argparse.ArgumentTypeError(f"{path} is not a valid binary")

def deploy(args):
    if os.path.isfile('manifest.txt'):
        with open('manifest.txt', "rb") as f:
            config = tomli.load(f)
            sources = config['sources'].strip('\n').split('\n')
            import shutil
            for source in sources:
                dst = os.path.join("./Deploy", source)
                os.makedirs(os.path.dirname(dst), exist_ok=True)
                shutil.copy2(source, dst)
    else:
        print('Please run CMake configuration first to generate a manifest.txt')

def main():
    parser = argparse.ArgumentParser(
        description='SysY Optimizing Compiler Test Script')
    flags = None

    subparser = parser.add_subparsers(dest='command')

    cc = subparser.add_parser(
        'compile', help='compile the testsuite with given compiler')
    cc.add_argument('cc', choices=['gcc', 'clang',
                    'syoc'], default='clang', help='select one compiler')
    cc.add_argument('--path', dest='path', type=filepath,
                    help='path to the compiler')
    cc.add_argument('--flags', dest=flags, type=str,
                    help='extra compiler flags')
    cc.add_argument('--test', nargs='+',
                    help='the name of the test you want to compile')
    rn = subparser.add_parser('run', help='run the binary')
    rn.add_argument('cc', choices=['gcc', 'clang',
                                   'syoc'], default='clang', help='select one compiler')
    rn.add_argument('--test', nargs='+',
                    help='the name of the test you want to run')
    subparser.add_parser('clean', help='clean the generated files')
    ck = subparser.add_parser('check', help='check if syoc will crash')
    ck.add_argument('--test', nargs='+',
                    help='the name of the test you want to check')
    ck.add_argument('--output', action="store_true")

    dy = subparser.add_parser(
        'deploy', help='generate the files for deployment')

    args = parser.parse_args()

    os.environ['ASAN_OPTIONS'] = 'detect_leaks=0'

    if args.command == 'compile':
        compile(args)
    elif args.command == 'check':
        check(args)
    elif args.command == 'clean':
        clean(args)
    elif args.command == 'run':
        run(args)
    elif args.command == 'deploy':
        deploy(args)


if __name__ == "__main__":
    main()
