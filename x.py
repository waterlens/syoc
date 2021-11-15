#! /usr/bin/env python
import os
import subprocess
import re

def main():
    if not os.path.exists('./build/syoc'):
        print('Error: ./build/syoc not found')
        return
    if not os.path.isdir('./Test/'):
        print('Error: ./Test/ not found')
        return
    for root, dirs, files in os.walk('./Test/'):
        for name in sorted(files):
            if name.endswith('.sy'):
                name = os.path.join(root, name)
                print('Running test: ' + name)
                if subprocess.run(['./build/syoc', name], stdout=subprocess.DEVNULL).returncode != 0:
                    print(f'Error: {name}')
                    open('current.txt', 'wb').write(name.encode())
                    return

if __name__ == "__main__":
    main()