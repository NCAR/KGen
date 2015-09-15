# KGEN Syntax Checker

class State(object):
    recursive = True
    files = []

class Config(object):
    pass

state = State()
config = Config()

def parse_cmdline():
    import os
    import sys
    from utils import module_file_extensions

    args = sys.argv[1:]

    if len(args)<1:
        print 'ERROR: No directory or file is provided in command line.'
        sys.exit(-1)

    for path in args:
        if os.path.basename(path).startswith('.'): continue

        if os.path.isdir(path):
            if state.recursive:
                for root, dirnames, filenames in os.walk(path):
                    for filename in filenames:
                        if os.path.basename(filename).startswith('.'): continue
                        fname, fext = os.path.splitext(filename)
                        if len(fext)>1 and fext.lower() in module_file_extensions:
                            state.files.append(os.path.join(root, filename))
            else:
                for f in os.listdir(path):
                    if os.path.basename(f).startswith('.'): continue
                    fpath = os.path.join(path,f)
                    if os.path.isfile(fpath):
                        fname, fext = os.path.splitext(fpath)
                        if len(fext)>1 and fext.lower() in module_file_extensions:
                            state.files.append(fpath)

        elif os.path.isfile(path):
            if os.path.isfile(path):
                state.files.append(path)
        else:
            raise '%s is not a direcotory nor a file'%path

def check_files():
    from api import parse, walk
    for file in state.files:
        print file
        fsrc  = open(file, 'rb')

        # convert the preprocessed for fparser
        prep = map(lambda l: '!KGEN'+l if l.lstrip().startswith('#') else l, fsrc.readlines())
        fsrc.close()

        # fparse
        tree = parse(''.join(prep), ignore_comments=True, analyze=False, isfree=True, isstrict=False, \
            include_dirs=None, source_only=None )

        # parse f2003
        for stmt, depth in walk(tree, -1):
            stmt.parse_f2003()

def output_result():
    pass

def main():
    parse_cmdline()

    print 'Checking source files'
    check_files()

    print 'Check Result:'
    output_result()

if __name__ == '__main__':
    main()
