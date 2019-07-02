"""Copy the Plugin Contents of .vimrc to its own file, so that we can run
:PluginInstall without generating any other errors."""

import sys


def main():

    with open('.vimrc', 'r') as v:
        with open('.pluginrc', 'w') as p:
            for line in v:
                if 'END PLUGINS' in line:
                    sys.exit(0)
                p.write(line)


if __name__=='__main__':
    main()
