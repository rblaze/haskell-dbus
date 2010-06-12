The source code for "dbus-core" is literate. To build the library from scratch,
install the "anansi" application and then run:

    anansi -o hs/ src/dbus-core.anansi

To generate the woven PDF, run:

    anansi -w -o dbus-core.tex src/dbus-core.anansi
    xelatex dbus-core.tex
    xelatex dbus-core.tex
