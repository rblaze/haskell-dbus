TEX_SOURCES=tex/DBus/Address.tex \
            tex/DBus/Bus.tex \
            tex/DBus/Connection.tex \
            tex/DBus/Constants.tex \
            tex/DBus/Introspection.tex \
            tex/DBus/Message.tex \
            tex/DBus/Types.tex \
            tex/DBus/Util.tex \
            tex/DBus/Wire.tex \
            tex/Tests.tex \
            dbus-core.tex

tex/%.tex : %.nw
	mkdir -p tex/DBus
	noweave -n "$<" | cpif "$@"

dbus-core.pdf: $(TEX_SOURCES)
	xelatex dbus-core.tex
	xelatex dbus-core.tex
