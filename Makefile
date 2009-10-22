HS_SOURCES=\
	hs/DBus/Address.hs \
	hs/DBus/Bus.hs \
	hs/DBus/Connection.hs \
	hs/DBus/Constants.hs \
	hs/DBus/Introspection.hs \
	hs/DBus/Message.hs \
	hs/DBus/Types.hs \
	hs/DBus/Util.hs \
	hs/DBus/Wire.hs \
	hs/Tests.hs

all: $(HS_SOURCES)

%.tex: %.nw
	noweave -delay "$<" | cpif "$@"

hs/%.hs: dbus-core.nw hs/DBus
	notangle -R"$*.hs" "$<" | cpphs --hashes --noline | cpif "$@"

hs/Tests.hs: Tests.nw hs/DBus
	notangle -R"Tests.hs" "$<" dbus-core.nw | cpphs --hashes --noline | cpif "$@"

hs/DBus: hs
	mkdir -p hs/DBus

hs:
	mkdir -p hs

dbus-core.pdf: dbus-core.tex
	xelatex dbus-core.tex
	xelatex dbus-core.tex

pdf: dbus-core.pdf
