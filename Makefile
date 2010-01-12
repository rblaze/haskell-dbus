HS_SOURCES=\
	hs/DBus/Address.hs \
	hs/DBus/Authentication.hs \
	hs/DBus/Bus.hs \
	hs/DBus/Connection.hs \
	hs/DBus/Constants.hs \
	hs/DBus/Introspection.hs \
	hs/DBus/MatchRule.hs \
	hs/DBus/Message.hs \
	hs/DBus/Message/Internal.hs \
	hs/DBus/NameReservation.hs \
	hs/DBus/Types.hs \
	hs/DBus/Util.hs \
	hs/DBus/UUID.hs \
	hs/DBus/Wire.hs \
	hs/DBus/Wire/Internal.hs \
	hs/Tests.hs

all: $(HS_SOURCES)

%.tex: %.nw
	noweave -delay "$<" | cpif "$@"

hs/%.hs: dbus-core.nw hs/DBus/Message hs/DBus/Wire
	notangle -R"$*.hs" "$<" | cpphs --hashes --noline | cpif "$@"

hs/Tests.hs: Tests.nw hs
	notangle -R"Tests.hs" "$<" dbus-core.nw | cpphs --hashes --noline | cpif "$@"

hs:
	mkdir -p hs

hs/DBus:
	mkdir -p hs/DBus

hs/DBus/Message:
	mkdir -p hs/DBus/Message

hs/DBus/Wire:
	mkdir -p hs/DBus/Wire

%.pdf: %.tex
	xelatex "$<"
	xelatex "$<"

pdfs: dbus-core.pdf manual.pdf
