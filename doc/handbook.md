% erl-ksuid

# Introduction
The erl-ksuid project provides functions to manipulate KSUID as described in
the [reference implementation](https://github.com/segmentio/ksuid).

KSUID, for "K-Sortable Unique IDentifier", are 20 byte globally unique
identifiers with various interesting properties.

Compared to incremental integers, KSUID provide (reasonable) unicity without
coordination around a single counter and protect against identifier discovery.

Compared to [UUID v4](https://www.ietf.org/rfc/rfc4122.txt), KSUID are loosely
time ordered (which is useful for resource pagination), and both their textual
and binary representations are lexicographically sortable.

Compared to [ULID](https://github.com/ulid/spec), KSUID do not require a
state.

# Implementation
KSUID generation uses
[`erlang:system_time/1`](https://erlang.org/doc/man/erlang.html#system_time-1)
to obtain the timestamp part, and
[`crypto:strong_rand_bytes/1`](https://erlang.org/doc/man/crypto.html#strong_rand_bytes-1)
for the high quality random data required by the random part.

# API
## KSUID values
The `ksuid/0` and `ksuid_binary/0` types are used to represent the textual
representation and binary representation of KSUID values. Both are based on
Erlang binaries.

## Generation
The `ksuid:generate/0` and `ksuid:generate_binary/0` functions generate a
random KSUID.

# Validation
The `ksuid:is_valid/1` function is used to check the validity of a textual
KSUID. The textual representation of a KSUID is valid if it a string of 27
characters, each one being either an ASCII letter or digit.

The `ksuid:is_valid_binary/1` function is used to check the validity of a
binary KSUID. The binary representation of a KSUID is valid if it is a 20 byte
long binary.

# Parsing
The `ksuid:parse/1` function is used to obtain the binary representation of a
textual KSUID. It returns `{ok, KSUID}` if the parameter is a valid textual
KSUID or `{error, Reason}` if it is not.

# Formatting
The `ksuid:format/1` function returns the textual representation of a binary
KSUID.
