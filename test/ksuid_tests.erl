%% Copyright (c) 2020-2021 Exograd SAS.
%%
%% Permission to use, copy, modify, and/or distribute this software for any
%% purpose with or without fee is hereby granted, provided that the above
%% copyright notice and this permission notice appear in all copies.
%%
%% THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
%% WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
%% MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
%% SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
%% WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
%% ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR
%% IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

-module(ksuid_tests).

-include_lib("eunit/include/eunit.hrl").

parse_test_() ->
  [?_assertEqual({ok, <<12,82,194,192,81,235,30,45,167,101,
                        37,49,142,237,93,150,108,158,91,122>>},
                 ksuid:parse(<<"1l12i5euax5i7oGDn5DFULPYdCM">>)),
   ?_assertEqual({error, invalid_format},
                 ksuid:parse(<<"">>)),
   ?_assertEqual({error, invalid_format},
                 ksuid:parse(<<"1l12i5euax5i7oGDn5DFULPYdCM2">>)),
   ?_assertEqual({error, {invalid_character, $=}},
                 ksuid:parse(<<"1l12i5euax5i7oGDn5DFULPYdC=">>))].

is_valid_test_() ->
  Ids = [ksuid:generate() || _ <- lists:seq(1, 100)],
  [?_assert(ksuid:is_valid(Id)) || Id <- Ids].

is_valid_binary_test_() ->
  Ids = [ksuid:generate_binary() || _ <- lists:seq(1, 100)],
  [?_assert(ksuid:is_valid_binary(Id)) || Id <- Ids].

format_test_() ->
  Ids = [ksuid:generate_binary() || _ <- lists:seq(1, 100)],
  [?_assertEqual(27, byte_size(ksuid:format(Id))) || Id <- Ids].

format_parse_test_() ->
  Ids = [ksuid:generate_binary() || _ <- lists:seq(1, 100)],
  [?_assertEqual({ok, Id}, ksuid:parse(ksuid:format(Id))) || Id <- Ids].
