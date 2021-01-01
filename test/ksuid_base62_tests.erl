%% Copyright (c) 2020-2021 Nicolas Martyanoff <khaelin@gmail.com>.
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

-module(ksuid_base62_tests).

-include_lib("eunit/include/eunit.hrl").

encode_test_() ->
  [?_assertEqual(<<"0">>, ksuid_base62:encode(0)),
   ?_assertEqual(<<"1">>, ksuid_base62:encode(1)),
   ?_assertEqual(<<"K">>, ksuid_base62:encode(20)),
   ?_assertEqual(<<"10">>, ksuid_base62:encode(62)),
   ?_assertEqual(<<"100">>, ksuid_base62:encode(3844)),
   ?_assertEqual(<<"3D7">>, ksuid_base62:encode(12345))].

decode_test_() ->
  [?_assertEqual({ok, 0}, ksuid_base62:decode(<<"0">>)),
   ?_assertEqual({ok, 1}, ksuid_base62:decode(<<"1">>)),
   ?_assertEqual({ok, 20}, ksuid_base62:decode(<<"K">>)),
   ?_assertEqual({ok, 62}, ksuid_base62:decode(<<"10">>)),
   ?_assertEqual({ok, 3844}, ksuid_base62:decode(<<"100">>)),
   ?_assertEqual({ok, 12345}, ksuid_base62:decode(<<"3D7">>)),
   ?_assertEqual({error, {invalid_character, $.}},
                 ksuid_base62:decode(<<"a.b">>))].

encode_decode_test_() ->
  Numbers = [rand:uniform((2 bsl 160) - 1) || _ <- lists:seq(1, 100)],
  [?_assertEqual({ok, N},
                 ksuid_base62:decode(ksuid_base62:encode(N))) || N <- Numbers].
