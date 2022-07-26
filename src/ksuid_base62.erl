%% Copyright (c) 2022 Bryan Frimin <bryan@frimin.fr>.
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

-module(ksuid_base62).

-export([encode/1, decode/1, pow62/1]).

-spec encode(non_neg_integer()) -> binary().
encode(N) ->
  list_to_binary(encode(N, [])).

-spec encode(non_neg_integer(), string()) -> string().
encode(0, []) ->
  "0";
encode(0, Acc) ->
  Acc;
encode(N, Acc) ->
  encode(N div 62, [number_to_character(N rem 62) | Acc]).

-spec decode(binary()) -> {ok, non_neg_integer()} | {error, term()}.
decode(Bin) ->
  String = binary_to_list(Bin),
  decode(String, length(String)-1, 0).

-spec decode(string(), non_neg_integer(), non_neg_integer()) ->
        {ok, non_neg_integer()} | {error, {invalid_character, byte()}}.
decode([], _, Acc) ->
  {ok, Acc};
decode([C | Rest], I, Acc) ->
  case character_to_number(C) of
    {ok, N} ->
      decode(Rest, I-1, Acc + N*pow62(I));
    error ->
      {error, {invalid_character, C}}
  end.

-spec pow62(non_neg_integer()) -> non_neg_integer().
pow62(0) ->
  1;
pow62(N) ->
  62 * pow62(N-1).

-spec number_to_character(0..61) -> byte().
number_to_character(N) when N < 10 ->
  $0 + N;
number_to_character(N) when N < 36 ->
  $A + N - 10;
number_to_character(N) ->
  $a + N - 36.

-spec character_to_number(byte()) -> {ok, 0..61} | error.
character_to_number(C) when C >= $0, C =< $9 ->
  {ok, C - $0};
character_to_number(C) when C >= $A, C =< $Z ->
  {ok, C - $A + 10};
character_to_number(C) when C >= $a, C =< $z ->
  {ok, C - $a + 36};
character_to_number(_) ->
  error.
