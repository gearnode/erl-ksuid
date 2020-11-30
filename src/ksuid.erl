%% Copyright (c) 2020 Nicolas Martyanoff <khaelin@gmail.com>.
%%
%% Permission to use, copy, modify, and/or distribute this software for any
%% purpose with or without fee is hereby granted, provided that the above
%% copyright notice and this permission notice appear in all copies.
%%
%% THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES WITH
%% REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY
%% AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY SPECIAL, DIRECT,
%% INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM
%% LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR
%% OTHER TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR
%% PERFORMANCE OF THIS SOFTWARE.

-module(ksuid).

-export([generate/0, generate_string/0, system_time/1, format/1, parse/1,
         random_data/0, current_timestamp/0,
         system_time_to_timestamp/1, timestamp_to_system_time/1]).

-export_type([ksuid/0, ksuid_string/0]).

-type ksuid() :: <<_:160>>.
-type ksuid_string() :: <<_:216>>.
-type ksuid_timestamp() :: 0..4_294_967_295.

-spec generate() -> ksuid().
generate() ->
  Timestamp = current_timestamp(),
  RandomData = random_data(),
  <<Timestamp:32, RandomData/binary>>.

-spec generate_string() -> ksuid_string().
generate_string() ->
  format(generate()).

-spec system_time(ksuid()) -> integer().
system_time(<<Id:32, _/binary>>) ->
  timestamp_to_system_time(Id).

-spec format(ksuid()) -> ksuid_string().
format(<<Id:160>>) ->
  ksuid_base62:encode(Id).

-spec parse(binary()) -> {ok, ksuid()} | {error, term()}.
parse(Data) when byte_size(Data) =:= 27 ->
  case ksuid_base62:decode(Data) of
    {ok, N} ->
      {ok, <<N:160>>};
    {error, Reason} ->
      {error, Reason}
  end;
parse(_Data) ->
  {error, invalid_format}.

-spec current_timestamp() -> ksuid_timestamp().
current_timestamp() ->
  system_time_to_timestamp(erlang:system_time(second)).

-spec system_time_to_timestamp(integer()) -> ksuid_timestamp().
system_time_to_timestamp(SysTime) ->
  SysTime - epoch().

-spec timestamp_to_system_time(ksuid_timestamp()) -> integer().
timestamp_to_system_time(Timestamp) ->
  Timestamp + epoch().

-spec epoch() -> ksuid_timestamp().
epoch() ->
  1_400_000_000.

-spec random_data() -> <<_:128>>.
random_data() ->
  crypto:strong_rand_bytes(16).
