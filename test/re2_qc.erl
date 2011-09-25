%% Copyright 2011 Tuncer Ayaz. All Rights Reserved.
%% Use of this source code is governed by a BSD-style
%% license that can be found in the LICENSE file.

-module(re2_qc).

-ifdef(PROPER).
-include_lib("proper/include/proper.hrl").

prop_number() ->
    ?FORALL(Int, non_neg_integer(),
            begin
                N = list_to_binary(integer_to_list(Int)),
                {match, [N]} =:= re2:match(N, "^[0-9]+$")
            end).

prop_byte() ->
    ?FORALL(Byte, byte(),
            match =:= re2:match([Byte], "^.*", [{capture, none}])).

prop_iolist() ->
    ?FORALL(IOList, iolist(),
            match =:= re2:match([IOList], ".*", [{capture, none}])).

%% limit to integers accepted by unicode module
valid_char() -> ?SUCHTHAT(C, char(), C < 16#D800 orelse C > 16#DFFF).
valid_string() -> non_empty(list(valid_char())).

binary_str(Str) -> unicode:characters_to_binary(Str).

prop_char() ->
    ?FORALL(Char, valid_char(),
            begin
                C = unicode:characters_to_binary([Char]),
                match =:= re2:match(C, "^.+$", [{capture, none}])
            end).

prop_string() ->
    ?FORALL(Str, valid_string(),
            begin
                S = binary_str(Str),
                match =:= re2:match(S, "^.+$", [{capture, none}])
            end).

-define(REGEXES,
        [".*", "([0-7]+)[uUlL]*", "[a|b]", "h.*o", "(\\d+):(\\w+)"]).

-define(REGEXOPTS,
        [[{capture, none}],
         [{capture, all_but_first, binary}],
         [{capture, first, index}],
         [{capture, first, binary}],
         [{offset, 1}, {capture, all_but_first, binary}]]).

-define(STRINGS, ["12", "123", "hejsan", "foobar",
                  "proper", "manolis", "kostis"]).

random_re_prop({M0, F0}, {M1, F1}) ->
    ?FORALL({Str,RE,Opts},
            {oneof(?STRINGS), oneof(?REGEXES), oneof(?REGEXOPTS)},
            M0:F0(Str, RE, Opts) =:= M1:F1(Str, RE, Opts)).

prop_random_re2() ->
    random_re_prop({re2, match}, {re2, match}).

prop_random_re_vs_re2() ->
    random_re_prop({re, run}, {re2, match}).

-define(MIN_MEM, 1100).
-define(MAX_MEM, 2 bsl 30 - 1).
sanitize_max_mem(Opts) ->
    lists:map(fun({max_mem, M}) when M < ?MIN_MEM -> {max_mem, ?MIN_MEM};
                 ({max_mem, M}) when M > ?MAX_MEM -> {max_mem, ?MAX_MEM};
                 (E) -> E
              end, Opts).

prop_compile() ->
    ?FORALL({RE, Opts}, {oneof(?REGEXES),
                         ?LET(Opts, list(re2:compile_option()),
                              sanitize_max_mem(Opts))},
            {ok, <<>>} =:= re2:compile(RE, Opts)).

prop_replace() ->
    ?FORALL({Str,RE,Replacement,Opts},
            {?LET(S, oneof(?STRINGS), binary_str(S)),
             oneof(?REGEXES),
             ?LET(S, valid_string(), binary_str(S)),
             list(re2:replace_option())},
            ?IMPLIES(not is_substr(Str, Replacement),
                     begin
                         case re2:match(Str, RE, [{capture, none}]) of
                             match ->
                                 Res = re2:replace(Str, RE, Replacement, Opts),
                                 %% Replacement must be a substring of Str
                                 is_substr(Res, Replacement);
                             nomatch ->
                                 true
                         end
                     end)).

is_substr(Str1, Str2) ->
    nomatch =/= binary:match(Str1, Str2).


valid_match_option(Str) ->
    union(['caseless',
           {'offset',range(1,length(Str))},
           {'capture', union(['all','all_but_first', 'first','none',
                              non_neg_integer(),string(), atom()])},
           {'capture', union(['all','all_but_first','first','none',
                              non_neg_integer(),string(),atom()]),
            union(['index','binary'])}
          ]).
random_re_tuple() ->
    ?LET({Str, RE}, {oneof(?STRINGS), oneof(?REGEXES)},
         ?LET(Opts, list(valid_match_option(Str)),
              {Str, RE, Opts})).
prop_random_re_spec() ->
    ?FORALL({Str,RE,Opts}, random_re_tuple(),
            re2:match(Str, RE, Opts) =:= re2:match(Str, RE, Opts)).

%% TODO: use this when PropEr doesn't fail with ::undef anymore
sanitize_offset(StrLen, Opts) ->
    lists:map(
      fun({offset, Offset}) when Offset > StrLen -> {offset, StrLen};
         (E) -> E
      end, Opts).
random_re_tuple2() ->
    ?LET({Str,RE}, {oneof(?STRINGS), oneof(?REGEXES)},
         ?LET(Opts, list(re2:match_option()),
              {Str, RE, sanitize_offset(length(Str), Opts)})).
prop_random_re_spec2() ->
    ?FORALL({Str,RE,Opts}, ?LET({S,R}, {oneof(?STRINGS), oneof(?REGEXES)},
                                ?LET(O, list(re2:match_option()),
                                     {S, R,
                                      sanitize_offset(length(S), O)})),
            re2:match(Str, RE, Opts) =:= re2:match(Str, RE, Opts)).

-endif.
