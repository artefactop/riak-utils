-module(riak_utils).

-export([
    dict_to_json/1,
    json_to_dict/1,
    date_to_integer_iso8601/1,
    datetime_to_integer_iso8601/1,
]).

dict_to_json(Dict) when is_tuple(Dict) ->
    jsx:encode(dict:to_list(Dict)).

json_to_dict(Json) when is_binary(Json) ->
    dict:from_list(jsx:decode(Json)).

date_to_integer_iso8601({date,{Y,M,D}}) ->
    Y * 10000000000 + M * 100000000 + D * 1000000.

datetime_to_integer_iso8601({datetime,{{Y,M,D},{H,Min,S}}}) ->
    Y * 10000000000 + M * 100000000 + D * 1000000 + H * 10000 + Min * 100 + S.