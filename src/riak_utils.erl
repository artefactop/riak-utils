-module(riak_utils).

-export([
    dict_to_json/1,
    json_to_dict/1,
    date_to_integer_iso8601/1,
    datetime_to_integer_iso8601/1,
    copy_metadata/2
]).

dict_to_json(Dict) when is_tuple(Dict) ->
    jsx:encode(dict:to_list(Dict)).

json_to_dict(Json) when is_binary(Json) ->
    dict:from_list(jsx:decode(Json)).

-spec date_to_integer_iso8601(Datetime::calendar:date()) -> non_neg_integer().

date_to_integer_iso8601({Y,M,D}) ->
    Y * 10000000000 + M * 100000000 + D * 1000000.

-spec datetime_to_integer_iso8601(Datetime::calendar:datetime()) -> non_neg_integer().

datetime_to_integer_iso8601({datetime, {{Y,M,D},{H,Min,S}}}) ->
    datetime_to_integer_iso8601({{Y,M,D},{H,Min,S}});
datetime_to_integer_iso8601({{Y,M,D},{H,Min,S}}) ->
    Y * 10000000000 + M * 100000000 + D * 1000000 + H * 10000 + Min * 100 + S.

-spec copy_metadata(Origin::riakc:riak_obj(), Destination::riakc:riak_obj()) -> riakc:riak_obj().

copy_metadata(undefined, Destination) ->
	Destination;
copy_metadata(Origin, Destination) ->
	UMD = riakc_obj:get_update_metadata(Origin),
    MD = riakc_obj:get_metadata(Origin),
    NMD = dict:merge(fun(_K, _V1, V2) -> V2 end, MD, UMD),
	riakc_obj:update_metadata(Destination, NMD).