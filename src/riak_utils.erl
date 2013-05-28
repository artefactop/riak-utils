-module(riak_utils).

-export([
    dict_to_json/1,
    json_to_dict/1,
    date_to_integer_iso8601/1,
    datetime_to_integer_iso8601/1,
    copy_secundary_indexes/2
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

-spec copy_secundary_indexes(Origin::riakc:riak_obj(), Destination::riakc:riak_obj()) -> riakc:riak_obj().

copy_secundary_indexes(undefined, Destination) ->
	Destination;
copy_secundary_indexes(Origin, Destination) ->
	MD = riakc_obj:get_update_metadata(Origin),
    Indexes = riakc_obj:get_secondary_indexes(MD),
    MD1 = riakc_obj:set_secondary_index(MD, Indexes),
	riakc_obj:update_metadata(Destination, MD1).