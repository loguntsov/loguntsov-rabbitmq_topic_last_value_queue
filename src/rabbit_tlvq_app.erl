-module(rabbit_tlvq_app).

-behaviour(application).
-export([
    start/2,
    prep_stop/1,
    stop/1
]).

-export([
    app/0,
    set_env/2, get_env/1
]).

-define(APP, rabbit_topic_last_value_queue).

start(_, []) ->
    %%rabbit_topic_last_value_queue_1:enable(),
    rabbit_tlvq_sup:start_link().

prep_stop(State) ->
    rabbit_topic_last_value_queue_1:disable(),
    rabbit_log:info("Last value queue is disabled"),
    State.

stop(_State) ->
    ok.

app() ->
    ?APP.

set_env(Key, Value) ->
    application:set_env(?APP, Key, Value).

get_env(Key) ->
    { ok, Value } = application:get_env(?APP, Key),
    Value.

