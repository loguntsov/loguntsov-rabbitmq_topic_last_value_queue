
-module(ltq_queue).
-author("Sergey Loguntsov <loguntsov@gmail.com>").

-export([
  new/0,
  in/3, in_r/3, join/2,
  out/1, out_r/1,
  take/2, fetch/1, peek/1,
  prev/2, next/2,
  front/1, back/1,
  foldl/3, map/2, while/3,
  to_list/1, from_list/1,
  reverse/1,
  len/1,
  is_empty/1
]).

%% API

% -type time() :: integer().
-type key() :: term().
-type value() :: term().

-record(ltq_item, {
  prev = null :: key() | null,
  next = null :: key() | null,
  value :: value()
}).

-record(ltq_queue, {
  front = null :: key() | null,
  back = null :: key() | null,
  index = #{} :: maps:map(key(), #ltq_item{})
}).

-type queue() :: #ltq_queue{}.

-export_type([
  key/0, value/0, queue/0
]).

new() ->
  #ltq_queue{}.

in(Q = #ltq_queue{ front = null, back = null }, Key, Value) ->
  Item = #ltq_item{
    value = Value
  },
  { ok, Q#ltq_queue{
    index = #{ Key => Item },
    front = Key,
    back = Key
  }};

in(Q, Key, Value) ->
  Index = Q#ltq_queue.index,
  Result = case maps:find(Key, Index) of
    { ok, #ltq_item{} = Item } ->
      {{ duplicate, Item#ltq_item.value}, Q#ltq_queue{
        index = Index#{ Key => Item#ltq_item{ value = Value }}
      }};
    error ->
      Front = Q#ltq_queue.front,
      NewItem = #ltq_item{
        prev = null,
        next = Front,
        value = Value
      },
      { ok, OldItem } = maps:find(Front, Index),
      { ok, Q#ltq_queue{
        index = Index#{
          Key => NewItem,
          Front => OldItem#ltq_item{
            prev = Key
          }
        },
        front = Key
      }}
  end,
  %%rabbit_log:info("In ~p:~p -> ~p", [ Key, Value, Result]),
  Result.

in_r(Q = #ltq_queue{ front = null, back = null }, Key, Value) ->
  Item = #ltq_item{
    value = Value
  },
  { ok, Q#ltq_queue{
    index = #{ Key => Item },
    front = Key,
    back = Key
  }};

in_r(Q, Key, Value) ->
  Index = Q#ltq_queue.index,
  Result = case maps:find(Key, Index) of
    { ok, #ltq_item{} = Item } ->
      {{ duplicate, Item#ltq_item.value}, Q#ltq_queue{
        index = Index#{ Key => Item#ltq_item{ value = Value }}
      }};
    error ->
      Back = Q#ltq_queue.back,
      NewItem = #ltq_item{
        prev = Back,
        next = null,
        value = Value
      },
      { ok, OldItem } = maps:find(Back, Index),
      { ok, Q#ltq_queue{
        index = Index#{
          Key => NewItem,
          Back => OldItem#ltq_item{
            next = Key
          }
        },
        back = Key
      }}
  end,
  %%rabbit_log:info("In_r ~p:~p -> ~p", [ Key, Value, Result]),
  Result.

out(Q = #ltq_queue{ front = null, back = null }) -> { empty, Q };
out(Q = #ltq_queue{ front = Key, back = Key, index = Index }) ->
  { ok, Item} = maps:find(Key, Index),
  {{ Key, Item#ltq_item.value }, Q#ltq_queue{
    front = null, back = null, index = #{}
  }};
out(Q = #ltq_queue{ back = Back, index = Index }) ->
  { ok, Item } = maps:find(Back, Index),
  NewBack = Item#ltq_item.prev,
  { ok, OldItem } = maps:find(NewBack, Index),
  NewIndex = maps:remove(Back, Index),
  NewQ = Q#ltq_queue{
    back = NewBack,
    index = NewIndex#{
      NewBack => OldItem#ltq_item{
        next = null
      }
    }
  },
  {{ Back, Item#ltq_item.value}, NewQ}.

out_r(Q = #ltq_queue{ front = null, back = null }) -> { empty, Q };
out_r(Q = #ltq_queue{ front = Key, back = Key, index = Index }) ->
  { ok, Item} = maps:find(Key, Index),
  {{ Key, Item#ltq_item.value }, Q#ltq_queue{
    front = null, back = null, index = #{}
  }};
out_r(Q = #ltq_queue{ front = Front, index = Index }) ->
  { ok, Item } = maps:find(Front, Index),
  NewFront = Item#ltq_item.next,
  { ok, OldItem } = maps:find(NewFront, Index),
  NewIndex = maps:remove(Front, Index),
  NewQ = Q#ltq_queue{
    front = NewFront,
    index = NewIndex#{
      NewFront => OldItem#ltq_item{
        prev = null
      }
    }
  },
  {{ Front, Item#ltq_item.value}, NewQ}.


take(Q = #ltq_queue{ back = Key}, Key) ->
  case out(Q) of
    empty -> { empty, Q };
    {{ _, Value }, NewQ } -> {{value, Value}, NewQ}
  end;

take(Q = #ltq_queue{ front = Key }, Key) ->
  Index = Q#ltq_queue.index,
  { ok, Item } = maps:find(Key, Index),
  { ok, NextItem } = maps:find(Item#ltq_item.next, Index),
  NewFront = Item#ltq_item.next,
  NewIndex = maps:remove(Key, Index),
  NewQ = Q#ltq_queue{
    index = NewIndex#{
      NewFront => NextItem#ltq_item{
        prev = null
      }
    },
    front = NewFront
  },
  { {value, Item#ltq_item.value}, NewQ};

take(Q, Key) ->
  Index = Q#ltq_queue.index,
  case maps:find(Key, Index) of
    { ok, Item } ->
      { ok, NextItem } = maps:find(Item#ltq_item.next, Index),
      { ok, PrevItem } = maps:find(Item#ltq_item.prev, Index),
      NewIndex = maps:remove(Key, Index),
      NewQ = Q#ltq_queue{
        index = NewIndex#{
          Item#ltq_item.next => NextItem#ltq_item{
            prev = Item#ltq_item.prev
          },
          Item#ltq_item.prev => PrevItem#ltq_item{
            next = Item#ltq_item.next
          }
        }
      },
      {{value, Item#ltq_item.value}, NewQ };
    error ->
      { undefined, Q }
  end.

fetch(_Q = #ltq_queue{ back = null}) -> empty;
fetch(Q) ->
  Index = Q#ltq_queue.index,
  Back = Q#ltq_queue.back,
  { ok, Item } = maps:find(Back, Index),
  { Back, Item#ltq_item.value }.

from_list(List) ->
  lists:foldl(fun({Key, Value}, Q) ->
    { _, NewQ } = in(Q, Key, Value),
    NewQ
  end, new(), List).

to_list(Q) ->
  lists:reverse(foldl(Q, [], fun(Key, Value, Acc) ->
    [{Key, Value} | Acc ]
  end)).

len(Q) ->
  maps:size(Q#ltq_queue.index).

is_empty(Q) ->
  len(Q) =:= 0.

join(Q1, Q2) ->
  foldl(Q1, Q2, fun(Key, Value, Q) ->
    { _, NewQ } = in(Q, Key, Value),
    NewQ
  end).

%%TODO: needs to be optimised
peek(Q) ->
  { Result, _ } = out(Q),
  Result.

%%TODO: needs to be optimised
peek_r(Q) ->
  { Result, _ } = out_r(Q),
  Result.


next(Key, Q) ->
  case maps:find(Key, Q#ltq_queue.index) of
    { ok, #ltq_item{ next = Next }} -> Next;
    error -> null
  end.

prev(Key, Q) ->
  case maps:find(Key, Q#ltq_queue.index) of
    { ok, #ltq_item{ prev = Prev}} -> Prev;
    error -> null
  end.

front(Q) ->
  Q#ltq_queue.front.

back(Q) ->
  Q#ltq_queue.back.

foldl(Q, Acc, Fun) ->
  while_internal(Q, Acc, fun(Key, Item, A) ->
    NewAcc = Fun(Key, Item#ltq_item.value, A),
    { continue, NewAcc }
  end).

map(Q, Fun) ->
  NewIndexList = while_internal(Q, [], fun(Key, Item, Acc) ->
    NewValue = Fun(Key, Item#ltq_item.value),
    [{ Key, Item#ltq_item{ value = NewValue }} | Acc ]
  end),
  Q#ltq_queue{
    index = maps:from_list(NewIndexList)
  }.

reverse(Q) ->
  Index = Q#ltq_queue.index,
  NewIndex = maps:map(fun(_Key, Item) ->
    #ltq_item{
      prev = Item#ltq_item.next,
      next = Item#ltq_item.prev
    }
  end, Index),
  Q#ltq_queue{
    front = Q#ltq_queue.back,
    back = Q#ltq_queue.front,
    index = NewIndex
  }.

while(Q, Acc, Fun) ->
  while_internal(Q, Acc, fun(Key, Item, A) ->
    Fun(Key, Item#ltq_item.value, A)
  end).

%% Internal

while_internal(Q, Acc, Fun) ->
  Index = Q#ltq_queue.index,
  (fun
    Loop(null, A) -> A;
    Loop(Key, A) ->
      { ok, Item } = maps:find(Key, Index),
      case Fun(Key, Item, A) of
        { continue, NewAcc } ->
          Loop(Item#ltq_item.prev, NewAcc);
        { stop, NewAcc } -> NewAcc
      end
  end)(back(Q), Acc).


