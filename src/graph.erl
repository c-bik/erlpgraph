-module(graph).

-export([all/0]).

-define(NODE_PID,       0).
-define(NODE_PORT,      1).
-define(NODE_UNKNOWN,   2).

all() ->
    All = erlang:processes() ++ erlang:ports(),
    %All = [erlang:whereis(P) || P <- erlang:registered()],

    Vertices = lists:foldl(
        fun(PID, Acc) ->
            [port_or_pid_to_obj(PID) | Acc]
        end,
        [],
        All),

    Edges0 = lists:foldl(
        fun(PID, Acc) ->
            Acc0 = case catch erlang:process_info(PID, links) of
                {links, LinkList}   -> [{PID, LinkList} | Acc];
                _                   -> Acc
            end,
            Acc1 = case catch erlang:port_info(PID, links) of
                {links, LinkList2}  -> [{PID, LinkList2} | Acc0];
                _                   -> Acc0
            end,
            case catch erlang:process_info(PID, monitored_by) of
                {monitored_by, MonList} -> [{PID, MonList} | Acc1];
                _                       -> Acc1
            end
        end,
        [],
        All),

    % prepare unique edges list (undirected graph)
    Edges = lists:foldl(
        fun({PID, Links}, Acc) ->
            case lists:keyfind(PID, 1, Acc) of
                false ->
                    case lists:keyfind(PID, 2, Acc) of
                        false -> [[port_or_pid_to_obj(PID),port_or_pid_to_obj(L)] || L <- Links] ++ Acc;
                        _ -> Acc
                    end;
                _ -> Acc
            end
        end,
        [],
        Edges0),

    lager:info("~s~n~s", [binary_to_list(jsx:prettify(jsx:encode(Vertices))), binary_to_list(jsx:prettify(jsx:encode(Edges)))]),
    jsx:encode([{<<"vertices">>, Vertices}, {<<"edges">>, Edges}]).

port_or_pid_to_obj(PID) ->
    case catch erlang:process_info(PID, registered_name) of
        {registered_name, Name} -> [{<<"type">>, ?NODE_PID}, {<<"name">>, list_to_binary(atom_to_list(Name))}];
        _ ->
            case catch erlang:port_info(PID, name) of
                {name, Name} -> [{<<"type">>, ?NODE_PORT}, {<<"name">>, list_to_binary(Name)}];
                _ -> [{<<"type">>, ?NODE_UNKNOWN}, {<<"name">>, list_to_binary(pid_to_list(PID))}]
            end
    end.
