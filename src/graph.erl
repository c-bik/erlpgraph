-module(graph).

-export([all/0]).

all() ->
    All = erlang:processes() ++ erlang:ports(),

    Vertices = lists:foldl(
        fun(PID, Acc) ->
            case catch erlang:process_info(PID, registered_name) of
                {registered_name, Name} -> [{PID, pid_reg, atom_to_list(Name)} | Acc];
                _ ->
                    case catch erlang:port_info(PID, name) of
                        {name, Name} -> [{PID, port_reg, Name} | Acc];
                        _ -> [{PID, unnamed, pid_to_list(PID)} | Acc]
                    end
            end
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

    Edges = lists:foldl(
        fun({PID, Links}, Acc) ->
            case lists:keyfind(PID, 1, Acc) of
                false ->
                    case lists:keyfind(PID, 2, Acc) of
                        false -> [{PID,L} || L <- Links] ++ Acc;
                        _ -> Acc
                    end;
                _ -> Acc
            end
        end,
        [],
        Edges0),

    {Vertices, Edges}.
