-module(els_execute_command_provider).

-behaviour(els_provider).

-export([ handle_request/2
        , is_enabled/0
        , options/0
        ]).

%%==============================================================================
%% Includes
%%==============================================================================
-include("els_lsp.hrl").
-include_lib("kernel/include/logger.hrl").

-type state() :: any().

%%==============================================================================
%% els_provider functions
%%==============================================================================

-spec is_enabled() -> boolean().
is_enabled() -> true.

-spec options() -> map().
options() ->
  #{ commands => [ els_command:with_prefix(<<"replace-lines">>)
                 , els_command:with_prefix(<<"server-info">>)
                 , els_command:with_prefix(<<"ct-run-test">>)
                 , els_command:with_prefix(<<"show-behaviour-usages">>)
                 , els_command:with_prefix(<<"suggest-spec">>)
                 , els_command:with_prefix(<<"function-references">>)
                 , els_command:with_prefix(<<"refactorerl-variable-origin">>)
                 , els_command:with_prefix(<<"refactorerl-variable-reach">>)
                 , els_command:with_prefix(<<"refactorerl-dependency-graph">>)
                 , els_command:with_prefix(<<"refactorerl-dyncall">>)        
                 ] }.

-spec handle_request(any(), state()) -> {any(), state()}.
handle_request({workspace_executecommand, Params}, State) ->
  #{ <<"command">> := PrefixedCommand } = Params,
  Arguments = maps:get(<<"arguments">>, Params, []),
  Result = execute_command( els_command:without_prefix(PrefixedCommand)
                          , Arguments),
  {Result, State}.

%%==============================================================================
%% Internal Functions
%%==============================================================================

-spec execute_command(els_command:command_id(), [any()]) -> [map()].
execute_command(<<"replace-lines">>
               , [#{ <<"uri">>   := Uri
                   , <<"lines">> := Lines
                   , <<"from">>  := LineFrom
                   , <<"to">>    := LineTo }]) ->
  Method = <<"workspace/applyEdit">>,
  Params = #{ edit =>
                  els_text_edit:edit_replace_text(Uri, Lines, LineFrom, LineTo)
            },
  els_server:send_request(Method, Params),
  [];
execute_command(<<"server-info">>, _Arguments) ->
  {ok, Version} = application:get_key(?APP, vsn),
  BinVersion = list_to_binary(Version),
  Root = filename:basename(els_uri:path(els_config:get(root_uri))),
  ConfigPath = case els_config:get(config_path) of
                 undefined -> <<"undefined">>;
                 Path -> list_to_binary(Path)
               end,

  OtpPathConfig = list_to_binary(els_config:get(otp_path)),
  OtpRootDir = list_to_binary(code:root_dir()),
  OtpMessage = case OtpRootDir == OtpPathConfig of
                 true ->
                   <<", OTP root ", OtpRootDir/binary>>;
                 false ->
                   <<", OTP root(code):"
                    , OtpRootDir/binary
                   , ", OTP root(config):"
                    , OtpPathConfig/binary>>
               end,
  Message = <<"Erlang LS (in ", Root/binary, "), version: "
             , BinVersion/binary
             , ", config from "
             , ConfigPath/binary
             , OtpMessage/binary
            >>,
  els_server:send_notification(<<"window/showMessage">>,
                               #{ type => ?MESSAGE_TYPE_INFO,
                                  message => Message
                                }),
  [];
execute_command(<<"ct-run-test">>, [Params]) ->
  els_command_ct_run_test:execute(Params),
  [];
execute_command(<<"function-references">>, [_Params]) ->
  [];
execute_command(<<"show-behaviour-usages">>, [_Params]) ->
  [];
execute_command(<<"suggest-spec">>, []) ->
  [];
execute_command(<<"suggest-spec">>, [#{ <<"uri">> := Uri
                                      , <<"line">> := Line
                                      , <<"spec">> := Spec
                                      }]) ->
  Method = <<"workspace/applyEdit">>,
  {ok, #{text := Text}} = els_utils:lookup_document(Uri),
  LineText = els_text:line(Text, Line - 1),
  NewText = <<Spec/binary, "\n", LineText/binary, "\n">>,
  Params =
    #{ edit =>
         els_text_edit:edit_replace_text(Uri, NewText, Line - 1, Line)
     },
  els_server:send_request(Method, Params),
  [];
execute_command(<<"refactorerl-variable-origin">>, Arguments) ->
  case length(Arguments) of
    1 ->
      Argument = hd(Arguments),
      #{ <<"range">> := RangeRaw, <<"uri">> := Uri} = Argument,
      Range = els_range:to_poi_range(RangeRaw),
      #{from := From} = Range,
      Path = els_uri:path(Uri),
      els_refactorerl_utils:variable_orgin(Path, From),
      [];
    _ -> []
  end;

execute_command(<<"refactorerl-variable-reach">>, Arguments) ->
  case length(Arguments) of
    1 ->
      Argument = hd(Arguments),
      #{ <<"range">> := RangeRaw, <<"uri">> := Uri} = Argument,
      Range = els_range:to_poi_range(RangeRaw),
      #{from := From} = Range,
      Path = els_uri:path(Uri),
      els_refactorerl_utils:variable_reach(Path, From),
      [];
    _ -> []
  end;

execute_command(<<"refactorerl-dependency-graph">>, Arguments) ->
  case length(Arguments) of
    1 ->
      Argument = hd(Arguments),
      #{ <<"type">> := Type, <<"name">> := Name} = Argument,
      els_refactorerl_utils:dependency_graph(Name, Type),
      [];
    _ -> []
  end;

execute_command(<<"refactorerl-dyncall">>, Arguments) ->
  case length(Arguments) of
    1 ->
      Argument = hd(Arguments),
      #{ <<"module">> := Module, <<"func">> := Func, <<"arity">> := Arity} = Argument,
      
      els_refactorerl_utils:dyncall(hd(Module), hd(Func), hd(Arity)),
      [];
    _ -> []
  end;


execute_command(Command, Arguments) ->
  ?LOG_INFO("Unsupported command: [Command=~p] [Arguments=~p]"
           , [Command, Arguments]),
  [].
