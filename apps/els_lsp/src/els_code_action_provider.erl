-module(els_code_action_provider).

-behaviour(els_provider).

-export([ handle_request/2
        , is_enabled/0
        ]).

-include("els_lsp.hrl").

-type state() :: any().

%%==============================================================================
%% els_provider functions
%%==============================================================================

-spec is_enabled() -> boolean().
is_enabled() -> true.

-spec handle_request(any(), state()) -> {any(), state()}.
handle_request({document_codeaction, Params}, State) ->
  #{ <<"textDocument">> := #{ <<"uri">> := Uri}
   , <<"range">>        := RangeLSP
   , <<"context">>      := Context } = Params,
  %els_refactorerl_utils:notification(io_lib:format("R~p", [RangeLSP])),
  Result = code_actions(Uri, RangeLSP, Context),
  {Result, State}.

%%==============================================================================
%% Internal Functions
%%==============================================================================


%% @doc Result: `(Command | CodeAction)[] | null'
-spec code_actions(uri(), range(), code_action_context()) -> [map()].
code_actions(Uri, Range, Context) ->
  #{ <<"diagnostics">> := Diagnostics } = Context,
  Actions0 = [ make_code_action(Uri, D) || D <- Diagnostics],
  Actions = lists:flatten(Actions0),
  %els_refactorerl_utils:notification(io_lib:format("~p", [Actions])),
  RefactorErlActions = referl_action(Uri, Range),
  %els_refactorerl_utils:notification(io_lib:format("RA-~p", [RefactorErlActions])),
  
  Actions2 = Actions ++ RefactorErlActions,

  Actions2.

%% @doc Note: if the start and end line of the range are the same, the line
%% is simply added.
-spec replace_lines_action(uri(), binary(), binary(), binary(), range())
                          -> map().
replace_lines_action(Uri, Title, Kind, Lines, Range) ->
  #{ <<"start">> := #{ <<"character">> := _StartCol
                     , <<"line">>      := StartLine }
   , <<"end">>   := #{ <<"character">> := _EndCol
                     , <<"line">>      := EndLine }
   } = Range,
  #{ title => Title
   , kind => Kind
   , command =>
       els_command:make_command( Title
                               , <<"replace-lines">>
                               , [#{ uri   => Uri
                                   , lines => Lines
                                   , from  => StartLine
                                   , to    => EndLine }])
   }.

-spec make_code_action(uri(), els_diagnostics:diagnostic()) -> [map()].
make_code_action(Uri, #{ <<"message">> := Message
                       , <<"range">>   := Range } = _Diagnostic) ->
  unused_variable_action(Uri, Range, Message).

%%------------------------------------------------------------------------------

-spec unused_variable_action(uri(), range(), binary()) -> [map()].
unused_variable_action(Uri, Range, Message) ->
  %% Processing messages like "variable 'Foo' is unused"
  case re:run(Message, "variable '(.*)' is unused"
             , [{capture, all_but_first, binary}]) of
      {match, [UnusedVariable]} ->
          X = make_unused_variable_action(Uri, Range, UnusedVariable),
          %els_refactorerl_utils:notification(io_lib:format("~p", [X])),
          X;
      _ -> []
  end.

-spec make_unused_variable_action(uri(), range(), binary()) -> [map()].
make_unused_variable_action(Uri, Range, UnusedVariable) ->
  #{ <<"start">> := #{ <<"character">> := _StartCol
                     , <<"line">>      := StartLine }
   , <<"end">>   := _End
   } = Range,
  %% processing messages like "variable 'Foo' is unused"
  {ok, #{text := Bin}} = els_utils:lookup_document(Uri),
  Line = els_utils:to_list(els_text:line(Bin, StartLine)),

  {ok, Tokens, _} = erl_scan:string(Line, 1, [return, text]),
  UnusedString = els_utils:to_list(UnusedVariable),
  Replace =
        fun(Tok) ->
            case Tok of
                {var, [{text, UnusedString}, _], _} -> "_" ++ UnusedString;
                {var, [{text, VarName}, _], _} -> VarName;
                {_,   [{text, Text   }, _], _} -> Text;
                {_,   [{text, Text   }, _]}    -> Text
            end
  end,
  UpdatedLine = lists:flatten(lists:map(Replace, Tokens)) ++ "\n",
    [ replace_lines_action( Uri
                      , <<"Add '_' to '", UnusedVariable/binary, "'">>
                      , ?CODE_ACTION_KIND_QUICKFIX
                      , els_utils:to_binary(UpdatedLine)
                      , Range)].

%%==============================================================================
%% RefactorErl
%%==============================================================================

-spec referl_action(any(), range()) -> any().
referl_action(Uri, Range) ->
  variable_origin_action(Uri, Range).

-spec variable_origin_action(uri(), range()) -> [map()].
variable_origin_action(Uri, Range) ->
  {ok, DocumentList} = els_dt_document:lookup(Uri),
  [ Document | _Rest ] = DocumentList,
  Pois = els_dt_document:pois(Document, [variable]),
  %els_refactorerl_utils:notification(io_lib:format("POIS-~p", [ Pois ])),
  %els_refactorerl_utils:notification("ALMA"),
  Alma = lists:flatten([ make_variable_origin_action(Uri, Range, Pois) ]), % Todo: flatten
  %els_refactorerl_utils:notification(io_lib:format("VO-~p", [Alma])),
  Alma.


-spec make_variable_origin_action(uri(), range(), [poi()]) -> [map()].
make_variable_origin_action(Uri, Range, Pois) ->
  %#{ <<"start">> := #{ <<"character">> := _StartCol
  %                   , <<"line">>      := StartLine }
  % , <<"end">>   := _End
  % } = Range,
  %% processing messages like "variable 'Foo' is unused"
  %{ok, #{text := Bin}} = els_utils:lookup_document(Uri),
  %Line = els_utils:to_list(els_text:line(Bin, StartLine)),

  %{ok, Tokens, _} = erl_scan:string(Line, 1, [return, text]),
  %UnusedString = els_utils:to_list(Variable),
  %Replace =
  %      fun(Tok) ->
  %          case Tok of
  %              {var, [{text, UnusedString}, _], _} -> "_" ++ UnusedString;
  %              {var, [{text, VarName}, _], _} -> VarName;
  %              {_,   [{text, Text   }, _], _} -> Text;
  %              {_,   [{text, Text   }, _]}    -> Text
  %          end
  %end,
  %UpdatedLine = lists:flatten(lists:map(Replace, Tokens)) ++ "\n",
  % 
  % 
  
  ConvertedRange = els_range:to_poi_range(Range),
  %els_refactorerl_utils:notification(io_lib:format("~p", [Range])),

  
  Ok = lists:filter(fun(#{range := PoiRange}) -> els_range:in(ConvertedRange, PoiRange) end, Pois),
  case length(Ok) of
    0 -> [];
    _ ->
      #{id := VarName} = hd(Ok),
      VarNameBin = atom_to_binary(VarName),

      Title =  <<"Variable Origin of ", VarNameBin/binary>>,
      Aha = #{ title =>  Title
       , kind => <<"refactor">>
       , command =>
           els_command:make_command( Title
                                   , <<"refactorerl-variable-origin">>
                                   , [#{ uri  => Uri, range => Range}])
       },

      [ Aha ]
      end.


   
  
  

  


%%------------------------------------------------------------------------------
