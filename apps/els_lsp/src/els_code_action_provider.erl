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
  RefactorErlActions = referl_action(Uri, Range),
  Actions ++ RefactorErlActions.


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
  variable_action(Uri, Range) 
  ++ depgraph_action(Uri, Range) 
  ++ dyncall_action(Uri, Range).

-spec variable_action(uri(), range()) -> [map()].
variable_action(Uri, Range) ->
  {ok, DocumentList} = els_dt_document:lookup(Uri),
  [ Document | _Rest ] = DocumentList,
  Pois = els_dt_document:pois(Document, [variable]),
  lists:flatten([ make_variable_origin_action(Uri, Range, Pois) ]).


-spec make_variable_origin_action(uri(), range(), [poi()]) -> [map()].
make_variable_origin_action(Uri, Range, Pois) ->
  ConvertedRange = els_range:to_poi_range(Range),
  MatchingRanges = lists:filter(
    fun(#{range := PoiRange}) ->
      els_range:in(ConvertedRange, PoiRange)
    end, Pois),

  case length(MatchingRanges) of
    0 -> [];
    _ ->
      #{id := VarName} = hd(MatchingRanges),
      VarNameBin = atom_to_binary(VarName, utf8),

      TitleOrigin =  <<"Variable Origin of ", VarNameBin/binary>>,
      OriginAction = #{ title =>  TitleOrigin
       , kind => <<"refactor">>
       , command =>
           els_command:make_command( TitleOrigin
                                   , <<"refactorerl-variable-origin">>
                                   , [#{ uri  => Uri, range => Range}])
       },

      TitleReach =  <<"Variable Reach of ", VarNameBin/binary>>,
      ReachAction = #{ title =>  TitleReach
       , kind => <<"refactor">>
       , command =>
           els_command:make_command( TitleReach
                                   , <<"refactorerl-variable-reach">>
                                   , [#{ uri  => Uri, range => Range}])
       },

      [ OriginAction, ReachAction ]
      end.

-spec depgraph_action(uri(), range()) -> [map()].
depgraph_action(Uri, Range) ->
  {ok, DocumentList} = els_dt_document:lookup(Uri),
  [ Document | _Rest ] = DocumentList,
  Pois = els_dt_document:pois(Document, [application, function_clause, module]),

  Alma = lists:flatten([ make_depgraph_action(Uri, Range, Pois) ]),
  Alma.

-spec make_depgraph_action(uri(), range(), [poi()]) -> [map()].
make_depgraph_action(Uri, Range, Pois) ->
  ConvertedRange = els_range:to_poi_range(Range),
  MatchingRanges = lists:filter(
    fun(#{range := PoiRange}) ->
      els_range:in(ConvertedRange, PoiRange)
    end, Pois),

  case length(MatchingRanges) of
    0 -> [];
    _ ->
      #{kind := Kind, id := ID} = hd(MatchingRanges),
      {Name, Type} = case Kind of
        application -> % Function calls
          els_refactorerl_utils:notificaiton("Hoo!"),
          {Mod, Fun, Arity} = ID,
          FunName = io_lib:format("~p:~p/~p", [Mod, Fun, Arity]),
          els_refactorerl_utils:notificaiton("Hoo!" ++ FunName),
          {FunName, func};

        function_clause -> % Function definitions
          {Fun, Arity, _Clause} = ID,
          Mod = els_uri:module(Uri),
          FunName = io_lib:format("~p:~p/~p", [Mod, Fun, Arity]),
          {FunName, func};

        module ->
          ModName = atom_to_list(ID),
          {ModName, mod}
      end,

      NameBin = list_to_binary(Name),
      Title =  <<"Dependency graph from ", NameBin/binary>>,
      CodeAction = #{ title =>  Title
                    , kind => <<"refactor">>
                    , command =>
           els_command:make_command( Title
                                   , <<"refactorerl-dependency-graph">>
                                   , [#{ type => Type, name => Name}])
       },
      [ CodeAction ]
    end.



-spec dyncall_action(uri(), range()) -> [map()].
dyncall_action(Uri, Range) ->
  {ok, DocumentList} = els_dt_document:lookup(Uri),
  [ Document | _Rest ] = DocumentList,
  Pois = els_dt_document:pois(Document, [function_clause, application]),
  lists:flatten([ make_dyncall_action(Uri, Range, Pois) ]).

-spec make_dyncall_action(uri(), range(), [poi()]) -> [map()].
make_dyncall_action(Uri, Range, Pois) ->
  ConvertedRange = els_range:to_poi_range(Range),
  MatchingRanges = lists:filter(
    fun(#{range := PoiRange}) ->
      els_range:in(ConvertedRange, PoiRange)
    end, Pois),

  case length(MatchingRanges) of
    0 -> [];
    _ ->
      #{kind := Kind, id := ID} = hd(MatchingRanges),
      {Mod, Fun, Arity} = case Kind of
        application -> % Function calls
          els_refactorerl_utils:notificaiton("Hey!"),
          {_M, _F, _A} = ID;
         
        function_clause -> % Function definitions
          {F, A, _Clause} = ID,
          M = els_uri:module(Uri),
          {M, F, A}
      end,

      Name =  io_lib:format("~p:~p/~p", [Mod, Fun, Arity]),
      NameBin = list_to_binary(Name),

      ModBin = io_lib:format("~p", [Mod]),
      FunBin = io_lib:format("~p", [Fun]),
      ArityBin = io_lib:format("~p", [Arity]),

      Title =  <<"Function references of", NameBin/binary>>,
      CodeAction = #{ title =>  Title
                    , kind => <<"refactor">>
                    , command =>
           els_command:make_command( Title
                                   , <<"refactorerl-dyncall">>
                                   , [#{ module  => ModBin, func => FunBin, arity => ArityBin}])
       },
      [ CodeAction ]
    end.



%%------------------------------------------------------------------------------
