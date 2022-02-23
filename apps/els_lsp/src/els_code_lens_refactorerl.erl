-module(els_code_lens_refactorerl).

-behaviour(els_code_lens).
-export([ is_default/0
        , pois/1
        , command/3
        ]).

-include("els_lsp.hrl").

-spec is_default() -> boolean().
is_default() ->
  true.

-spec pois(els_dt_document:item()) -> [poi()].
pois(Document) ->
  els_dt_document:pois(Document, [function]).

-spec command(els_dt_document:item(), poi(), els_code_lens:state()) ->
        els_command:command().
command(Document, POI, _State) ->
  #{uri := Uri} = Document,
  Title = title(Document, POI),
  CommandId = <<"refactorerl">>,
  CommandArgs = [#{ uri => Uri}],
  els_command:make_command(Title, CommandId, CommandArgs).

-spec title(els_dt_document:item(), poi()) -> binary().
title(_Document, POI) ->
  %#{uri := Uri} = Document,
  %M = els_uri:module(Uri),
  #{id := _V} = POI,
  %%{ok, References} = els_dt_references:find_by_id(function, {M, F, A}),
  %%N = length(References),
  unicode:characters_to_binary("RefactorErl").
