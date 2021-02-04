%%%-------------------------------------------------------------------
%%% @copyright (C) 2021, Aeternity Anstalt
%%%-------------------------------------------------------------------

-module(swagger_endpoints_tests).

-include_lib("eunit/include/eunit.hrl").

-define(TEST_MODULE, swagger_endpoints).

from_yaml_test_() ->
    {foreach,
      fun() ->
          application:start(yamerl)
      end,
      fun(_) ->
          application:stop(yamerl)
      end,
      [ {"Parses single definition", fun parses_definition/0}
      ]}.

parses_definition() ->
    Props =
        #{ paths => [req_current_keyblock_height(),
                     req_get_current_keyblock(),
                     req_get_keyblock_by_hash()
                    ]
        , definitions => [d_uint(), d_uint16(), d_uint32(), d_uint64(), d_txblockheight(),
                          d_encodedhash(), d_encodedpubkey(), d_encodedbytearray(),
                          d_error(),
                          d_pow(),
                          d_keyblock()
                         ]},
    V2Yaml = yaml(Props#{ swagger => "2.0"}),
    _Expected = ?TEST_MODULE:from_yaml(V2Yaml, []).

yaml(Opts) ->
    G = fun(K, Def) -> maps:get(K, Opts, Def) end,
    Tags =
        multiline(lists:map(
            fun({Name, Descr}) ->
                multiline([ "  - name: " ++ Name
                          , "    description: " ++ Descr])
            end,
            G(tags, [ {"external", "External API"}
                    , {"internal", "Internal API"}
                    , {"chain", "Chain related endpoints"}
                    , {"transaction", "Transaction related endpoints"}
                    ]))),
    Schemes =
        multiline(lists:map(
            fun(Scheme) -> "  - " ++ Scheme end, G(schemes, ["http"]))),
    Paths =
        multiline(lists:map(
            fun({URL, Methods}) ->
                EncMethods =
                    multiline(lists:map(
                        fun encode_method/1,
                        Methods)),
                multiline(
                    [ "  " ++ URL ++ ":"
                    , EncMethods])
            end,
            G(paths, []))),
    Definitions =
        multiline(lists:map(fun enc_definition/1, G(definitions, []))),
    Str =
        multiline(
            [ "swagger: '" ++ G(swagger, "2.0") ++ "'"
            , "info:"
            , "  description: '" ++ G(description, "This is the [Aeternity](https://www.aeternity.com/) node API.") ++ "'"
            , "  version: " ++ G(version, "x.y.z")
            , "  title: " ++ G(title, "Aeternity node")
            , "  termsOfService: '" ++ G(tos, "https://www.aeternity.com/terms/") ++ "'"
            , "  contact:"
            , "    email: " ++ G(email, "apiteam@aeternity.com")
            , "basePath: " ++ G(basePath, "/v2")
            , "tags:\n" ++ Tags
            , "schemes:\n" ++ Schemes 
            , "paths:\n" ++ Paths
            , "definitions:\n" ++ Definitions
            ]),
    [Yaml] =yamerl_constr:string(Str),
    Yaml.

multiline(L) -> lists:concat(lists:join("\n", L)).

encode_method({Method, #{ tags        := Tags0
                        , operationid := OperationId
                        , description := Description
                        , parameters  := Parameters0
                        , responses   := Responses0}}) ->
    Tags =
        multiline(lists:map(
            fun(T) -> "        - " ++ T end, Tags0)),
    Parameters1 =
        multiline(lists:map(
            fun(#{ in := In
                 , name := Name
                 , description := ParamDesr
                 , required := Required
                 , type := Type }) ->
                multiline(
                    [ "        - in: " ++ In
                    , "          name: " ++ Name
                    , "          description: '" ++ ParamDesr ++ "'"
                    , "          required: " ++ Required
                    , "          type: " ++ Type])
            end,
            Parameters0)),
    Parameters =
        case length(Parameters1) =:= 0 of
            true ->  "      parameters: []";
            false -> "      parameters:\n" ++ Parameters1
        end,
    Responses =
        multiline(lists:map(
            fun({Code, #{ description := RespDesr
                        , schema := Schema}}) ->
                multiline(
                    [ "        '" ++ integer_to_list(Code) ++ "':"
                    , "          description: '" ++ RespDesr ++ "'" 
                    , "          schema:\n" ++ encode_schema(Schema)])
            end,
            Responses0)),
    multiline(
        [ "    " ++ Method ++ ":"
        , "      tags:\n" ++ Tags
        , "      operationId: " ++ OperationId
        , "      description: '" ++ Description ++ "'"
        , "      produces:"
        , "        - application/json"
        , Parameters
        , "      responses:\n" ++ Responses]).


encode_schema({ref, Ref}) ->
    "            $ref: '" ++ Ref ++ "'";
encode_schema({object, Properties0}) ->
    Properties =
        multiline(lists:map(
            fun({Name, #{ description := Descr
                        , ref := Ref}}) ->
                multiline(
                    [ "              " ++ Name ++ ":"
                    , "                description: '" ++ Descr ++ "'"
                    , "                $ref: '" ++ Ref ++ "'"])
            end,
            Properties0)),
    multiline(
        [ "            type: object"
        , "            properties:\n" ++ Properties]).

enc_definition({Name, Type, Opts}) when Type =:= integer;
                                        Type =:= string ->
    OFun =
        fun(Key) ->
            case maps:find(Key, Opts) of
                {ok, Val} when is_integer(Val) -> ["    " ++ atom_to_list(Key) ++ ": " ++ integer_to_list(Val)];
                {ok, Val} when is_list(Val)    -> ["    " ++ atom_to_list(Key) ++ ": '" ++ Val ++ "'"];
                error -> []
            end
        end,
    Optionals = [OFun(E) || E <- [minimum, maximum, description]],
    multiline(
      [ "  " ++ Name ++ ":"
      , "    type: " ++ atom_to_list(Type)
      ] ++ Optionals);
enc_definition({Name, array, Ref, Opts}) ->
    OFun =
        fun(Key) ->
            case maps:find(Key, Opts) of
                {ok, Val} when is_integer(Val) -> ["    " ++ atom_to_list(Key) ++ ": " ++ integer_to_list(Val)];
                {ok, Val} when is_list(Val)    -> ["    " ++ atom_to_list(Key) ++ ": '" ++ Val ++ "'"];
                error -> []
            end
        end,
    Optionals = [OFun(E) || E <- [minItems, maxItems, description]],
    multiline(
      [ "  " ++ Name ++ ":"
      , "    type: array"
      , "    items:"
      , "      $ref: '" ++ Ref ++ "'"
      ] ++ Optionals);
enc_definition({Name, object, Properties0, Required0, Opts}) ->
    OFun =
        fun(Key) ->
            case maps:find(Key, Opts) of
                {ok, Val} when is_integer(Val) -> ["    " ++ atom_to_list(Key) ++ ": " ++ integer_to_list(Val)];
                {ok, Val} when is_list(Val)    -> ["    " ++ atom_to_list(Key) ++ ": '" ++ Val ++ "'"];
                error -> []
            end
        end,
    Optionals = [OFun(E) || E <- [minItems, maxItems, description]],
    Properties =
        multiline(lists:map(
            fun({K, Ref}) when is_list(Ref) ->
                "      " ++ atom_to_list(K) ++ ":\n" ++
                "        $ref: '" ++ Ref ++ "'";
               ({K, Type}) when Type =:= integer; Type =:= string ->
                "      " ++ atom_to_list(K) ++ ":\n" ++
                "        type: " ++ atom_to_list(Type)
            end,
            maps:to_list(Properties0))),
    Required = 
        multiline(lists:map(
            fun(K) -> "      - " ++ atom_to_list(K) end,
            Required0)),
    multiline(
      [ "  " ++ Name ++ ":"
      , "    type: object"
      , "    properties:\n" ++ Properties
      , "    required:\n" ++ Required 
      ] ++ Optionals).

request_method(Method, Tags, OperationId, Description, Params, Responses) ->
    {Method, #{ tags        => Tags
              , operationid => OperationId
              , description => Description
              , parameters  => Params
              , responses   => Responses}}.

response(Code, Desc, Schema) ->
    {Code, #{description => Desc, schema => Schema}}.

req_current_keyblock_height() ->
    Resp =
        {object, [{"height",
                    #{ description => "Height"
                    , ref => "#/definitions/UInt16"}}]},
    Get =
        request_method("get", ["external", "chain"],
                      "GetCurrentKeyBlockHeight", "Get the height of the current key block",
                      [], [success_response(Resp),
                           error_response(404, "Block not found")]),
    {"/key-blocks/current/height", [Get]}.

req_get_current_keyblock() ->
    Get =
        request_method("get", ["external", "chain"],
                      "GetCurrentKeyBlock", "Get the current key block",
                      [], [success_response({ref, "#/definitions/KeyBlock"}),
                           error_response(404, "Block not found")]),
    {"/key-blocks/current", [Get]}.

req_get_keyblock_by_hash() ->
    Params =
        [#{ in => "path"
          , name => "hash"
          , description => "The hash of the block"
          , required => "true" 
          , type => "string" }],
    Get =
        request_method("get", ["external", "chain"],
                      "GetKeyBlockByHash", "Get a key block by hash",
                      Params,
                      [ success_response({ref, "#/definitions/KeyBlock"}),
                        error_response(404, "Block not found"),
                        error_response(400, "Invalid hash")]),
    {"/key-blocks/hash/{hash}", [Get]}.
    
success_response(RespObj) ->
    response(200, "Successful operation", RespObj).

error_response(Code, Description) ->
    response(Code, Description, {ref, "#/definitions/Error"}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% definitions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%
d_uint() ->
    {"UInt", integer, #{minimum => 0}}.

d_uint16() ->
    {"UInt16", integer, #{minimum => 0, maximum => 65535}}.

d_uint32() ->
    {"UInt32", integer, #{minimum => 0, maximum => 4294967295}}.

d_uint64() ->
    {"UInt64", integer, #{minimum => 0, maximum => 18446744073709551615}}.

d_txblockheight() ->
    {"TxBlockHeight", integer, #{minimum => -1, maximum => 18446744073709551615}}.

d_encodedhash() ->
    {"EncodedHash", string, #{description => "Base58Check encoded tagged hash"}}.

d_encodedpubkey() ->
    {"EncodedPubkey", string, #{description => "Base58Check encoded tagged pubkey"}}.

d_encodedbytearray() ->
    {"EncodedByteArray", string, #{description => "Base64Check encoded tagged byte array"}}.

d_pow() ->
    {"Pow", array, "#/definitions/UInt32", #{minItems => 42, maxItems => 42}}.

d_error() ->
    {"Error", object, #{reason => string}, [reason], #{}}.

d_keyblock() ->
    Properties = #{ hash => "#/definitions/EncodedHash"
                  , height => "#/definitions/UInt64"
                  , prev_hash => "#/definitions/EncodedHash"
                  , prev_key_hash => "#/definitions/EncodedHash"
                  , state_hash => "#/definitions/EncodedHash"
                  , miner => "#/definitions/EncodedPubkey"
                  , beneficiary => "#/definitions/EncodedPubkey"
                  , target => "#/definitions/UInt32"
                  , pow => "#/definitions/Pow"
                  , nonce => "#/definitions/UInt64"
                  , time => "#/definitions/UInt64"
                  , version => "#/definitions/UInt32"
                  , info => "#/definitions/EncodedByteArray"},
    Required = maps:keys(Properties),
    {"KeyBlock", object, Properties, Required, #{}}.

