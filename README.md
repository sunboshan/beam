# Beam

Helper functions to understand BEAM.

## Installation

```
$ mix escript.build
```

## Usage

```Elixir
defmodule Foo do
  def run(a) do
    a + 1
  end
end
```

```
$ ./beam /path/to/Elixir.Module.beam

run/1
label 6
  {:line, [{:location, 'lib/foo.ex', 2}]}
  {:func_info, {:atom, Foo}, {:atom, :run}, 1}
label 7
  {:line, [{:location, 'lib/foo.ex', 3}]}
  {:gc_bif, :+, {:f, 0}, 1, [x: 0, integer: 1], {:x, 0}}
  :return


$ ./beam /path/to/Elixir.Module.beam -e
-file("/private/tmp/ex/foo/lib/foo.ex", 1).

-module('Elixir.Foo').

-compile([no_auto_import]).

-export(['__info__'/1, run/1]).

-spec '__info__'(attributes | compile | functions |
                 macros | md5 | module | deprecated) -> any().

'__info__'(module) -> 'Elixir.Foo';
'__info__'(functions) -> [{run, 1}];
'__info__'(macros) -> [];
'__info__'(Key = attributes) ->
    erlang:get_module_info('Elixir.Foo', Key);
'__info__'(Key = compile) ->
    erlang:get_module_info('Elixir.Foo', Key);
'__info__'(Key = md5) ->
    erlang:get_module_info('Elixir.Foo', Key);
'__info__'(deprecated) -> [].

run(_a@1) -> _a@1 + 1.
```
