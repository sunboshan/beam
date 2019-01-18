# Beam

Helper functions to understand BEAM.

## Installation

```
$ mix do escript.build, escript.install
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
$ mix compile
$ beam /path/to/Elixir.Module.beam

run/1
label 6
  {:line, [{:location, 'lib/foo.ex', 2}]}
  {:func_info, {:atom, Foo}, {:atom, :run}, 1}
label 7
  {:line, [{:location, 'lib/foo.ex', 3}]}
  {:gc_bif, :+, {:f, 0}, 1, [x: 0, integer: 1], {:x, 0}}
  :return
```
