defmodule BeamTest do
  use ExUnit.Case
  doctest Beam

  test "greets the world" do
    assert Beam.hello() == :world
  end
end
