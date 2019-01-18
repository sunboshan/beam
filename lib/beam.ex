defmodule Beam do
  @moduledoc """
  Helper functions to understand BEAM.
  """

  def main([path]) do
    Path.absname(path)
    |> String.to_charlist()
    |> to_assembly()
    |> render()
  end

  def main([path, "-e"]) do
    Path.absname(path)
    |> String.to_charlist()
    |> to_erl()
  end

  def run1(path) do
    Path.absname(path)
    |> String.to_charlist()
    |> to_assembly()
  end

  def run2(path) do
    Path.absname(path)
    |> String.to_charlist()
    |> to_assembly()
    |> render()
  end

  @doc """
  Pretty print the AST in erlang.
  """
  @spec to_erl(charlist) :: :ok
  def to_erl(path) when is_list(path) do
    {:ok, {_, [{:abstract_code, {_, ac}}]}} = :beam_lib.chunks(path, [:abstract_code])
    :erl_syntax.form_list(ac)
    |> :erl_prettypr.format()
    |> IO.puts()
  end

  @doc """
  Return the assembly code from AST.
  """
  @spec to_assembly(charlist) :: list
  def to_assembly(path) when is_list(path) do
    {:ok, {_, [{:abstract_code, {_, ac}}]}} = :beam_lib.chunks(path, [:abstract_code])
    {:ok, _, {_, _, _, assembly, _}} = :compile.forms(ac, [:S, outdir: '/tmp'])
    assembly
  end

  @doc """
  Render the assembly code nicely.
  """
  @spec render(list) :: :ok
  def render(list) do
    Enum.each(list, &do_render/1)
  end

  defp do_render({:function, :__info__, _, _, _}), do: :ok
  defp do_render({:function, :module_info, _, _, _}), do: :ok
  defp do_render({:function, _, _, _, opcodes}) do
    Stream.map(opcodes, &opcode/1)
    |> Stream.filter(&(&1))
    |> Enum.each(&IO.puts/1)
    IO.puts ""
  end

  defp opcode({:line, _}), do: nil
  defp opcode({:label, n}), do: "label #{n}"
  defp opcode(:return), do: "  return"

  defp opcode({:func_info, {:atom, mod}, {:atom, fun}, arity}) do
    IO.ANSI.format([:bright, "  #{mod}.#{fun}/#{arity}"])
  end

  defp opcode({:test, test, {:f, f}, [{reg, n}]}) do
    IO.ANSI.format(["  #{test}(", color(reg), "#{reg}#{n}", :reset, ") -> ",
      :light_red, "label #{f}"])
  end

  defp opcode({:move, {:literal, term}, {reg, n}}) do
    IO.ANSI.format([color(reg), "  #{reg}#{n}", :reset, " <- #{term(term)}"])
  end
  defp opcode({:move, {:integer, term}, {reg, n}}) do
    IO.ANSI.format([color(reg), "  #{reg}#{n}", :reset, " <- #{term(term)}"])
  end
  defp opcode({:move, {reg1, n1}, {reg2, n2}}) do
    IO.ANSI.format([color(reg2), "  #{reg2}#{n2}", :reset, " <- ",
      color(reg1), "#{reg1}#{n1}"])
  end

  defp opcode({:get_list, {reg, n}, {reg_h, n_h}, {reg_t, n_t}}) do
    IO.ANSI.format(["  [", color(reg_h), "#{reg_h}#{n_h}", :reset, " | ",
      color(reg_t), "#{reg_t}#{n_t}", :reset, "] = ",
      color(reg), "#{reg}#{n}"])
  end

  defp opcode({:gc_bif, :+, _, _, [left, right], {reg, n}}) do
  end

  defp opcode({:call_only, _, {:f, n}}) do
    " call_only: label #{n} (tail recursive)"
  end
  defp opcode({:call_ext, _, {:extfunc, mod, fun, arity}}) do
    "  call_ext: #{mod}.#{fun}/#{arity}"
  end
  defp opcode({:call_ext_last, _, {:extfunc, mod, fun, arity}, n}) do
    IO.ANSI.format(["  call_ext_last: #{mod}.#{fun}/#{arity}, ",
      color(:y), "dealloc_stack: #{n}", :reset, " (tail)"])
  end

  defp opcode({:allocate, n, _}) do
    IO.ANSI.format([color(:y), "  alloc_stack: #{n}"])
  end

  defp opcode(opcode) do
    "  #{term(opcode)}"
  end

  defp term(term) do
    # inspect(term, syntax_colors: [atom: :cyan, number: :yellow])
    inspect(term)
  end

  defp color(:x), do: :light_blue
  defp color(:y), do: :light_magenta
  defp color(_), do: :bright
end
