defmodule Khf1 do

  @moduledoc """
  Kemping
  @author "Reiter Márton marci.reiter501@gmail.com"
  @date   "2022-09-25"
  ...
  """

  @doc """
  ...
  """
  @type row   :: integer    # sor száma (1-től n-ig)
  @type col   :: integer    # oszlop száma (1-től m-ig)
  @type field :: {row, col} # egy parcella koordinátái

  @type tents_count_rows :: [integer] # a sátrak száma soronként
  @type tents_count_cols :: [integer] # a sátrak száma oszloponként
  @type value            :: String.t # a sátor pozíciójának karaktere
  @type cell             :: {col, value} # oszlopszám - pozíció páros
  @type useful_row       :: [cell] # egy informatív sora az mátrixnak

  @type trees       :: [field]   # a fákat tartalmazó parcellák koordinátái lexikálisan rendezve
  @type puzzle_desc :: {tents_count_rows, tents_count_cols, trees} # a feladványleíró hármas


  @spec to_internal(file::String.t) :: pd::puzzle_desc
  # A file fájlban szövegesen ábrázolt feladvány leírója pd

  @spec get_column_numbers(firstRow::String.t) :: tents_count_cols
  # Az első sor karaktereit számokká alakítja

  @spec get_row_numbers(rawRows::[String.t]) :: tents_count_rows
  # Az sorok elején lévő számokat adja vissza
  
  @spec make_tuples_from_rows(rawRows::[String.t]) :: [useful_row]
  # A mátrixban lévő sorokat leképezi cellákra oszlopszám - karakter párosokkal

  @spec to_trees(rows::[useful_row]) :: [field]
  # A mátrix celláiból megtalálja a * értékűeket és összerendeli a sor - oszlop számot

  defp get_column_numbers(firstRow) do
    firstRow |> String.split |> Enum.map(&String.to_integer/1)
  end

  defp get_row_numbers(rawRows) do
    for row <- rawRows,
      rowCharacters = (row |> String.split),
      do: String.to_integer(List.first(rowCharacters))
  end

  defp make_tuples_from_rows(rawRows) do
    for row <- rawRows,
      rowCharacters = (row |> String.split),
      onlyCharacters = List.delete_at(rowCharacters,0),
      do: Enum.zip(Stream.iterate(1, & &1 + 1), onlyCharacters)
  end

  defp to_trees(rows) do
    for(
      index <- 0..length(rows) - 1,
      row = Enum.at(rows, index),
      cell <- row,
      elem(cell, 1) == "*",
      do: (
        {index + 1, elem(cell, 0)}
      )
    )
  end

  def to_internal(file) do
      input = File.read!(file) |> String.trim

      [head | tail] = input |> String.split("\n")

      columnNumbers = get_column_numbers(head)
      rowNumbers = get_row_numbers(tail)
      rows = make_tuples_from_rows(tail)
      trees = to_trees(rows)

      {rowNumbers, columnNumbers, trees}
    end
  end

  IO.inspect Khf1.to_internal("khf1_f2.txt")
