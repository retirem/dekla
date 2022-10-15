defmodule Khf3 do

  @moduledoc """
  Kemping helyessége
  @author "Reiter Márton marci.reiter501@gmail.com"
  @date   "2022-10-06"
  ...
  """

  @type row   :: integer    # sor száma (1-től n-ig)
  @type col   :: integer    # oszlop száma (1-től m-ig)
  @type field :: {row, col} # egy parcella koordinátái

  @type tents_count_rows :: [integer] # a sátrak száma soronként
  @type tents_count_cols :: [integer] # a sátrak száma oszloponként

  @type trees       :: [field]   # a fákat tartalmazó parcellák koordinátái lexikálisan rendezve
  @type puzzle_desc :: {tents_count_rows, tents_count_cols, trees} # a feladványleíró hármas

  @type dir       :: :n | :e | :s | :w # a sátorpozíciók iránya: north, east, south, west
  @type tent_dirs :: [dir]             # a sátorpozíciók irányának listája a fákhoz képest

  @type cnt_tree  :: integer                         # a fák száma a kempingben
  @type cnt_tent  :: integer                         # az elemek száma a sátorpozíciók irányának listájában
  @type err_rows  :: %{err_rows:  [integer]}         # a sátrak száma rossz a felsorolt sorokban
  @type err_cols  :: %{err_cols:  [integer]}         # a sátrak száma rossz a felsorolt oszlopokban
  @type err_touch :: %{err_touch: [field]}           # a felsorolt koordinátájú sátrak másikat érintenek
  @type errs_desc :: {err_rows, err_cols, err_touch} # hibaleíró hármas

  @spec check_sol(pd::puzzle_desc, ds::tent_dirs) :: ed::errs_desc
  # Az {rs, cs, ts} = pd feladványleíró és a ds sátorirány-lista
  # alapján elvégzett ellenőrzés eredménye a cd hibaleíró, ahol
  #   rs a sátrak soronként elvárt számának a listája,
  #   cs a sátrak oszloponként elvárt számának a listája,
  #   ts a fákat tartalmazó parcellák koordinátájának a listája
  # Az {e_rows, e_cols, e_touch} = ed hármas elemei olyan
  # kulcs-érték párok, melyekben a kulcs a hiba jellegére utal, az
  # érték pedig a hibahelyeket felsoroló lista (üres, ha nincs hiba)

  @spec summary(numbers::[integer], row::boolean(), tents::[field]) :: [integer]
  # Összegzi az adott sorban vagy oszlopban található sátrak számát és
  # visszaadja egy listában

  @spec compare(numbers::[integer], summary::[integer]) :: [integer]
  # Összehasonlítja a várt és a kapott sátor mennyiséget adott sorban/oszlopban
  # és visszaadja a nem egyező sor- vagy oszlopszámokat

  defp compare(numbers, summary) do 
    for(
        index <- 0..length(numbers) - 1,
        num = Enum.at(numbers, index),
        sum = Enum.at(summary, index),
        num != sum,
        do: (
            index + 1
        )
    )
  end

  defp summary(numbers, row, tents) do
    for(
        index <- 0..length(numbers) - 1,
        num = Enum.at(numbers, index),
        do: (
            if num >= 0 do
                length(tents |> Enum.filter(&(elem(&1, row && 0 || 1) == index + 1)))
            else
                num
            end
        )
    )
  end

  def check_sol(pd, []) do 
    {rowNumbers, colNumbers, _} = pd
    rows_temp = for(
                    rowIndex <- 0..length(rowNumbers) - 1,
                    item = Enum.at(rowNumbers, rowIndex),
                    item != 0,
                    do: (
                        rowIndex + 1
                    )
                )

    col_temp = for(
                    colIndex <- 0..length(colNumbers) - 1,
                    item = Enum.at(colNumbers, colIndex),
                    item != 0,
                    do: (
                        colIndex + 1
                    )
                )
    {%{err_rows: List.flatten(rows_temp)}, %{err_cols: List.flatten(col_temp)}, %{err_touch: []}}
  end

  def check_sol(pd, ds) do
        {rowNumbers, colNumbers, treeCells} = pd

        tents = for(
            treeIndex <- 0..length(treeCells) - 1,
            tree = Enum.at(treeCells, treeIndex),
            dir = Enum.at(ds, treeIndex),
            do: (
                cond do
                    dir == :w ->
                        {elem(tree, 0), elem(tree, 1) - 1}
                    dir == :e ->
                        {elem(tree, 0), elem(tree, 1) + 1}
                    dir == :s ->
                        {elem(tree, 0) + 1, elem(tree, 1)}
                    dir == :n ->
                        {elem(tree, 0) - 1, elem(tree, 1) }
                end
            )
        )

        rowSum = summary(rowNumbers, true, tents)
        colSum = summary(colNumbers, false, tents)
        badRows = compare(rowNumbers, rowSum)
        badCols = compare(colNumbers, colSum)
        
        touches = for(
            tent <- tents,
            tent2 <- tents,
            tent != tent2,
            do: (
                {x1, y1} = tent;
                {x2, y2} = tent2
                
                if (x1 + 1 == x2 && y1 == y2) ||  
                (x1 - 1 == x2 && y1 == y2) ||
                (y1 + 1 == y2 && x1 == x2) ||
                (y1 - 1 == y2 && x1 == x2) ||
                (x1 + 1 == x2 && y1 + 1 == y2) ||
                (x1 + 1 == x2 && y1 - 1 == y2) ||
                (x1 - 1 == x2 && y1 + 1 == y2) ||
                (x1 - 1 == x2 && y1 - 1 == y2) do
                    tent
                end
            ) 
        )
        sanitizedTouches = touches |> Enum.uniq |> Enum.reject(&is_nil/1) |> Enum.sort_by(&(elem(&1, 0)))
        {%{err_rows: badRows}, %{err_cols: badCols}, %{err_touch: sanitizedTouches}}
  end
end
