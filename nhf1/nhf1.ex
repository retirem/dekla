defmodule Nhf1 do

  @moduledoc """
  Kemping
  @author "Reiter Márton marci.reiter501@gmail.com"
  @date   "2022-10-15"
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
  @type extra_tree :: {field, tent_dirs}

  @type possible_dirs :: {field, [dir]} # egy fa pozíciója és a hozzáköthető irányok

  @spec satrak(pd::puzzle_desc) :: tss::[tent_dirs]
  # tss a pd feladványleíróval megadott feladvány összes megoldásának listája, tetszőleges sorrendben

  @spec check_sol(pd::puzzle_desc, ds::tent_dirs) :: boolean()
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
    #{%{err_rows: List.flatten(rows_temp)}, %{err_cols: List.flatten(col_temp)}, %{err_touch: []}}
    length(rows_temp) == 0 && length(col_temp) == 0
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
        # {%{err_rows: badRows}, %{err_cols: badCols}, %{err_touch: sanitizedTouches}}
        length(badRows) == 0 && length(badCols) == 0 && length(sanitizedTouches) == 0
  end


  @spec check_dir(rowNumbers::[integer], colNumbers::[integer], coord::field, trees::trees, val::dir) :: dir

  @spec collision(trees::trees, dirs::tent_dirs) :: boolean()

  @spec solve(extra_trees::[extra_tree], chosen_dirs::tent_dirs, pure_trees::trees) :: [tent_dirs]

  defp check_dir(rowNumbers, colNumbers, coord, trees, val) when val == :n do 
    if elem(coord, 0) > 0 && Enum.at(rowNumbers, elem(coord, 0) - 1) != 0 && Enum.at(colNumbers, elem(coord, 1) - 1) != 0 && !(coord in trees) do
      val
    end
  end

  defp check_dir(rowNumbers, colNumbers, coord, trees, val) when val == :s do 
    if elem(coord, 0) <= length(rowNumbers) && Enum.at(rowNumbers, elem(coord, 0) - 1) != 0 && Enum.at(colNumbers, elem(coord, 1) - 1) != 0 && !(coord in trees) do
      val
    end
  end

  defp check_dir(rowNumbers, colNumbers, coord, trees, val) when val == :e do 
    if elem(coord, 1) <= length(colNumbers) && Enum.at(rowNumbers, elem(coord, 0) - 1) != 0 && Enum.at(colNumbers, elem(coord, 1) - 1) != 0 && !(coord in trees) do
      val
    end
  end

  defp check_dir(rowNumbers, colNumbers, coord, trees, val) when val == :w do 
    if elem(coord, 1) > 0 && Enum.at(rowNumbers, elem(coord, 0) - 1) != 0 && Enum.at(colNumbers, elem(coord, 1) - 1) != 0 && !(coord in trees) do
      val
    end
  end

  defp create_tents(trees, dirs) do
    for(
      index <- 0..length(dirs) - 1,
      dir = Enum.at(dirs, index),
      tree = Enum.at(trees, index),
      do: (
        cond do
          dir == :n ->
            {elem(tree, 0) - 1, elem(tree, 1)}
          dir == :e ->
            {elem(tree, 0), elem(tree, 1) + 1}
          dir == :w ->
            {elem(tree, 0), elem(tree, 1) - 1}
          dir == :s ->
            {elem(tree, 0) + 1, elem(tree, 1)}
          true ->
            {}
        end
      )
    )
  end

  defp collision(trees, dirs) do
    tents = create_tents(trees, dirs |> Enum.reverse() |> tl() |> Enum.reverse())

    dir = Enum.at(dirs, length(dirs) - 1)
    tree = Enum.at(trees, length(dirs) - 1)

    new_pos = cond do
        dir == :n ->
          {elem(tree, 0) - 1, elem(tree, 1)}
        dir == :e ->
          {elem(tree, 0), elem(tree, 1) + 1}
        dir == :w ->
          {elem(tree, 0), elem(tree, 1) - 1}
        dir == :s ->
          {elem(tree, 0) + 1, elem(tree, 1)}
        true ->
          {}
      end
    
    if(new_pos in tents) do
      true
    else
      touches = for(
            tent <- tents,
            do: (
                {x1, y1} = tent;
                {x2, y2} = new_pos
                
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
      length(Enum.filter(touches, fn(x) -> x != nil end)) > 0
    end
  end

  defp solve([], chosen_dirs, _pure_trees) do
    chosen_dirs
  end

  defp solve(extra_trees, chosen_dirs, pure_trees) do
    [tree | tail] = extra_trees
    dirs_for_tree = elem(tree, 1)
    
    for(
      dir <- dirs_for_tree,
      do: (
        if !collision(pure_trees, chosen_dirs ++ [dir]) do
          solve(tail, chosen_dirs ++ [dir], pure_trees)
        else
          []
        end
      )
    )
  end

  def satrak(pd) do
    {rowNumbers, colNumbers, trees} = pd
    
    optional_tent_positions = for(
      tree <- trees,
      do: (
        [check_dir(rowNumbers, colNumbers, {elem(tree, 0) - 1, elem(tree, 1)}, trees, :n), 
        check_dir(rowNumbers, colNumbers, {elem(tree, 0) + 1, elem(tree, 1)}, trees, :s), 
        check_dir(rowNumbers, colNumbers, {elem(tree, 0), elem(tree, 1) + 1}, trees, :e), 
        check_dir(rowNumbers, colNumbers, {elem(tree, 0), elem(tree, 1) - 1}, trees, :w)] |> Enum.uniq |> Enum.reject(&is_nil/1)
      )
    )

    extra_trees = for(
      index <- 0..length(trees) -1,
      tree = Enum.at(trees, index),
      dirs_for_tree = Enum.at(optional_tent_positions, index),
      do: (
        {tree, dirs_for_tree}
      )
    )

    possible_solutions = List.flatten(solve(extra_trees, [], trees)) |> Enum.chunk_every(length(trees))
    Enum.filter(possible_solutions, fn(x) -> check_sol(pd, x) end)
  end
end

IO.inspect Nhf1.satrak({[1, 1, 0, 3, 0], [1, 0, 2, 0, 2], [{1, 2}, {3, 3}, {3, 5}, {5, 1}, {5, 5}]})
