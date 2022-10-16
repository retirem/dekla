defmodule Nhf1 do

  @moduledoc """
  Kemping
  @author "Reiter Márton marci.reiter501@gmail.com"
  @date   "2022-10-16"
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

  @spec satrak(pd::puzzle_desc) :: tss::[tent_dirs]
  # tss a pd feladványleíróval megadott feladvány összes megoldásának listája, tetszőleges sorrendben.

  @spec check_sol(pd::puzzle_desc, dirs::tent_dirs) :: boolean()
  # Az {rs, cs, ts} = pd feladványleíró és a ds sátorirány-lista
  # alapján elvégzett ellenőrzés eredménye a cd hibaleíró, ahol
  #   rs a sátrak soronként elvárt számának a listája,
  #   cs a sátrak oszloponként elvárt számának a listája,
  #   ts a fákat tartalmazó parcellák koordinátájának a listája
  # Az {e_rows, e_cols, e_touch} = ed hármas elemei olyan
  # kulcs-érték párok, melyekben a kulcs a hiba jellegére utal, az
  # érték pedig a hibahelyeket felsoroló lista (üres, ha nincs hiba).

  @spec summary(numbers::[integer], row::boolean(), tents::[field]) :: [integer]
  # Összegzi az adott sorban vagy oszlopban található sátrak számát és
  # visszaadja egy listában.

  @spec compare_lists(numbers::[integer], summary::[integer]) :: [integer]
  # Összehasonlítja a várt és a kapott sátor mennyiséget adott sorban/oszlopban
  # és visszaadja a nem egyező sor- vagy oszlop indexét.

  @spec check_dir(rowNumbers::[integer], colNumbers::[integer], tent::field, trees::trees, dir::dir) :: dir
  # Megvizsgálja, hogy a kapott dir opcionális iránya-e a tent-nek. Tehát nem 
  # ütközik másik fával, és nem megy ki a mezőről a sátor, ha ide tesszük.

  @spec collision(new_tent::field, current_tents::[field]) :: boolean()
  # Kiszámolja a fákhoz tartozó sátrak helyét, amennyiben létezik a fához irány, majd megnézi, 
  # hogy a legutolsó sátor pozíciója ütközik-e valamelyik másik sátoréval.

  @spec solve(trees::trees, rowNumbers::[integer], colNumbers::[integer], chosen_dirs::tent_dirs, pure_trees::trees, current_tents::[field]) :: [tent_dirs]
  # Segédfüggvénye a satrak-nak. Rekurzívan végigmegy a fák helyzetén, és a hozzájuk tartozó irányokon. 
  # Minden kombinációt letesztel, majd ha üres listára jut, mert végig ért az összes fán és 
  # mindegyikhez talált egy lehetséges irányt, akkor visszaadja a kigyűjtött irányokat. Ha valahol 
  # olyan irányra lépett, ami ütközéshez vezet, akkor üres listát ad vissza.

  @spec calculate_dirs(rowNumbers::[integer], colNumbers::[integer], tree::field, trees::trees) :: tent_dirs 
  # Egy fához tartozó lehetséges irányok listáját adja meg, figyelembe véve, hogy ne menjen ki 
  # a jövőbeli sátor a mezőről, illetve, hogy egy sorba és oszlopba csak akkor lehessen letenni egy sátrat, 
  # ha van még hely.

  # A függvény összehasonlít két integer listát, és visszaadja azokat az indexeket,
  # ahol eltérés van a két listában.
  defp compare_lists(numbers, summary) do 
    for(
        index <- 0..length(numbers) - 1,
        num = Enum.at(numbers, index),
        sum = Enum.at(summary, index),
        num != sum,
        do: (
            index
        )
    )
  end

  # Ha az adott sorra/oszlopra meg van adva nem negatív 
  # sátorszám korlát, akkor összegzi az adott sorban/oszlopban 
  # lévő sátrak számát. Egyébként visszaadja a korlátot.
  defp summary(numbers, row, tents) do
    for(
        index <- 0..length(numbers) - 1,
        num = Enum.at(numbers, index),
        do: (
            if num >= 0 do
                length(Enum.filter(tents, fn(x) -> elem(x, row && 0 || 1) == (index + 1) end))
            else
                num
            end
        )
    )
  end

  # Megvizsgálja, hogy a kapott puzzle_desc és a hozzá tartozó 
  # irányok által elkészített elrendezés helyes-e.
  defp check_sol(pd, dirs) do
        {rowNumbers, colNumbers, treeCells} = pd

        tents = for(
            treeIndex <- 0..length(treeCells) - 1,
            tree = Enum.at(treeCells, treeIndex),
            dir = Enum.at(dirs, treeIndex),
            do: (
              create_tent(tree, dir)
            )
        )

        rowSum = summary(rowNumbers, true, tents)
        colSum = summary(colNumbers, false, tents)
        badRows = compare_lists(rowNumbers, rowSum)
        badCols = compare_lists(colNumbers, colSum)
        
        touches = for(
            tent <- tents,
            tent2 <- tents,
            tent != tent2,
            do: (
                {x1, y1} = tent;
                {x2, y2} = tent2
                
                if abs(x1 - x2) <= 1 && abs(y1 - y2) <= 1 do
                    tent
                end
            ) 
        )
        sanitizedTouches = touches |> Enum.uniq |> Enum.reject(&is_nil/1)
        length(badRows) == 0 && length(badCols) == 0 && length(sanitizedTouches) == 0
  end

  # Vizsgálja, hogy a dir-nek megfelelően elhelyezett sátor, 
  # helyesen van-e elhelyezve. Azaz nem üktözik semmivel, 
  # nem lóg ki a mezőről. Ha helyes az elhelyezés, 
  # visszaadja az irányt, egyébként nil-t.
  defp check_dir(rowNumbers, colNumbers, tent, trees, dir) when dir == :n do 
    if elem(tent, 0) > 0 &&
      Enum.at(rowNumbers, elem(tent, 0) - 1) != 0 &&
      Enum.at(colNumbers, elem(tent, 1) - 1) != 0 &&
      !(tent in trees) do
        dir
    end
  end

  # Vizsgálja, hogy a dir-nek megfelelően elhelyezett sátor, 
  # helyesen van-e elhelyezve. Azaz nem üktözik semmivel, 
  # nem lóg ki a mezőről. Ha helyes az elhelyezés, 
  # visszaadja az irányt, egyébként nil-t.
  defp check_dir(rowNumbers, colNumbers, tent, trees, dir) when dir == :s do 
    if elem(tent, 0) <= length(rowNumbers) &&
      Enum.at(rowNumbers, elem(tent, 0) - 1) != 0 &&
      Enum.at(colNumbers, elem(tent, 1) - 1) != 0 &&
      !(tent in trees) do
        dir
    end
  end

  # Vizsgálja, hogy a dir-nek megfelelően elhelyezett sátor, 
  # helyesen van-e elhelyezve. Azaz nem üktözik semmivel, 
  # nem lóg ki a mezőről. Ha helyes az elhelyezés, 
  # visszaadja az irányt, egyébként nil-t.
  defp check_dir(rowNumbers, colNumbers, tent, trees, dir) when dir == :e do 
    if elem(tent, 1) <= length(colNumbers) &&
      Enum.at(rowNumbers, elem(tent, 0) - 1) != 0 &&
      Enum.at(colNumbers, elem(tent, 1) - 1) != 0 && 
      !(tent in trees) do
        dir
    end
  end

  # Vizsgálja, hogy a dir-nek megfelelően elhelyezett sátor, 
  # helyesen van-e elhelyezve. Azaz nem üktözik semmivel, 
  # nem lóg ki a mezőről. Ha helyes az elhelyezés, 
  # visszaadja az irányt, egyébként nil-t.
  defp check_dir(rowNumbers, colNumbers, tent, trees, dir) when dir == :w do 
    if elem(tent, 1) > 0 && 
      Enum.at(rowNumbers, elem(tent, 0) - 1) != 0 &&
      Enum.at(colNumbers, elem(tent, 1) - 1) != 0 &&
      !(tent in trees) do
        dir
    end
  end

  # Vizsgálja, hogy a kapott új sátor, ütközik-e a már 
  # elhelyezett sátrak valamelyikével.
  defp collision(new_tent, current_tents) do
    if(new_tent in current_tents) do
      true
    else
      touches = for(
            tent <- current_tents,
            {x2, y2} = new_tent,
            do: (
                {x1, y1} = tent;
                if abs(x1 - x2) <= 1 && abs(y1 - y2) <= 1 do
                    tent
                end
            ) 
        )
      length(Enum.filter(touches, fn(x) -> x != nil end)) > 0
    end
  end

  
  # A kapott fa és irány alapján előállít egy sátrat.
  defp create_tent(tree, dir) do
    cond do
      dir === :n ->
        {elem(tree, 0) - 1, elem(tree, 1)}
      dir == :s ->
        {elem(tree, 0) + 1, elem(tree, 1)}
      dir == :e ->
        {elem(tree, 0), elem(tree, 1) + 1}
      dir == :w ->
        {elem(tree, 0), elem(tree, 1) - 1}
      true ->
        {}
    end
  end

  
  # Visszaadja azoknak az irányoknak a listáját, amik szóba 
  # jöhetnek a tree fa helyzetéhez.
  defp calculate_dirs(rowNumbers, colNumbers, tree, trees) do
    [check_dir(rowNumbers, colNumbers, create_tent(tree, :n), trees, :n), 
    check_dir(rowNumbers, colNumbers, create_tent(tree, :s), trees, :s), 
    check_dir(rowNumbers, colNumbers, create_tent(tree, :e), trees, :e), 
    check_dir(rowNumbers, colNumbers, create_tent(tree, :w), trees, :w)] 
    |> Enum.uniq |> Enum.reject(fn(x) -> is_nil(x) end)
  end

  
  # A solve azon verziója, amikor már elfogytak az ellenőrizendő fák, 
  # tehát megoldásra jutottunk, így visszaadja a már kiválasztott irányokat.
  defp solve([], _rowNumbers, _colNumbers, chosen_dirs, _pure_trees, _current_tents) do
    chosen_dirs
  end

  
  # A satrak segdéfüggvénye, lényegében ez végzi az algoritmus futtatását rekurzívan. 
  # Kiszámítja adott fához a lehetséges irányokat, figyelembe véve a már elhelyezett 
  # sátrakat is, majd minden lehetséges irányra továbbhívja saját magát, átadva 
  # az eddig nem vizsgált fákat, a már elhelyezett sátrakat, és csökkentve a 
  # sor/oszlop korlátokat az újonnan elhelyezett sátor helyén.
  defp solve(trees, rowNumbers, colNumbers, chosen_dirs, pure_trees, current_tents) do
    [tree | tail] = trees
    
    dirs_for_tree = calculate_dirs(rowNumbers, colNumbers, tree, pure_trees) 
    |> Enum.filter(fn(x) -> !collision(create_tent(tree, x), current_tents) end)

    for(
      dir <- dirs_for_tree,
      do: (
        tent = create_tent(tree, dir);
        solve(tail, 
        List.replace_at(rowNumbers, elem(tent, 0) - 1, Enum.at(rowNumbers, elem(tent, 0) - 1) - 1),
        List.replace_at(colNumbers, elem(tent, 1) - 1, Enum.at(colNumbers, elem(tent, 1) - 1) - 1),
        [dir | chosen_dirs], pure_trees, [tent | current_tents])
      )
    )
  end

  # Előállítja a kapott puzzle_desc-hez a megoldásokat.
  def satrak(pd) do
    {rowNumbers, colNumbers, trees} = pd
    List.flatten(solve(trees, rowNumbers, colNumbers, [], trees, [])) 
    |> Enum.reverse 
    |> Enum.chunk_every(length(trees)) 
    |> Enum.filter(fn(x) -> check_sol(pd, x) end)
  end
end
