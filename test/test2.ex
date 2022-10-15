
defmodule Asd do

   @spec dec2hex(d::integer) :: h::charlist

    @spec doDiv(num::integer, chars::charlist) :: charlist

    @spec decodeRem(r::integer) :: char

    def decodeRem(r) do
        a = Enum.at('123456789ABCDEF', r - 1)
        ?a
    end

    def doDiv(0, chars) do
        chars
    end

    def doDiv(num, chars) when num > 0 do
        IO.inspect(num)
        doDiv(trunc(num / 16), Enum.concat(chars, decodeRem(rem(num, 16))))
    end

   def dec2hex(d) do
        chars = doDiv(d, '')
        IO.inspect chars
        reverted_chars = List.flatten(chars) |> Enum.reverse(chars)
        if rem(length(reverted_chars), 2) == 1 do
            '0' ++ reverted_chars
        else
            reverted_chars
        end
   end
end

IO.inspect Asd.dec2hex(443)