structure Temp =
struct
  type temp = int
  val temps = ref 0
  fun newTemp() =
    let val t = !temps in
      temps := t + 1; t
  end

  type label = Symbol.symbol
  local structure F = Format
    val labelIdx = ref 0
    fun postInc x =
      let val i = !x in
        x := i + 1; i
    end
  in
    fun newLabel() =
      Symbol.createSymbol(F.format "L%d" [F.INT(postInc labelIdx)])
  end
end
