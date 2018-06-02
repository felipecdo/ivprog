structure CalcTest =
  struct
    structure CP = CalcParseFn(CalcLexer)
      fun tok2s (ID s) = s
        | tok2s (NUM n) = Int.toString n
        | tok2s tok = CalcTokens.toString tok
      
      (* val calc : TextIO.instream -> Int.int *)
      fun calc instrm = let
        val sm = AntlrStreamPos.mkSourcemap()
        val lex = CalcLexer.lex sm
        val strm = CalcLexer.streamifyInstream instrm
        val (r, strm, errs) = CP.parse lex AtomMap.empty strm
        in
          print (String.concatWith "\n"
            (List.map (AntlrRepair.repairToString tok2s sm)
              errs));
          r
        end

end