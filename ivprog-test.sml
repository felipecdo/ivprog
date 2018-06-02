structure IvProgTest =
  struct
    structure CP = IvProgParseFn(IvProgLexer)
      fun tok2s (ID s) = s
        | tok2s (NUM n) = Int.toString n
        | tok2s tok = IvProgTokens.toString tok
      
      (* val calc : TextIO.instream -> Int.int *)
      fun calc instrm = let
        val sm = AntlrStreamPos.mkSourcemap()
        val lex = IvProgLexer.lex sm
        val strm = IvProgLexer.streamifyInstream instrm
        val (r, strm, errs) = CP.parse lex AtomMap.empty strm
        in
          print (String.concatWith "\n"
            (List.map (AntlrRepair.repairToString tok2s sm)
              errs));
          r
        end

end