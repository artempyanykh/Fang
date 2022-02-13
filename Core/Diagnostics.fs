module Fang.Diagnostics

type UntypedParseError = UntypedParseError of obj

let parse_errors: ResizeArray<UntypedParseError> = ResizeArray ()

let reset_parse_errors () : unit =
    parse_errors.Clear()
    
let add_parse_error (e: UntypedParseError) : unit =
    parse_errors.Add(e)
