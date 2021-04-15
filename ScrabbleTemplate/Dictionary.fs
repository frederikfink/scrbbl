// Insert your Dictionary.fsi file here. All modules must be internal.

module internal Dictionary

    type Dict = Dict of List<string> // Not implemented

    // If you have made a lookup function you may include it here, but it will be generated for you by the project.

    let empty () = Dict List.empty
    let lookup s (Dict d) = List.contains s d
    let insert s (Dict d) = Dict (s::d)


    // Only implement reverse if you have made a Gaddag
    let reverse : Dict -> (bool * Dict) option = fun _ -> failwith "Not implemented"