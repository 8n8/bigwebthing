main =
        finiteStateMachine update


update input state =
        case input of
        FileHandle path (Err err) ->
                (GenerateNoiseKeys, state)

        FileHandle path (Ok raw) ->
                if path == staticKeysPath then
                case parseStaticKeys raw of
                Left err ->
                        (panic err, state)

                Right keys ->
                        (

                
