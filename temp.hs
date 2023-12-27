type State = (String, String, String)
filterStateFuncBySymbol :: String -> String -> [State] -> [String]
filterStateFuncBySymbol state symbol stateFunc = map (\(a, b, c) -> c) (filter (\(a, b, c) -> a == state && b == symbol) stateFunc)
