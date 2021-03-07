type ConsoleCommand =
    | GenerateSymbols of GenerateSymbolsCommand.GenerateSymbolsArguments
    | InvalidCommand of Arguments: string[]

let parseConsoleCommand: string[] -> ConsoleCommand = function
    | [| "generate-symbols"; "--for-source"; "--database-server"; databaseServer; "--database-name"; databaseName; "--app-file"; appFilePath |] ->
        GenerateSymbols {
            SymbolsRole = GenerateSymbolsCommand.SymbolsRole.SourceDatabase
            SymbolsSource = GenerateSymbolsCommand.Database {
                DatabaseServer = databaseServer
                DatabaseName = databaseName
                DatabaseCredential = None }
            AppFilePath = appFilePath }
    | [| "generate-symbols"; "--for-target"; "--database-server"; databaseServer; "--database-name"; databaseName; "--app-file"; appFilePath |] ->
        GenerateSymbols {
            SymbolsRole = GenerateSymbolsCommand.SymbolsRole.TargetDatabase
            SymbolsSource = GenerateSymbolsCommand.Database {
                DatabaseServer = databaseServer
                DatabaseName = databaseName
                DatabaseCredential = None }
            AppFilePath = appFilePath }
    | [| "generate-symbols"; "--for-source"; "--database-server"; databaseServer; "--database-name"; databaseName; "--database-user"; user; "--database-password"; password; "--app-file"; appFilePath |] ->
        GenerateSymbols {
            SymbolsRole = GenerateSymbolsCommand.SymbolsRole.SourceDatabase
            SymbolsSource = GenerateSymbolsCommand.Database {
                DatabaseServer = databaseServer
                DatabaseName = databaseName
                DatabaseCredential = Some (user, password) }
            AppFilePath = appFilePath }
    | [| "generate-symbols"; "--for-target"; "--database-server"; databaseServer; "--database-name"; databaseName; "--database-user"; user; "--database-password"; password; "--app-file"; appFilePath |] ->
        GenerateSymbols {
            SymbolsRole = GenerateSymbolsCommand.SymbolsRole.TargetDatabase
            SymbolsSource = GenerateSymbolsCommand.Database {
                DatabaseServer = databaseServer
                DatabaseName = databaseName
                DatabaseCredential = Some (user, password) }
            AppFilePath = appFilePath }
    | arguments ->
        InvalidCommand arguments

[<EntryPoint>]
let main argv =
    match parseConsoleCommand argv with
    | GenerateSymbols args ->
        GenerateSymbolsCommand.run args
    | InvalidCommand arguments ->
        failwithf "Invalid command %A" arguments    
    0
