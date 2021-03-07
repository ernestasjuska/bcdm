module GenerateSymbolsCommand

open System
open System.IO
open System.Data.SqlClient
open Microsoft.Dynamics.Nav.CodeAnalysis.SymbolReference
open Microsoft.Dynamics.Nav.CodeAnalysis.Packaging

type SymbolsRole = SourceDatabase | TargetDatabase

type DatabaseConnectionInfo = {
    DatabaseServer: string
    DatabaseName: string
    DatabaseCredential: (string * string) option }

type SymbolsSource =
    | Database of DatabaseConnectionInfo
    | SymbolReferenceFile of string

type GenerateSymbolsArguments = {
    SymbolsRole: SymbolsRole
    SymbolsSource: SymbolsSource
    AppFilePath: string }

let generateLogicalModuleDefinitionFromDatabase (databaseConnectionInfo: DatabaseConnectionInfo) =
    printfn "  Connecting to database..."
    let cs = SqlConnectionStringBuilder()
    cs.DataSource <- databaseConnectionInfo.DatabaseServer
    match databaseConnectionInfo.DatabaseCredential with
    | None ->
        cs.IntegratedSecurity <- true
    | Some (user, password) ->
        cs.UserID <- user
        cs.Password <- password
    cs.InitialCatalog <- databaseConnectionInfo.DatabaseName
    use conn = new SqlConnection(cs.ConnectionString)
    conn.Open()

    printfn "  Generating symbol reference..."
    NavMetadata.readLogicalSymbolsFromDatabase conn

let private generateSymbolPackage: Stream -> NavAppManifest -> ModuleDefinition -> unit = fun navAppPackageStream navAppManifest moduleDefinition ->
    moduleDefinition.AppId <-  navAppManifest.AppId
    moduleDefinition.Name <- navAppManifest.AppName
    moduleDefinition.Publisher <- navAppManifest.AppPublisher
    moduleDefinition.Version <- string navAppManifest.AppVersion
    use navAppPackageWriter = NavAppPackageWriter.Create(navAppPackageStream)
    navAppPackageWriter.WriteString(navAppManifest.ToXml(), "/NavxManifest.xml")
    use symbolReferenceStream = navAppPackageWriter.GetPartStream("/SymbolReference.json")
    SymbolReferenceJsonWriter.WriteModule(symbolReferenceStream, moduleDefinition)

let run (args: GenerateSymbolsArguments) =
    let formAppFilePath appFileName =
        if not (Directory.Exists(args.AppFilePath)) then
            args.AppFilePath
        else
            Path.Combine(args.AppFilePath, appFileName)

    printfn "Running command [Generate symbols]..."
    printfn "  Arguments:"
    printfn "    Symbols role:   %A" args.SymbolsSource
    printfn "    Symbols source: %A" args.SymbolsSource
    match args.SymbolsSource with
    | Database databaseConnectionInfo ->
        printfn "    Database server:  %s" databaseConnectionInfo.DatabaseServer
        printfn "    Database name:    %s" databaseConnectionInfo.DatabaseName
        match databaseConnectionInfo.DatabaseCredential with
        | Some (user, _) -> 
            printfn "    Database user:    %s" user
        | None ->
            ()
    | SymbolReferenceFile symbolReferenceFilePath ->
        printfn "    Symbol reference file: %s" symbolReferenceFilePath
    printfn "    AppFilePath:     %s" (formAppFilePath "<app-file-name>")
    printfn ""

    let prefix, packageAppId, packageName, packagePublisher, packageVersion =
        match args.SymbolsRole with
        | SourceDatabase ->
            "BCDM ",
            Guid.Parse("c100736a-7860-4a80-aa9d-9c972e3e0f71"),
            "DMApp.SourceDatabase",
            "Default Publisher",
            Version(1, 0, 0, 0)
        | TargetDatabase ->
            "",
            Guid.Parse("0fccc92c-51e7-41de-9b7f-112ba9ae9d42"),
            "DMApp.TargetDatabase",
            "Default Publisher",
            Version(1, 0, 0, 0)
    
    let moduleDefinition =
        match args.SymbolsSource with
        | Database databaseConnectionInfo ->
            generateLogicalModuleDefinitionFromDatabase databaseConnectionInfo
        | SymbolReferenceFile symbolReferenceFilePath ->
            raise (NotImplementedException("Cannot generate symbols from symbol reference file yet."))

    if args.SymbolsRole = SourceDatabase then
        printfn "  Updating symbol reference..."
        for table in moduleDefinition.Tables do
            table.Id <- -table.Id.Value
            table.Name <- prefix + table.Name

    printfn "  Saving package information in app manifest..."
    let navAppManifest = NavAppManifest()
    navAppManifest.AppId <- packageAppId
    navAppManifest.AppName <- packageName
    navAppManifest.AppPublisher <- packagePublisher
    navAppManifest.AppVersion <- packageVersion
    navAppManifest.AppCompatibilityId <- typeof<NavAppManifest>.Assembly.GetName().Version

    printfn "  Generating %s app package..." navAppManifest.AppName
    use navAppPackageStream = new MemoryStream()
    generateSymbolPackage navAppPackageStream navAppManifest moduleDefinition
    navAppPackageStream.Seek(0L, SeekOrigin.Begin) |> ignore

    let actualAppFilePath = formAppFilePath (navAppManifest.DefaultPackageName() + ".app")
    printfn "  Writing %s app package to %s..." packageName actualAppFilePath
    use appFileStream = File.OpenWrite(actualAppFilePath)
    appFileStream.SetLength(0L)
    navAppPackageStream.CopyTo(appFileStream)