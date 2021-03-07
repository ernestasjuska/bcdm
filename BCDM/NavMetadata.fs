module NavMetadata

open System
open System.Linq
open System.IO
open System.IO.Compression
open System.Xml
open System.Xml.Linq
open System.Xml.XPath
open System.Data.SqlClient
open Microsoft.Dynamics.Nav.CodeAnalysis.SymbolReference

type private TableFieldType =
    | Integer
    | Decimal
    | Boolean
    | Date
    | Time
    | DateTime
    | Text of MaxLength: int
    | Code of MaxLength: int
    | Binary of MaxLength: int
    | Option of Values: string[]
    | Blob
    | DateFormula
    | GUID
    | Media
    | MediaSet
    | BigInteger
    | RecordId
    | Duration
    | OemText
    | TableFilter

let private deflate compressedStream decompressedStream =
    use deflateStream = new DeflateStream(compressedStream, CompressionMode.Decompress)
    deflateStream.CopyTo(decompressedStream)

let private decompressNavStream (compressedStream: Stream) =
    compressedStream.Seek(4L, SeekOrigin.Begin) |> ignore
    let decompressedStream = new MemoryStream()
    deflate compressedStream decompressedStream
    decompressedStream.Seek(0L, SeekOrigin.Begin) |> ignore
    decompressedStream

let private readNavMetadata (reader: SqlDataReader) (columnNo: int) =
    use compressedMetadataStream = reader.GetStream(columnNo)
    use metadataStream = decompressNavStream compressedMetadataStream
    XDocument.Load(metadataStream)

let private xmlNamespaceManager = XmlNamespaceManager(NameTable())
xmlNamespaceManager.AddNamespace("m", "urn:schemas-microsoft-com:dynamics:NAV:MetaObjects")

let private readMetaTableFieldType (field: XElement) =
    let fieldDataTypeName = field.Attribute(XName.Get("Datatype")).Value
    let fieldDataLength =
        match field.Attribute(XName.Get("DataLength")) with
        | null -> 0
        | a -> int a.Value
    let fieldOptionValues = 
        match field.Attribute(XName.Get("OptionString")) with
        | null -> [||]
        | a -> a.Value.Split(',')
    match fieldDataTypeName, fieldDataLength, fieldOptionValues with
    | "Integer", _, _ -> Integer
    | "Decimal", _, _ -> Decimal
    | "Boolean", _, _ -> Boolean
    | "Date", _, _ -> Date
    | "Time", _, _ -> Time
    | "DateTime", _, _ -> DateTime
    | "Text", n, _ -> Text n
    | "Code", n, _ -> Code n
    | "Binary", n, _ -> Binary n
    | "Option", _, xs -> Option xs
    | "BLOB", _, _ -> Blob
    | "DateFormula", _, _ -> DateFormula
    | "GUID", _, _ -> GUID
    | "Media", _, _ -> Media
    | "MediaSet", _, _ -> MediaSet
    | "BigInteger", _, _ -> BigInteger
    | "RecordID", _, _ -> RecordId
    | "Duration", _, _ -> Duration
    | "OemText", _, _ -> OemText
    | "TableFilter", _, _ -> TableFilter
    | t, _, _ -> failwithf "Unknown data type %s." t

let private convertTableFieldTypeToTypeDefinition (``type``: TableFieldType) =
    let mkTypeDefinition: (TypeDefinition -> TypeDefinition)-> string -> TypeDefinition = fun f name ->
        let typeDefinition = TypeDefinition()
        typeDefinition.Name <- name
        f typeDefinition
    let mkSimpleTypeDefinition = mkTypeDefinition id
    let mkLengthTypeDefinition n = mkTypeDefinition (fun t -> t.Name <- sprintf "%s[%i]" t.Name n; t)
    let mkOptionTypeDefinition xs = mkTypeDefinition (fun t -> t.OptionMembers <- xs; t)
    match ``type`` with
    | Integer -> mkSimpleTypeDefinition "Integer"
    | Decimal -> mkSimpleTypeDefinition "Decimal"
    | Boolean -> mkSimpleTypeDefinition "Boolean"                          
    | Date -> mkSimpleTypeDefinition "Date"
    | Time -> mkSimpleTypeDefinition "Time"
    | DateTime -> mkSimpleTypeDefinition "DateTime"   
    | Text n -> mkLengthTypeDefinition n "Text" 
    | Code n -> mkLengthTypeDefinition n "Code"
    | Binary n -> mkLengthTypeDefinition n "Binary"
    | Option xs -> mkOptionTypeDefinition xs "Option"
    | Blob -> mkSimpleTypeDefinition "Blob"
    | DateFormula -> mkSimpleTypeDefinition "DateFormula"
    | GUID -> mkSimpleTypeDefinition "Guid"
    | Media -> mkSimpleTypeDefinition "Media"
    | MediaSet -> mkSimpleTypeDefinition "MediaSet"
    | BigInteger -> mkSimpleTypeDefinition "BigInteger"
    | RecordId -> mkSimpleTypeDefinition "RecordId"
    | Duration -> mkSimpleTypeDefinition "Duration"
    | OemText -> mkSimpleTypeDefinition "OemText"
    | TableFilter -> mkSimpleTypeDefinition "TableFilter"

let private readMetaTable (metadata: XDocument) =
    metadata.Save(@"c:\users\ernjus\desktop\t.xml")
    // TODO: See if NAV2013 has TableType attribute
    match List.ofSeq (metadata.XPathSelectElements("m:MetaTable[(@TableType='Normal' or not(@TableType)) and not(@IsVirtual='1') and not(@LinkedObject='1')]", xmlNamespaceManager)) with
    | [metaTable] ->
        let tableDefinition = TableDefinition()
        tableDefinition.Id <- int (metaTable.Attribute(XName.Get("ID")).Value)
        tableDefinition.Name <- metaTable.Attribute(XName.Get("Name")).Value
        tableDefinition.Properties <- [|
            let dataPerCompanyPropertyDefinition = PropertyDefinition()
            dataPerCompanyPropertyDefinition.Name <- "DataPerCompany"
            dataPerCompanyPropertyDefinition.Value <- metaTable.Attribute(XName.Get("DataPerCompany")).Value
            yield dataPerCompanyPropertyDefinition |]
        tableDefinition.Fields <- [|
            for field in metaTable.XPathSelectElements("m:Fields/m:Field[(@Enabled='1' or not(@Enabled)) and @FieldClass='Normal']", xmlNamespaceManager) do
                let fieldDefinition = FieldDefinition()
                fieldDefinition.Id <- int (field.Attribute(XName.Get("ID")).Value)
                fieldDefinition.Name <- field.Attribute(XName.Get("Name")).Value
                fieldDefinition.TypeDefinition <- readMetaTableFieldType field |> convertTableFieldTypeToTypeDefinition
                fieldDefinition.Properties <- [|
                    let appIdPropertyDefinition = PropertyDefinition()
                    appIdPropertyDefinition.Name <- "AppId"
                    appIdPropertyDefinition.Value <-
                        match field.Attribute(XName.Get("SourceAppId")) with
                        | null -> string Guid.Empty
                        | appId -> appId.Value
                    yield appIdPropertyDefinition |]
                yield fieldDefinition |]
        tableDefinition.Keys <- [|
            let keyDefinition = KeyDefinition()
            keyDefinition.Name <- "PK"
            keyDefinition.FieldNames <-
                metaTable.XPathSelectElement("m:Keys/m:Key[1]", xmlNamespaceManager).Attribute(XName.Get("Key")).Value.Replace("Field", "").Split(",")
                |> Array.map (fun i -> tableDefinition.Fields.First(fun f -> f.Id.Value = int i).Name)
            yield keyDefinition |]
        Some tableDefinition
    | _ ->
        None

let private readTableExtensionMetadata (metadata: XDocument) (targetName: string) =
    let delta = metadata.XPathSelectElement("m:MetadataRuntimeDeltas", xmlNamespaceManager)
    let tableExtensionDefinition = TableExtensionDefinition()
    tableExtensionDefinition.Id <- int (delta.Attribute(XName.Get("ID")).Value)
    tableExtensionDefinition.Name <- delta.Attribute(XName.Get("Name")).Value
    tableExtensionDefinition.TargetObject <- targetName
    tableExtensionDefinition.Fields <- [|
        for field in delta.XPathSelectElements("m:FieldAdd[(@Enabled='1' or not(@Enabled)) and @FieldClass='Normal']", xmlNamespaceManager) do
            let fieldDefinition = FieldDefinition()
            fieldDefinition.Id <- int (field.Attribute(XName.Get("ID")).Value)
            fieldDefinition.Name <- field.Attribute(XName.Get("Name")).Value
            fieldDefinition.TypeDefinition <- readMetaTableFieldType field |> convertTableFieldTypeToTypeDefinition
            fieldDefinition.Properties <- [|
                let appIdPropertyDefinition = PropertyDefinition()
                appIdPropertyDefinition.Name <- "AppId"
                appIdPropertyDefinition.Value <- field.Attribute(XName.Get("SourceAppId")).Value
                yield appIdPropertyDefinition |]
            yield fieldDefinition |]
    tableExtensionDefinition

let private readTableDefinitionsFromDatabase (sqlConnection: SqlConnection) = seq {
    use cmd = sqlConnection.CreateCommand()
    cmd.CommandText <-
        "SELECT [Metadata] FROM [Object Metadata] WHERE [Object Type] = 1 AND [Object Subtype] = 'Normal' AND [Metadata] IS NOT NULL
         IF OBJECT_ID('[Application Object Metadata]') IS NOT NULL
            SELECT [Metadata] FROM [Application Object Metadata] WHERE [Object Type] = 1 AND [Object Subtype] = 'Normal' AND [Metadata] IS NOT NULL"
    use r = cmd.ExecuteReader()
    while r.HasRows do
        while r.Read() do
            let metadata = readNavMetadata r 0
            match readMetaTable metadata with
            | Some tableDefinition ->
                yield tableDefinition
            | _ ->
                ()
        r.NextResult() |> ignore }

let private readTableExtensionDefinitionsFromDatabase (sqlConnection: SqlConnection) (getTableDefinition: int -> TableDefinition option) = seq {
    use cmd = sqlConnection.CreateCommand()
    cmd.CommandText <-
        "IF OBJECT_ID('[Application Object Metadata]') IS NOT NULL
            SELECT [Metadata], [Object Subtype] FROM [Application Object Metadata] WHERE [Object Type] = 15 AND [Metadata] IS NOT NULL"
    use r = cmd.ExecuteReader()
    while r.Read() do
        let metadata = readNavMetadata r 0
        let targetTableId = int (r.GetString(1))
        match getTableDefinition targetTableId with
        | Some tableDefinition ->
            let tableExtensionDefinition = readTableExtensionMetadata metadata tableDefinition.Name
            yield tableExtensionDefinition
        | _ ->
            () }

let private readLogicalTableDefinitionsFromDatabase (sqlConnection: SqlConnection) =
    let tableDefinitions = readTableDefinitionsFromDatabase sqlConnection |> List.ofSeq
    let tableExtensionDefinitions = readTableExtensionDefinitionsFromDatabase sqlConnection (fun id -> tableDefinitions |> List.tryFind (fun td -> td.Id.Value = id))
    
    for (tableName, tableExtensionDefinitions) in tableExtensionDefinitions |> Seq.groupBy (fun ted -> ted.TargetObject) do
        match tableDefinitions |> List.tryFind (fun td -> td.Name = tableName) with
        | Some tableDefinition ->
            let extensionFields = tableExtensionDefinitions |> Seq.collect (fun ted -> ted.Fields) |> Array.ofSeq
            tableDefinition.Fields <-
                tableDefinition.Fields
                |> Array.append extensionFields
                |> Array.sortBy (fun f -> f.Id.Value)
        | None ->
            ()
    tableDefinitions

let readLogicalSymbolsFromDatabase (sqlConnection: SqlConnection) =
    let moduleDefinition = ModuleDefinition()
    moduleDefinition.Tables <- readLogicalTableDefinitionsFromDatabase sqlConnection |> Array.ofList
    moduleDefinition