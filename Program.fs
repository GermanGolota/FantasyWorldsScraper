open System
open System.Net
open System.IO
open HtmlAgilityPack
open System.Collections.Generic
open System.IO.Compression
open Cocona
open Microsoft.Extensions.DependencyInjection
open System.Net.Http

type UrlType =
    | AuthorPage of authorId: int
    | DownloadUrl of bookId: int

type Book = { name: string; id: int }

type Series = { name: string; books: Book list }

let fantasyWorldsUrl path = $"https://fantasy-worlds.net/{path}"

let createUrl url =
    match url with
    | AuthorPage id -> $"author/id{id}"
    | DownloadUrl id -> $"lib/id{id}/download"
    |> fantasyWorldsUrl

let loadHtmlDocument (url: UrlType) =
    let web = new HtmlWeb()
    createUrl url |> web.Load

let isMainNode (node: HtmlNode) = node.HasClass "news_body"

let optionParse (str: String) =
    match Int32.TryParse str with
    | true, value -> Some value
    | _ -> None


let tryParseBookId (bookUrl: String) =
    bookUrl.Replace("/lib/id", "").Replace("/", "")
    |> optionParse

type AuthorListItemType =
    | BookItem of Book
    | SeriesItem of string

let parseSeriesName (node: HtmlNode) =
    node.Descendants("a")
    |> List.ofSeq
    |> List.tryExactlyOne
    |> Option.map (fun linkNode -> linkNode.InnerHtml)

let noneStrToNone str =
    if "str" = "none" then
        None
    else
        Some str

let parseBook (node: HtmlNode) =
    match
        node.GetAttributeValue("href", "none")
        |> noneStrToNone
        with
    | Some url ->
        match tryParseBookId url with
        | Some bookId ->
            match node.Descendants("h3")
                  |> List.ofSeq
                  |> List.tryExactlyOne
                with
            | Some titleNode ->
                Some
                    { name = titleNode.InnerHtml
                      id = bookId }
            | None -> None
        | None -> None

    | None -> None

let parseAuthorItemNode (node: HtmlNode) =
    match node.Name with
    | "h2" ->
        parseSeriesName node
        |> Option.map (fun value -> SeriesItem(value))
    | "a" ->
        parseBook node
        |> Option.map (fun value -> BookItem(value))
    | _ -> None

let processItemsList items : Series list =
    let mutable currentSeriesName: string = "Без серии"
    let seriesDict = new Dictionary<string, Book list>()

    for item in items do
        match item with
        | SeriesItem name -> currentSeriesName <- name
        | BookItem book ->
            match seriesDict.TryGetValue currentSeriesName with
            | true, bookList ->
                let updatedBookList = book :: bookList
                seriesDict.Remove currentSeriesName |> ignore
                seriesDict.Add(currentSeriesName, updatedBookList)
            | false, _ -> seriesDict.Add(currentSeriesName, [ book ])

    seriesDict
    |> List.ofSeq
    |> List.map (fun pair -> { name = pair.Key; books = pair.Value })

let downloadFile (client: HttpClient) (url: String) (folder: String) (fileName: String) =
    let fileLocation = Path.Combine(folder, fileName)

    async {
        let! response = client.GetAsync(url) |> Async.AwaitTask
        use fs = new FileStream(fileLocation, FileMode.Create)

        let! _ =
            response.Content.CopyToAsync(fs)
            |> Async.AwaitTask

        return fileLocation
    }


let downloadBookArchieve (client: HttpClient) (folder: String) bookId bookName =
    let bookFileName = $"{bookName}.zip"
    let url = DownloadUrl(bookId) |> createUrl
    downloadFile client url folder bookFileName

let downloadBook seriesLocation downloaderBase (book: Book) =
    printfn "Downloading '%s'" book.name
    let downloader = downloaderBase seriesLocation
    Directory.CreateDirectory seriesLocation |> ignore

    async {
        let! archieveLocation = downloader book.id book.name
        let archieve = ZipFile.Open(archieveLocation, ZipArchiveMode.Read)

        let fileNameOption =
            archieve.Entries
            |> List.ofSeq
            |> List.tryExactlyOne
            |> Option.map (fun entry -> entry.Name)

        archieve.Dispose()
        ZipFile.ExtractToDirectory(archieveLocation, seriesLocation)
        File.Delete(archieveLocation)

        match fileNameOption with
        | Some fileName ->
            let archievedFileLocation = Path.Combine(seriesLocation, fileName)
            let extension = Path.GetExtension(fileName)
            let bookFullName = Path.ChangeExtension(book.name, extension)
            let bookLocation = Path.Combine(seriesLocation, bookFullName)
            File.Move(archievedFileLocation, bookLocation)
        | None -> printfn "Was not able to find book in the archieve"

        printfn "Downloaded '%s'" book.name
    }

[<EntryPoint>]
let main _ =
    let builder = CoconaApp.CreateBuilder()

    builder.Services.AddHttpClient() |> ignore

    let app = builder.Build()

    let authorCommand =
        app.AddCommand(
            "author",
            Action<int, string, IHttpClientFactory> (fun authorId outputLocation (clientFactory: IHttpClientFactory) ->
                let authorDocument = AuthorPage(authorId) |> loadHtmlDocument

                let mainDescendants =
                    (authorDocument.DocumentNode.Descendants("div")
                     |> List.ofSeq)

                let mainNodeOption =
                    (mainDescendants
                     |> List.filter isMainNode
                     |> List.tryExactlyOne)

                let authorNodeOption =
                    (mainDescendants
                     |> List.filter (fun node -> node.HasClass "news_title")
                     |> List.tryExactlyOne)

                let authorName =
                    match authorNodeOption with
                    | Some node -> node.FirstChild.InnerHtml
                    | None -> authorId.ToString()

                printfn "Located author name as '%s'" authorName

                match mainNodeOption with
                | Some mainNode ->
                    let descendants = mainNode.Descendants() |> List.ofSeq
                    let items = descendants |> List.choose parseAuthorItemNode
                    let serieses = processItemsList items

                    printfn
                        "Found '%i' serieses with '%i' total books"
                        serieses.Length
                        (List.sumBy (fun series -> series.books.Length) serieses)

                    Console.WriteLine "Please, insert location for file download (default is C:\\)"

                    let location =
                        if outputLocation = "" then
                            $"C:\\{authorName}"
                        else
                            outputLocation

                    Directory.CreateDirectory location |> ignore
                    Console.WriteLine($"Saving files to {location}")
                    use client = clientFactory.CreateClient()
                    let downloaderBase = downloadBookArchieve client

                    List.iter
                        (fun series ->
                            List.map
                                (fun book -> (downloadBook (Path.Combine(location, series.name)) downloaderBase) book)
                                series.books
                            |> Async.Parallel
                            |> Async.RunSynchronously
                            |> ignore)
                        serieses
                | None -> printfn "Failed to find any books")
        )

    app.Run()
    0
