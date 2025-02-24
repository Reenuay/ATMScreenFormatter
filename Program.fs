module Program

open System
open System.IO
open System.Threading.Tasks
open Spectre.Console
open SpectreCoff


type ProcessingResult =
    | NoFilesDetected of sourceFolder: string
    | FinishedSuccessfully of sourceFolder: string * targetFolder: string * processedCount: int

type Model =
    | WaitingForSourceFolder of input: string
    | WaitingForTargetFolder of sourceFolder: string * input: string
    | Processing of sourceFolder: string * targetFolder: string * images: string list * totalCount: int
    | FinishedProcessing of ProcessingResult
    | Exit

module Model =
    let init = WaitingForSourceFolder ""

type Input =
    | Character of Char
    | Backspace
    | Enter
    | Other

type Msg =
    | Start
    | InputReceived of Input
    | ClearInputReceived
    | FolderPathValidated of Result<string, string>
    | TargetFoldersCreated
    | SourceFilenamesReceived of filenames: string list
    | ImageProcessed

type Command =
    | DoNothing
    | WaitAndClearTheInput of milliseconds: int
    | ReadInput
    | ValidateFolderPath of path: string
    | CreateFoldersForEachTargetSize of targetFolder: string
    | GetAllSourceFileNames of sourceFolder: string
    | ProcessImage of sourceFolder: string * targetFolder: string * filename: string

module Command =
    let targetSizes =
        [
            (1024, 1280)
            (1280, 1024)
        ]

    let toTask cmd =
        task {
            match cmd with
            | DoNothing ->
                return Start

            | WaitAndClearTheInput millis ->
                do! Task.Delay(millis)
                return ClearInputReceived

            | ReadInput ->
                let keyInfo = Console.ReadKey(true)

                let input =
                    match keyInfo.Key with
                    | _ when not (Char.IsControl keyInfo.KeyChar) -> Character keyInfo.KeyChar
                    | ConsoleKey.Backspace -> Backspace
                    | ConsoleKey.Enter -> Enter
                    | _ -> Other

                return InputReceived input

            | ValidateFolderPath path ->
                return
                    if Directory.Exists path then
                        FolderPathValidated (Ok (DirectoryInfo(path).FullName))
                    else
                        FolderPathValidated (Error "Invalid folder path")

            | CreateFoldersForEachTargetSize targetFolder ->
                targetSizes
                |> List.iter (fun (targetWidth, targetHeight) ->
                    let targetPerSizeFolder = Path.Combine(targetFolder, $"{targetWidth}x{targetHeight}")
                    if not <| Directory.Exists targetPerSizeFolder then
                        Directory.CreateDirectory(targetPerSizeFolder) |> ignore
                )

                return TargetFoldersCreated

            | GetAllSourceFileNames folderPath ->
                return Directory.GetFiles(folderPath, "*.jpg") |> Array.toList |> List.map Path.GetFileName |> SourceFilenamesReceived

            | ProcessImage (sourceFolder, targetFolder, filename) ->
                targetSizes
                |> List.iter (fun (targetWidth, targetHeight) ->
                    let sourceFilename = Path.Combine(sourceFolder, filename)
                    let targetFilename = Path.Combine(targetFolder, $"{targetWidth}x{targetHeight}", filename)

                    ImageProcessing.processImage targetWidth targetHeight sourceFilename targetFilename
                )

                return ImageProcessed
        }

let init = (Model.init, DoNothing)

let update model msg =
    match model, msg with
    | WaitingForSourceFolder _, Start -> model, ReadInput
    | WaitingForSourceFolder input, InputReceived (Character c) -> WaitingForSourceFolder (input + c.ToString()), ReadInput
    | WaitingForSourceFolder input, InputReceived Backspace -> WaitingForSourceFolder input[0 .. max -1 (input.Length - 2)], ReadInput
    | WaitingForSourceFolder input, InputReceived Enter -> WaitingForSourceFolder input, ValidateFolderPath input
    | WaitingForSourceFolder _, ClearInputReceived -> WaitingForSourceFolder "", ReadInput
    | WaitingForSourceFolder _, FolderPathValidated (Error error) -> WaitingForSourceFolder error, WaitAndClearTheInput 2000
    | WaitingForSourceFolder _, FolderPathValidated (Ok path) -> WaitingForTargetFolder (path, String.Empty), DoNothing

    | WaitingForTargetFolder _, Start -> model, ReadInput
    | WaitingForTargetFolder (source, input), InputReceived (Character c) -> WaitingForTargetFolder (source, input + c.ToString()), ReadInput
    | WaitingForTargetFolder (source, input), InputReceived Backspace -> WaitingForTargetFolder (source, input[0 .. max -1 (input.Length - 2)]), ReadInput
    | WaitingForTargetFolder (source, input), InputReceived Enter -> WaitingForTargetFolder (source, input), ValidateFolderPath input
    | WaitingForTargetFolder (source, _), ClearInputReceived -> WaitingForTargetFolder (source, ""), ReadInput
    | WaitingForTargetFolder (source, _), FolderPathValidated (Error error) -> WaitingForTargetFolder (source, error), WaitAndClearTheInput 2000
    | WaitingForTargetFolder (source, _), FolderPathValidated (Ok path) -> Processing (source, path, [], 0), DoNothing

    | Processing (_, target, [], 0), Start -> model, CreateFoldersForEachTargetSize target
    | Processing (source, _, [], 0), TargetFoldersCreated -> model, GetAllSourceFileNames source
    | Processing (source, _, [], 0), SourceFilenamesReceived [] -> FinishedProcessing (NoFilesDetected source), DoNothing
    | Processing (source, target, [], 0), SourceFilenamesReceived (current::next) ->
        Processing (source, target, current::next, List.length next + 1), ProcessImage (source, target, current)
    | Processing (source, target, _::current::next, totalCount), ImageProcessed ->
        Processing (source, target, current::next, totalCount), ProcessImage (source, target, current)
    | Processing (source, target, _::[], totalCount), ImageProcessed ->
        FinishedProcessing (FinishedSuccessfully (source, target, totalCount)), DoNothing

    | FinishedProcessing _, Start -> model, ReadInput
    | FinishedProcessing _, InputReceived _ -> Exit, DoNothing

    | _ -> model, DoNothing

let view model =
    Many [
        Figlet.customFiglet Alignment.Left Spectre.Console.Color.Purple "ATM Screen Formatter"
        BlankLine

        match model with
        | WaitingForSourceFolder input ->
            Calm "Specify the source folder for processing."
            NextLine
            Pumped "You can copy it or drag the folder directly here."
            BlankLine
            alignedRule Alignment.Left "Source Folder"
            Calm "Current input:"
            Vanilla input

        | WaitingForTargetFolder (source, input) ->
            Calm $"Source folder: {source}"
            NextLine
            Calm "Specify the target folder for saving processed images."
            NextLine
            Pumped "You can copy it or drag the folder directly here."
            BlankLine
            alignedRule Alignment.Left "Target Folder"
            Calm "Current input:"
            Vanilla input

        | Processing (_, _, [], _) -> ()

        | Processing (source, target, current::next, totalCount) ->
            Calm $"Processing images from: {source}"
            NextLine
            Calm $"Saving to: {target}"
            BlankLine
            alignedRule Alignment.Left "Status"
            Calm $"Processing {current}"
            NextLine
            Edgy $"Progress: {totalCount - List.length next - 1}/{totalCount}"

        | FinishedProcessing (NoFilesDetected source) ->
            Calm $"No .jpg files detected in {source}"
            BlankLine
            Calm "Press any key to exit"

        | FinishedProcessing (FinishedSuccessfully (source, target, processedCount)) ->
            Calm $"Images processed from: {source}"
            NextLine
            Calm $"Saved to: {target}"
            NextLine
            Calm $"Total files processed: {processedCount}"
            BlankLine
            alignedRule Alignment.Left "Success"
            Pumped "Done!"
            BlankLine
            Calm "Press any key to exit"

        | Exit -> ()
    ]
    |> Output.payloadToRenderable


let run (ctx: LiveDisplayContext) =
    task {
        let mutable prevModel = None
        let mutable state = init

        while fst state <> Exit do
            let model, command = state

            if prevModel <> Some model then
                ctx.UpdateTarget(view model)

            let! msg = Command.toTask command

            prevModel <- Some model
            state <- update model msg

            ()
    }

[<EntryPoint>]
let main _ =
    selectTheme Theming.CyberPunk

    (LiveDisplay.start (view Model.init) run).Wait()

    0
