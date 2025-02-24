module Program

open System
open System.IO
open System.Threading.Tasks
open Spectre.Console
open SpectreCoff

type ProcessingResult =
    | NoFilesDetected of string
    | FinishedSuccessfully of string * string

type Model =
    | WaitingForSourceFolder of string
    | WaitingForTargetFolder of string * string
    | Processing of string * string * string list * string list
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
    | SourceFileNamesReceived of string list

type Command =
    | DoNothing
    | WaitAndClearTheInput of milliseconds: int
    | ReadInput
    | ValidateFolderPath of path: string
    | GetAllSourceFileNames of folderPath: string
    | ProcessImage of path: string

module Command =
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

            | GetAllSourceFileNames folderPath ->
                return Directory.GetFiles(folderPath, "*.jpg") |> Array.toList |> SourceFileNamesReceived
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
    | WaitingForTargetFolder (source, _), FolderPathValidated (Ok path) -> Processing (source, path, [], []), DoNothing

    | Processing (source, _, [], []), Start -> model, GetAllSourceFileNames source
    | Processing (source, _, [], []), SourceFileNamesReceived [] -> FinishedProcessing (NoFilesDetected source), DoNothing
    | Processing (source, target, [], []), SourceFileNamesReceived (firstToProcess::others)
        -> Processing (source, target, (firstToProcess::others), []), ProcessImage firstToProcess

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

        | Processing (source, target, inProgressImage::unprocessedImages, _) ->
            Calm $"Processing images from: {source}"
            NextLine
            Calm $"Saving to: {target}"
            BlankLine
            alignedRule Alignment.Left "Status"

            Table.table
                [ Column.column (Pumped "Filename") ]
                (Payloads [ Vanilla inProgressImage ] :: List.map (Vanilla >> List.singleton >> Payloads) unprocessedImages)
            |> Table.withCaption "Processing"
            |> toOutputPayload

        | FinishedProcessing (NoFilesDetected source) ->
            Calm $"No .jpg files detected in {source}"
            BlankLine
            Calm "Press any key to exit"

        | FinishedProcessing (FinishedSuccessfully (source, target)) ->
            Calm $"Images processed from: {source}"
            NextLine
            Calm $"Saved to: {target}"
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
