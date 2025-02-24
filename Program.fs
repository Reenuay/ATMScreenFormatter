module Program

open System
open System.IO
open System.Threading.Tasks
open Spectre.Console
open SpectreCoff


type Model =
    | WaitingForSourceFolder of string
    | WaitingForTargetFolder of string * string
    | Processing of string * string
    | FinishedProcessing of string * string
    | Exit

module Model =
    let init = WaitingForSourceFolder ""

type Input =
    | Character of Char
    | Backspace
    | Enter

type Msg =
    | NoOp
    | InputReceived of Input
    | ClearInputReceived
    | FolderPathValidated of Result<string, string>
    | ImageProcessingCompleted

type Command =
    | DoNothing
    | Wait of milliseconds: int
    | ReadInput
    | ValidateFolderPath of path: string

module Command =
    let toTask cmd =
        task {
            match cmd with
            | DoNothing ->
                return NoOp

            | Wait millis ->
                do! Task.Delay(millis)
                return ClearInputReceived

            | ReadInput ->
                let keyInfo = Console.ReadKey(true)

                return
                    match keyInfo.Key with
                    | _ when not (Char.IsControl keyInfo.KeyChar) -> InputReceived (Character keyInfo.KeyChar)
                    | ConsoleKey.Backspace -> InputReceived Backspace
                    | ConsoleKey.Enter -> InputReceived Enter
                    | _ -> NoOp

            | ValidateFolderPath path ->
                return
                    if Directory.Exists path then
                        FolderPathValidated (Ok (DirectoryInfo(path).FullName))
                    else
                        FolderPathValidated (Error "Invalid folder path")
        }

let init = (Model.init, DoNothing)

let update model msg =
    match model, msg with
    | WaitingForSourceFolder _, NoOp -> model, ReadInput
    | WaitingForSourceFolder input, InputReceived (Character c) -> WaitingForSourceFolder (input + c.ToString()), ReadInput
    | WaitingForSourceFolder input, InputReceived Backspace -> WaitingForSourceFolder input[0 .. max -1 (input.Length - 2)], ReadInput
    | WaitingForSourceFolder input, InputReceived Enter -> WaitingForSourceFolder input, ValidateFolderPath input
    | WaitingForSourceFolder _, ClearInputReceived -> WaitingForSourceFolder "", ReadInput
    | WaitingForSourceFolder _, FolderPathValidated (Error error) -> WaitingForSourceFolder error, Wait 2000
    | WaitingForSourceFolder _, FolderPathValidated (Ok path) -> WaitingForTargetFolder (path, String.Empty), DoNothing

    | WaitingForTargetFolder _, NoOp -> model, ReadInput
    | WaitingForTargetFolder (source, input), InputReceived (Character c) -> WaitingForTargetFolder (source, input + c.ToString()), ReadInput
    | WaitingForTargetFolder (source, input), InputReceived Backspace -> WaitingForTargetFolder (source, input[0 .. max -1 (input.Length - 2)]), ReadInput
    | WaitingForTargetFolder (source, input), InputReceived Enter -> WaitingForTargetFolder (source, input), ValidateFolderPath input
    | WaitingForTargetFolder (source, _), ClearInputReceived -> WaitingForTargetFolder (source, ""), ReadInput
    | WaitingForTargetFolder (source, _), FolderPathValidated (Error error) -> WaitingForTargetFolder (source, error), Wait 2000
    | WaitingForTargetFolder (source, _), FolderPathValidated (Ok path) -> Processing (source, path), DoNothing

    

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

        | Processing (source, target) ->
            Calm $"Processing images from: {source}"
            NextLine
            Calm $"Saving to: {target}"
            BlankLine
            alignedRule Alignment.Left "Status"
            Pumped "Processing..."

        | FinishedProcessing (source, target) ->
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
        let mutable state = init

        while fst state <> Exit do
            let model, command = state

            ctx.UpdateTarget(view model)

            let! msg = Command.toTask command

            state <- update model msg

            ()
    }

[<EntryPoint>]
let main _ =
    selectTheme Theming.CyberPunk

    (LiveDisplay.start (view Model.init) run).Wait()

    0
