module ImageProcessing

open System.IO
open System.Reflection
open SixLabors.ImageSharp
open SixLabors.ImageSharp.Drawing.Processing
open SixLabors.ImageSharp.PixelFormats
open SixLabors.ImageSharp.Processing


let logo =
    let assembly = Assembly.GetExecutingAssembly()
    use stream = assembly.GetManifestResourceStream("ATMScreenFormatter.LOGO.jpg")
    use reader = new MemoryStream()
    stream.CopyTo(reader)
    reader.ToArray()

let processImage targetWidth targetHeight (sourceFilename: string) (targetFilename: string) =
    use sourceImage = Image.Load(sourceFilename)
    use logo = Image.Load(logo)
    use targetImage = new Image<Rgba32>(targetWidth, targetHeight) :> Image

    sourceImage.Mutate(fun x ->
        x.Resize(targetWidth, sourceImage.Size.Height) |> ignore
    )

    logo.Mutate(fun x ->
        x.Resize(targetWidth, targetHeight - sourceImage.Size.Height) |> ignore
    )

    targetImage.Mutate(fun x ->
        x.Fill(Color.Black)
            .DrawImage(sourceImage, Point.Empty, 1.0f)
            .DrawImage(logo, Point(0, sourceImage.Size.Height), 1.0f)
        |> ignore
    )

    targetImage.Save(targetFilename)
