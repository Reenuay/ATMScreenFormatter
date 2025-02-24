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

    let logoWidth = logo.Size.Width
    let logoHeight = logo.Size.Height

    let gapWidth = int (float targetWidth * 0.8)
    let gapHeight = targetHeight - sourceImage.Size.Height

    let scale = min (float gapWidth / float logoWidth) (float gapHeight / float logoHeight)

    let newLogoWidth = int (float logoWidth * scale)
    let newLogoHeight = int (float logoHeight * scale)

    sourceImage.Mutate(fun x ->
        x.Resize(targetWidth, sourceImage.Size.Height) |> ignore
    )

    logo.Mutate(fun x ->
        x.Resize(newLogoWidth, newLogoHeight) |> ignore
    )

    targetImage.Mutate(fun x ->
        x.Fill(Color.White)
            .DrawImage(sourceImage, Point.Empty, 1.0f)
            .DrawImage(logo, Point((targetWidth - newLogoWidth) / 2, sourceImage.Size.Height + ((gapHeight - newLogoHeight) / 2)), 1.0f)
        |> ignore
    )

    targetImage.Save(targetFilename)

let getFileDimensions (filename: string) =
    let size = Image.Identify(filename).Size
    size.Width, size.Height
