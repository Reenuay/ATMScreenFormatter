module ImageProcessing

open SixLabors.ImageSharp
open SixLabors.ImageSharp.Drawing
open SixLabors.ImageSharp.Drawing.Processing
open SixLabors.ImageSharp.PixelFormats
open SixLabors.ImageSharp.Processing


let processImage targetWidth targetHeight (sourceFileName: string) (targetFileName: string) =
    use targetImage = new Image<Rgba32>(targetWidth, targetHeight) :> Image
    use sourceImage = Image.Load(sourceFileName)

    let newHeight = targetWidth / sourceImage.Size.Width * sourceImage.Size.Height

    sourceImage.Mutate(fun x ->
        x.Resize(targetWidth, newHeight) |> ignore
    )

    targetImage.Mutate(fun x ->
        x.Fill(Color.Black).DrawImage(sourceImage, Point.Empty, 1.0f) |> ignore
    )

    targetImage.Save(targetFileName)
