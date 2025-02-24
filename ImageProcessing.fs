module ImageProcessing

open SixLabors.ImageSharp
open SixLabors.ImageSharp.Drawing
open SixLabors.ImageSharp.Drawing.Processing
open SixLabors.ImageSharp.PixelFormats
open SixLabors.ImageSharp.Processing


let processImage targetWidth targetHeight (sourceFilename: string) (targetFilename: string) =
    use targetImage = new Image<Rgba32>(targetWidth, targetHeight) :> Image
    use sourceImage = Image.Load(sourceFilename)

    sourceImage.Mutate(fun x ->
        x.Resize(targetWidth, sourceImage.Size.Height) |> ignore
    )

    targetImage.Mutate(fun x ->
        x.Fill(Color.Black).DrawImage(sourceImage, Point.Empty, 1.0f) |> ignore
    )

    targetImage.Save(targetFilename)
