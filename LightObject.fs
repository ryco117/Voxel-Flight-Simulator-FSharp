module LightObject

open LightModel

let mutable currentId = 0UL

type LightObject () =
    let id = System.Threading.Interlocked.Increment &currentId - 1UL
    let mutable transform = System.Numerics.Matrix4x4.Identity
    let mutable model: LightModel option = None
    member _.Id = id
    member _.Transform
        with get () = transform
        and set transform' = transform <- transform'
    member _.Model
        with get () = model
        and set model' = model <- model'