module LightState

[<System.Runtime.InteropServices.StructLayout(System.Runtime.InteropServices.LayoutKind.Explicit)>]
type PushConstantData =
    struct
        [<System.Runtime.InteropServices.FieldOffset 0>]
        val cameraPosition: System.Numerics.Vector3
        [<System.Runtime.InteropServices.FieldOffset 12>]
        val time: float32
        [<System.Runtime.InteropServices.FieldOffset 16>]
        val lightDir: System.Numerics.Vector3
        new (cameraPosition', time') = {
            cameraPosition = cameraPosition'
            time = time'
            lightDir =
                let delta = time' / -2.f
                System.Numerics.Vector3(0.8f * sin delta, 0.6f, 0.8f * cos delta)}
    end
let pushConstantSize = sizeof<PushConstantData>

type LightState = {
    gameObjects: LightObject.LightObject[]
    upTime: System.Diagnostics.Stopwatch}