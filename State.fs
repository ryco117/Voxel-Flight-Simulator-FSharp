module LightState

type LightState = {
    demoControls: bool
    gameObjects: LightObject.LightObject[]
    keyBackInput: float32
    keyForwardInput: float32
    keyLeftInput: float32
    keyRightInput: float32
    lastFrameTime: float
    lastSpeed: float32
    playerPosition: System.Numerics.Vector3
    playerQuaternion: Maths.Vector4
    upTime: System.Diagnostics.Stopwatch}

let demoCameraPosition time =
    let delta = time / 16.f;
    System.Numerics.Vector3 (-2.25f * sin delta, 0.125f, -2.25f * cos delta)

let demoCameraQuaternion time =
    let delta = time / 32.f
    Maths.Vector4 (0.f, sin delta, 0.f, cos delta)

let lightDir time =
    let delta = time / -15.f
    System.Numerics.Vector3 (0.8f * sin delta, 0.6f, 0.8f * cos delta)