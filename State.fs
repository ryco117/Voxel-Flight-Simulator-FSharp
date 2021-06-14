module LightState

type LightState = {
    demoControls: bool
    gameObjects: LightObject.LightObject[]
    keyBackInput: float32
    keyForwardInput: float32
    keyLeftInput: float32
    keyRightInput: float32
    lastFrameTime: float
    playerPosition: System.Numerics.Vector3
    playerQuaternion: Maths.Vector4
    upTime: System.Diagnostics.Stopwatch}