module Maths

[<System.Runtime.InteropServices.StructLayout(System.Runtime.InteropServices.LayoutKind.Explicit)>]
type Vector4 =
    struct
        [<System.Runtime.InteropServices.FieldOffset 0>]
        val mutable x: float32
        [<System.Runtime.InteropServices.FieldOffset 4>]
        val mutable y: float32
        [<System.Runtime.InteropServices.FieldOffset 8>]
        val mutable z: float32
        [<System.Runtime.InteropServices.FieldOffset 12>]
        val mutable w: float32
        new (x', y', z', w') = {x = x'; y = y'; z = z'; w = w'}
        new (t) = {x = t; y = t; z = t; w = t}
        static member (+) (u: Vector4, v: Vector4) = Vector4 (u.x + v.x, u.y + v.y, u.z + v.z, u.w + v.w)
        static member (*) (t: float32, v: Vector4) = Vector4 (t * v.x, t * v.y, t * v.z, t * v.w)
        static member (*) (v: Vector4, t: float32) = Vector4 (t * v.x, t * v.y, t * v.z, t * v.w)
        static member (/) (v: Vector4, t: float32) = Vector4 (v.x / t, v.y / t, v.z / t, v.w / t)
        static member Zero = Vector4 (0.f)
        member self.AddInPlace (vec: Vector4) =
            self.x <- self.x + vec.x
            self.y <- self.y + vec.y
            self.z <- self.z + vec.z
            self.w <- self.w + vec.w
    end