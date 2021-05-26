module Helpers

open Vulkan
open System.Runtime.InteropServices
open System.Numerics

let deviceSizeZero = DeviceSize.op_Implicit 0

let rectFromFourNumbers x y width height =
    Rect2D(Offset = Offset2D(X = x, Y = y), Extent = Extent2D(Width = width, Height = height))

[<StructLayout(LayoutKind.Explicit)>]
type Matrix3 =
    struct
        [<FieldOffset(0)>]
        val i_00: float32
        [<FieldOffset(4)>]
        val i_01: float32
        [<FieldOffset(8)>]
        val i_02: float32
        [<FieldOffset(16)>]
        val i_10: float32
        [<FieldOffset(20)>]
        val i_11: float32
        [<FieldOffset(24)>]
        val i_12: float32
        [<FieldOffset(32)>]
        val i_20: float32
        [<FieldOffset(36)>]
        val i_21: float32
        [<FieldOffset(40)>]
        val i_22: float32
        new (mat: Matrix4x4) = {
            i_00 = mat.M11
            i_01 = mat.M12
            i_02 = mat.M13
            i_10 = mat.M21
            i_11 = mat.M22
            i_12 = mat.M23
            i_20 = mat.M31
            i_21 = mat.M32
            i_22 = mat.M33
        }
        new (i_00', i_01', i_02', i_10', i_11', i_12', i_20', i_21', i_22') = {
            i_00 = i_00'
            i_01 = i_01'
            i_02 = i_02'
            i_10 = i_10'
            i_11 = i_11'
            i_12 = i_12'
            i_20 = i_20'
            i_21 = i_21'
            i_22 = i_22'
        }
    end

let mat3FromAxisAndAngle (u: Vector3) (theta: float32) =
    let c = cos theta
    let cC = 1.f - c
    let s = sin theta
    Matrix3 (
        c+u.X*u.X*cC, u.Y*u.X*cC+u.Z*s, u.Z*u.X*cC-u.Y*s,
        u.X*u.Y*cC-u.Z*s, c+u.Y*u.Y*cC, u.Z*u.Y*cC+u.X*s,
        u.X*u.Z*cC+u.Y*s, u.Y*u.Z*cC-u.X*s, c+u.Z*u.Z*cC)

let mat3Identity =
    Matrix3 (
        1.f, 0.f, 0.f,
        0.f, 1.f, 0.f,
        0.f, 0.f, 1.f)