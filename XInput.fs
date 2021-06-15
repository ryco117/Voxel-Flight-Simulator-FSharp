module XInput

open SharpDX.XInput

type XInput () =
    let controller = Controller (UserIndex.One)
    let mutable packetIndex = None
    member _.UpdatedState =
        if controller.IsConnected then
            let state = controller.GetState ()
            let newIndex = state.PacketNumber
            match packetIndex with
            | None ->
                packetIndex <- Some state.PacketNumber
                None
            | Some oldIndex when oldIndex = newIndex -> None
            | Some _ ->
                packetIndex <- Some newIndex
                Some state.Gamepad
        else
            None