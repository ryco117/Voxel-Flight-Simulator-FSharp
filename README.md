# Voxel Flight Simulator

A simple game developed for the purposes of both showcasing interesting features of sparse-voxel-octree raycasting and to utilize the Vulkan graphics API in a project.
Written in F#, I decided to use the [mono/VulkanSharp](https://github.com/mono/VulkanSharp) library to access Vulkan bindings in .NET.
The [SharpDX](https://github.com/sharpdx/SharpDX) NuGet package is taken as a dependency for Xbox controller input, but no other DirectX functionality is used.

### Video Demo
[![Video Demo](https://i.vimeocdn.com/filter/overlay?src0=https%3A%2F%2Fi.vimeocdn.com%2Fvideo%2F1165882050_1280x720&src1=https%3A%2F%2Ff.vimeocdn.com%2Fimages_v6%2Fshare%2Fplay_icon_overlay.png)](https://vimeo.com/563966468 "Video Demo")

### Controls
| Keyboard | Xbox Controlelr | Function |
| -------- | --------------- | -------- |
| `W` | `LEFT-ANALOG FORWARD` | Pitch camera down |
| `S` | `LEFT-ANALOG BACKWARD` | Pitch camera up |
| `A` | `LEFT-ANALOG LEFT` | Roll camera left |
| `D` | `LEFT-ANALOG RIGHT` | Roll camera right |
| `Q` | `LEFT-TRIGGER` | Turn camera yaw left |
| `E` | `RIGHT-TRIGGER` | Turn camera yaw right |
| `ESC` | `BACK` | Close application |
| `F11` | N/A | Toggle borderless fullscreen on primary display |
| `CTRL`+`R` | `B` | Reset player to start of current world |
| `F5` | `Start` | Randomize world and reset player |
| `SHIFT`+`F5` | N/A | Reset world and player to game start |