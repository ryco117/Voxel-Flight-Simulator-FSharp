# Voxel Flight Simulator

A simple game developed for the purposes of both showcasing interesting features of sparse-voxel-octree raycasting and to utilize the Vulkan graphics API in a project.
Written in F#, I decided to use the [mono/VulkanSharp](https://github.com/mono/VulkanSharp) library to access Vulkan bindings in .NET.
The [SharpDX](https://github.com/sharpdx/SharpDX) NuGet package is taken as a dependency for Xbox controller input, but no other DirectX functionality is used.

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
|| `F5` | `Start` | Randomize world and reset player |