# NBT-Essentials
Draw the landscapes of your dream in Blender, paste them in Minecraft!

## Features
- Obj files import
- Convert object files to schematics
- Material name parsing to get various blocks
- Runs very fast
- Console only for the moment :(
- Run in any platform (requires Java)

## For developpers
- A simple and strong API to edit, load and save NBT files
- Schematics creating and editing through code

## How to use
### Creating a good obj file
You need to use the Blender internal engine (Cycles does not save the materials in ojb files) The scale in Minecraft will be the same as in
Blender: 1 block for 1 unit, so make sure your build is large enough to preserve the details.
Customize the blocks by naming the materials of your 3D model as in Minecraft, in upper snake case. Examples:
- GRASS
- COAL_ORE
- LEAVES
- etc

** Unrecognized matrials names be set as "AIR" and their associated faces will not be converted. **
Export as WaveFont object file. Don't forget to change the axis in the export window: Y forward and Z up. You also have to check the 
"Triangulate faces" box. Then name your file and hit "Export Obj".

### Convert the model
Open a terminal, and "cd" to the directory that contains the program. Type:
`java -jar ObjToSchematic[extension].jar <obj file path> [schematic file path]`
In case you did not give the output, a file name as the obj will be created in its directory.
After a few seconds, the schematic should be created

### See the model
You can open the schematic in Minecraft with mods like WorldEdit or Schematica.
