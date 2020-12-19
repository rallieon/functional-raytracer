# Illuminate

A raytracer built using F#. The renderer will be based upon a variety of works, research papers, and websites but primary references include:

1. [PBRT](https://pbrt.org/)
2. [Raytracing in One Weekend](https://github.com/petershirley/raytracinginoneweekend)
3. [Scratch a Pixel](https://www.scratchapixel.com/index.php)

## Building & Running

Illuminate was developed using VSCode. In order to run the raytracer you will need dotnet core 2.2+ installed. The command line will work for most actions, but there is work to reference in `.vscode/tasks.json` to make development easier.

### Building

`dotnet restore && dotnet build`

### Testing

`dotnet test`

Unit testing is minimal to date, but will improve over time. There are integration tests as well that are more useful to ensure non breaking changes to the raytracing algorithm.

### Run

Utilize the run task available from VSCode or run:

`dotnet run --project illuminate.console -- ./meta/scenes/main.json ./meta/images/output.jpeg`

The command line will take two arguments. The first argument will define the path to a scene file and the second argument will define the output path to a file. Currenty the raytracer only supports JPEG images for output. `./meta/scenes/main.json` is the default option if no input parameters are defined. The output will write a timestamped image to the `images` folder.

## Scene Definition

The following is a sample scene file to be used as reference. Currently it is self explanatory, but will eventually need to expound upon this format.

```javascript
{
  "width": 640,
  "height": 480,
  "fov": 90,
  "shapes": [{
      "Sphere": {
        "origin": {
          "x": 4,
          "y": 0,
          "z": -10
        },
        "radius": 4,
        "color": {
          "r": 0,
          "g": 0,
          "b": 255
        }
      }
    },
    {
      "Sphere": {
        "origin": {
          "x": -4,
          "y": 0,
          "z": -5
        },
        "radius": 1,
        "color": {
          "r": 255,
          "g": 0,
          "b": 0
        }
      }
    }
  ],
  "lights": [{
      "PointLight": {
        "origin": {
          "x": -8,
          "y": 0,
          "z": -2
        },
        "luminosity": {
          "r": 255,
          "g": 255,
          "b": 255
        },
        "intensity": 1
      }
    }
  ],
  "camera": {
    "x": 0,
    "y": 0,
    "z": 0
  }
}
```

## Publishing & Versioning

Illuminate follows a loose interpretation of SemVar. Currently every tag will increment the minor version. Major and Patch are not utilized to date, but will eventually upon a `1.0.0` release.

Illuminate has a manual versioning increment policy, but the physical publishing uses a script `publish.sh` that makes publishing the latest version a simpler process. The intent is to commit the primary scene and image generated by the raytracer at that time and then increment the minor version number. The script will create a release folder for the `0.x.0` tag and then perform the necessary commands to create the tag and push to GitHub.
