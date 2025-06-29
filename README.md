# Simulating a potential mpox outbreak in India: Implications for outbreak control  in non-endemic settings

The provided code is a simulation written using the agent-based simulation framework _BharatSim_. This simulation is designed to model a potential mpox outbreak in the Indian context. It uses a population containing a network of homes and workplaces, with an embedded sexual-contact network of men who have sex with men (MSMs). This code was used to produce all the results in the paper "Simulating a potential mpox outbreak in India: Implications for outbreak control  in non-endemic settings".

Questions or comments can be directed to dpcherian@gmail.com, or on this project's GitHub page.

## Requirements

The code is written using the _BharatSim_ framework, coded in _Scala 2_. The _BharatSim_ framework is provided as a library with this code, in the `lib` folder. More information can be found in the documentation of [bharatsim.ashoka.edu.in](https://bharatsim.ashoka.edu.in), or on the [GitHub page for the BharatSim project](https://github.com/bharatsim).

However, this framework requires both Java and Scala 2 to run. We recommend using either Java 8 or Java 11, either of which can be obtained from [Oracle](https://www.oracle.com/) or [OpenJDK](https://openjdk.org/). Scala can be installed using _coursier_, the Scala application manager. The installation instructions for coursier can be found on the [coursier site](https://get-coursier.io/docs/cli-installation).

## License

[![CC BY-SA 4.0][cc-by-sa-shield]][cc-by-sa]

This work is licensed under a
[Creative Commons Attribution-ShareAlike 4.0 International License][cc-by-sa].

[![CC BY-SA 4.0][cc-by-sa-image]][cc-by-sa]

[cc-by-sa]: http://creativecommons.org/licenses/by-sa/4.0/
[cc-by-sa-image]: https://licensebuttons.net/l/by-sa/4.0/88x31.png
[cc-by-sa-shield]: https://img.shields.io/badge/License-CC%20BY--SA%204.0-lightgrey.svg
