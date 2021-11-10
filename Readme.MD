------------------------------------
------------Introduction------------
------------------------------------
This is a parallelization framework developed at the HydroGeophysics Group, Aarhus University, Denmark.
The framework is developed in Fortran/OpenMP, and is designed with focus on portability and a wide userbase. 
The framework removes the need for setting global OpenMP directives throughout the !code and instead gathers most of the parallelization calls into a single module. This promotes safer coding practices and should provide at great foundation for any scientific modelling code.
------------------------------------
-------What is in this pack---------
------------------------------------
Example.f90          !Is the main program, which contains a small example showing how to use the parallelization framework
Math_routines.f90    !Contains a simple math routine and shows how to use the parallelization framework to set it up in a dynamic way. 
Misc.f90             !Contains a few helper routines needed by the other modules.
Parallelization.f90  !Contains the parallelization framework
Technical_Report.pdf !A technical report which introduces parallelization and explains why and how the parallelization framework was created. Furthermore it contains information about a sparse iterative solver also created by the Hydrogeophysics Group at Aarhus University.
------------------------------------
---------How to set it up-----------
------------------------------------
Open either the parallelization solution file or project file, or built your own project by using example.f90 as the main program and the rest as dependencies.

------------------------------------
-------------License----------------
------------------------------------
This code is provided freely under the MIT License.

Copyright (c) [2017] [HydroGeophysics Group, Aarhus University, Denmark]

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.