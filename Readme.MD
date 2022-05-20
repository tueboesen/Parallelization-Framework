
# OpenMP parallelization framework
## Introduction
This is a parallelization framework I developed at the HydroGeophysics Group during my Ph.D. studies.

The framework is developed in Fortran/OpenMP, and is designed with a focus on portability and a wide userbase. 

The framework removes the need for setting global OpenMP directives throughout the code and instead gathers most of the parallelization calls into a single module. This promotes safer coding practices and should provide at great foundation for any scientific modelling code. (at least until OpenMP gets their act together and make OpenMP have these features internally)

## What is in this pack

| Filename    | Description |
| ----------- | ----------- |
| Example.f90 | Is the file to run, which contains a small example showing how to use the parallelization framework.       |
| Math_routines.f90  | Contains a simple math routine and shows how to use the parallelization framework to set it up in a dynamic way. |
| Misc.f90  | Contains a few helper routines needed by the other modules. |
| Parallelization.f90  | Contains the parallelization framework. |
| Technical_Report.pdf  | A technical report which introduces the parallelization framework as well as an iterative solver I created during my Ph.D. studies.|

## How to set it up

Open either the parallelization solution file or project file, or build your own project by using example.f90 as the main program and the rest as dependencies.
