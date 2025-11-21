# Setting up ROMS-Darwin in a 6p4z configuration

This guide provides step-by-step instructions for setting up a ROMS-Darwin simulation using the 6 phytoplankton, 4 zooplankton (6p4z) configuration. The 6p4z configuration is a relatively simple Darwin setup that serves as a good starting point for using ROMS-Darwin. It can later be easily expanded to include more plankton groups or size classes.

## Prerequisites

A working physical ROMS setup and the ability to successfully compile and run the physical ROMS setup using the ROMS code from the ROMS-Darwin repository (including the `varinfo.yaml` file ([link](../ROMS/External/varinfo.yaml)) with variable information required by Darwin), without yet activating any Darwin-related options.

## Getting Started

### Step 1: Compile ROMS Executable with ROMS-Darwin

To compile the ROMS-Darwin executable for the 6p4z configuration:

1. Add the following options to your `build_roms.sh` compilation script:
```
MY_CPP_FLAGS="${MY_CPP_FLAGS} -DDARWIN -DDARWIN_6P4Z -DDARWIN_NOREAD_GRAZEMAX -DDARWIN_NOREAD_MORT2"
```
2. Add the lines from the `darwin_additions.h` file ([link](darwin_additions.h)) to your ROMS application header file (the h-file specified using the `ROMS_APPLICATION` variable in the `build_roms.sh` script).
    * These additional lines activate Darwin-related options and provide values for 6p4z-specific parameters.
    * The easiest approach is to copy-paste the entire contents of `darwin_additions.h` to the end of your application CPP definitions header file.

### Step 2: Configure the `darwin_6p4z.in` File

The ROMS-Darwin input file is located at `ROMS/External/darwin_6p4z.in` and contains parameters for the 6p4z configuration.

1. Copy this file and use it as the input biological parameters file (`BPARNAM`) in your ROMS main input parameters file.
2. Modify domain-specific settings (typically the only changes required):
    * Lateral boundary condition type: `LBC(isTvar)` (For phytoplankton, zooplankton, and chlorophyll variables, gradient boundary conditions (keyword `Gra` in `darwin_6p4z.in`) are typically the easiest option.)
    * Horizontal advection scheme: `Hadvection`
    * Vertical advection scheme: `Vadvection`

### Step 3: Add Darwin Biological Tracer Variables

Add initial conditions and boundary values for Darwin biological tracer variables to your initial and boundary files.
* The required variables are listed in the lateral boundary condition type `(LBC(isTvar))` variable in the darwin_6p4z.in file (boundary conditions do not need to be specified for variables with gradient boundary conditions).
* Note: This step typically requires the most effort and time, yet nearly all biological tracer variables (except plankton and chlorophyll) will be reusable in other ROMS-Darwin configurations.

### Step 4: Add Darwin-Specific Variables to Initial File

ROMS-Darwin requires additional configuration-specific variables in the initial file, such as the grazing palatability matrix (`palat`).

Use the `darwin_varutil.py` Python script ([link](darwin_varutil.py)) to create the default values for the 6p4z configuration:
```
python darwin_varutil.py add-var ROMS_DARWIN_INITIAL_FILE all --defaultvalues --darwin 6p4z
```
**Important:** This script will edit the input file and add missing dimensions (`nplank`) and variables.

### Step 5: Run ROMS-Darwin

You should now be ready to start the ROMS executable and perform a Darwin run.

## Support

If you experience problems with these steps, please open a GitHub issue.
