# Setting up ROMS-Darwin in a 6p4z configuration

This guide provides step-by-step instructions for setting up a ROMS-Darwin simulation using the 6 phytoplankton, 4 zooplankton (6p4z) configuration. The 6p4z configuration is a relatively simple Darwin setup that serves as a good starting point for using ROMS-Darwin. It can later be easily expanded to include more plankton groups or size classes.

**Notes:**
* 2025-12-03: removed the automatic inclusion of CDOM in the 6p4z configuration (to add it manually, compile with the `DARWIN_CDOM` option)

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


# Adding particulate organic matter (POM) classes using `DARWIN_POMCLASS`

The following instructions provide the steps for expanding a Darwin 6p4z configuration to include multiple POM classes. It can be easily modified to add POM classes to any ROMS-Darwin configuration.

## The `DARWIN_POMCLASS` option

Compiling ROMS-Darwin with the `DARWIN_POMCLASS` option will add additional variables for POC, PON, POP, POSi, and POFe (particulate organic carbon, nitrogen, phosphorous, silicate, and iron).
* The number of POM classes is set at compile time using the `DARWIN_DIM_NPOM` CPP option; it creates variables for all 5 POM constituents (POC, PON, POP, POSi, and POFe).
* The classes are not necessarily defined by particle size -- any distinguishing characteristic (size, composition, lability, ...) can be used, as long as it is consistently reflected in the sinking rates and the `pomfrac_mort`/`pom_deg`/`pom_frag` parameters described below.
* The class variables are called `POC01`, `POC02`, etc.; the `varinfo.yaml` file can be used to change the default naming scheme.
* Each class of each POM constituent can be configured to have a different sinking rate. Typically, each class is given the same sinking rate across the different constituents, so that `POC01`, `PON01`, `POP01`, `POSi01`, and `POFe01` all have identical sinking rates.
* The fractionation of dead plankton into POM classes is configured using the `nplank`x`npom` matrix `pomfrac_mort`. For example, in a configuration with 3 POM classes, in which the first column of `pomfrac_mort` is set to `[0.9, 0.1, 0.0]`, mortality of the first phytoplankton variable, `phytoplankton01`, converts 90% of its dead biomass to POM class 1, 10% to POM class 2 and no biomass to POM class 3.
* Similarly, the fractionation of grazing-derived POM (sloppy feeding/egestion) into POM classes is configured using the `nplank`x`npom` matrix `pomfrac_graz` -- same layout and same sum-to-1-per-row convention as `pomfrac_mort`, but indexed by the *predator* doing the grazing rather than the prey being consumed, since which POM classes an egestion event produces is treated as a property of the predator's own feeding strategy/behavior.

### Degradation and fragmentation between POM classes: `DARWIN_POMCLASS_DEGFRAG`

Compiling with the additional `DARWIN_POMCLASS_DEGFRAG` option (requires `DARWIN_POMCLASS`) allows POM to convert from one class to another over time, via two independent processes:
* **Degradation**, proportional to each class's own local remineralization rate (bacterial plus parameterized) and configured via the `npom`x`npom` matrix `pom_deg`, where `pom_deg(i,j)` is the fraction of class `i`'s remineralization flux that is instead redirected to class `j` (rather than lost to the dissolved pool).
* **Fragmentation**, proportional to each class's own standing concentration and configured via the `npom`x`npom` matrix `pom_frag`, where `pom_frag(i,j)` is the rate (per second) at which class `i`'s concentration converts into class `j`.

Both matrices should have zero (or near-zero) diagonal elements (a class does not degrade/fragment into itself).

## Steps for adding `DARWIN_POMCLASS` to a configuration

### Step 1: Compile a new ROMS Executable with `DARWIN_POMCLASS`

Add the following options to your `build_roms.sh` compilation script:
```
MY_CPP_FLAGS="${MY_CPP_FLAGS} -DDARWIN_POMCLASS -DDARWIN_DIM_NPOM=3"
```
and set the value of `DARWIN_DIM_NPOM` to `npom`, the desired number of POM classes. Add `-DDARWIN_POMCLASS_DEGFRAG` as well if POM should be able to convert between classes over time.

### Step 2: Configure the `darwin_6p4z.in` File

Edit the `darwin_6p4z.in` file.
1. Specify the sinking rates for each POM class. In the default configuration, the variables `wC_sink_pday`, `wP_sink_pday`, `wN_sink_pday`, `wFe_sink_pday`, and `wSi_sink_pday` specify the sinking rates of POC, PON, POP, POSi, and POFe, respectively. When using `DARWIN_POMCLASS`, ROMS expects `npom` values for each variable, one for each class (providing fewer than `npom` values repeats the last value for the remaining classes). The new `darwin_6p4z.in` could have the following entries:
```
! POC sinking rate (m d^-1)

  wC_sink_pday == 5.0d0 10.0d0 15.0d0

! POP sinking rate (m d^-1)

  wP_sink_pday == 5.0d0 10.0d0 15.0d0

! PON sinking rate (m d^-1)

  wN_sink_pday == 5.0d0 10.0d0 15.0d0

! POFe sinking rate (m d^-1)

 wFe_sink_pday == 5.0d0 10.0d0 15.0d0

! POSi sinking rate (m d^-1)

 wSi_sink_pday == 5.0d0 10.0d0 15.0d0

```
specifying a sinking rate of 5.0 m day⁻¹ for all POM constituents of the first class, 10 m day⁻¹ for the second class and 15 m day⁻¹ for the third class.
2. Update boundary, advection scheme specification and other variables, to include the new POM classes. Instead of 5 POM variables, the ROMS-Darwin configuration now contains 5x`npom` POM variables (in our example with `npom`=3, these are: `POC01`, `POC02`, `POC03`, `PON01`, ..., `POFe03`). These variables require changes to `LBC(isTvar)`, `Hadvection`, `Vadvection`, and perhaps other variables in the `darwin_6p4z.in` file that depend on the number of biological tracers, such as `TNU2` and following. Here it is important to know that `POC01` will be the 12th biological tracer (the index normally given to `POC`), `POC02` will be the 13th, followed by all other POM variables up to the last POFe class.

### Step 3: Add POM Variables to the Initial and Boundary Files

The updated ROMS-Darwin configuration requires initial and boundary conditions for the new tracer variables `POC01`, `POC02`, etc. (see **Step 3: Add Darwin Biological Tracer Variables** above). The new variables may be kept alongside initial and boundary conditions for the "regular" Darwin variables `POC`, ..., `POFe` to use the initial or boundary files for ROMS-Darwin applications with and without `DARWIN_POMCLASS`.

### Step 4: Add `pomfrac_mort`, `pomfrac_graz` (and, with `DARWIN_POMCLASS_DEGFRAG`, `pom_deg`/`pom_frag`) Variables to Initial File

ROMS-Darwin with `DARWIN_POMCLASS` requires two additional configuration-specific variables in the initial file, the `pomfrac_mort` and `pomfrac_graz` variables described above; with `DARWIN_POMCLASS_DEGFRAG`, it additionally requires `pom_deg` and `pom_frag`.

Use the `darwin_varutil.py` Python script ([link](darwin_varutil.py)) to create the default values for the 6p4z configuration:
```
python darwin_varutil.py add-var ROMS_DARWIN_INITIAL_FILE pomfrac_mort --defaultvalues
python darwin_varutil.py add-var ROMS_DARWIN_INITIAL_FILE pomfrac_graz --defaultvalues
python darwin_varutil.py add-var ROMS_DARWIN_INITIAL_FILE pom_deg --defaultvalues
python darwin_varutil.py add-var ROMS_DARWIN_INITIAL_FILE pom_frag --defaultvalues
```
If you are using an `npom` value not equal to 3, use (for example, for `pomfrac_mort`)
```
python darwin_varutil.py add-var ROMS_DARWIN_INITIAL_FILE pomfrac_mort --dimensions npom NPOM --values 0.0
```
where `NPOM` needs to be the integer value of `npom`. This latter call will only initialize the variable (filled with zeros); its values must still be set, so that the rows of `pomfrac_mort`/`pomfrac_graz` (one row per plankton/predator type) each sum to 1 (`pom_deg`/`pom_frag` may be left at zero if degradation/fragmentation between classes is not desired).

**Important:** This script will edit the input file and add missing dimensions (`npom`) and the variable.

### Step 5: Run ROMS-Darwin

You should now be ready to start the new ROMS executable and perform a Darwin run with POM classes.

# Using fixed elemental ratios for POM classes: `DARWIN_POMCLASS_FIXEDRATIO`

## The `DARWIN_POMCLASS_FIXEDRATIO` option

`DARWIN_POMCLASS` (above) gives every POM constituent (POC, PON, POP, POSi, POFe) its own independent, fully prognostic tracer for each class -- the elemental content of a class can drift freely, just like a plankton type with all of `DARWIN_NQUOTA`/`PQUOTA`/`SIQUOTA`/`FEQUOTA` turned on. `DARWIN_POMCLASS_FIXEDRATIO` (requires `DARWIN_POMCLASS`, and is combinable with `DARWIN_POMCLASS_DEGFRAG`) is a simpler alternative: only POC gets per-class tracers (`POC01`, `POC02`, ...); PON, POP, POSi, and POFe are **not tracers at all** under this option -- not even a single bulk one. Each POC class instead has a fixed, configurable C:N, C:P, C:Si, and C:Fe ratio, the same way a plankton type with a quota turned *off* has no separate "plankton N"-style tracer at all -- its N content is always `C * R_NC`, computed on demand, never stored as model state.

(An earlier version of this option kept PON/POP/POSi/POFe as single bulk tracers alongside the per-class POC. That design was architecturally wrong: it created a second, independently-evolving N/P/Si/Fe pool that was only ever supposed to *track* `sum(POC_i * R_XC_pom_i)`, with its own decoupled sinking rate, advection, and boundary conditions -- nothing forced its value to actually equal that sum, so it could silently drift. It has been removed.)

* The four per-class ratio parameters are `R_NC_pom`, `R_PC_pom`, `R_SiC_pom`, and `R_FeC_pom` (N:C, P:C, Si:C, and Fe:C, respectively), each an `npom`-length array read from the `.in` file like any other POM class parameter (see Step 2 below).
* Whenever carbon enters a POM class (plankton mortality, leaky exudation under `DARWIN_EXUDE`, grazing sloppy feeding) or moves between classes (`DARWIN_POMCLASS_DEGFRAG`), the N/P/Si/Fe actually available (from the plankton's own ratio, or from the class the carbon is leaving) generally differs from what the destination class's fixed ratio requires. This difference is created or consumed via DON/DOP/DOFe; since there is no dissolved organic silica pool anywhere in this model, the silica difference is instead created or consumed directly from/to the inorganic SiO2 pool.
* Simple consumption (bacterial uptake of POM) and loss (parameterized remineralization) events need no such reconciliation -- they already behave like the quota-off case does for plankton, consuming/releasing N/P/Si/Fe at a fixed ratio with no competing destination ratio involved.
* PON/POP/POSi/POFe are still available as *output*, computed on demand as `sum(POC_i * R_XC_pom_i)` once per timestep (the same way `DARWIN_CARBON_EXTRAOUT` computes `pH`/`pCO2`/`CO2flx`) -- not stored as tracers, never advected/diffused, just written to the history/average/quicksave files when requested. Enable them per-field with the usual `Hout`/`Qout`/`Aout` keywords, e.g. in the `.in` file:
```
  Hout(idPON) == T
  Hout(idPOP) == T
  Hout(idPOSi) == T
  Hout(idPOFe) == T
```
(`Qout(idPON)`/etc. for the quicksave file, `Aout(idPON)`/etc. for time-averaged output -- all three gated the same `DARWIN_POMCLASS_FIXEDRATIO` way as the fields themselves).

## Steps for adding `DARWIN_POMCLASS_FIXEDRATIO` to a `DARWIN_POMCLASS` configuration

### Step 1: Compile a new ROMS Executable with `DARWIN_POMCLASS_FIXEDRATIO`

Add the option to your `build_roms.sh` compilation script, alongside the existing `DARWIN_POMCLASS` (and, optionally, `DARWIN_POMCLASS_DEGFRAG`) options:
```
MY_CPP_FLAGS="${MY_CPP_FLAGS} -DDARWIN_POMCLASS -DDARWIN_DIM_NPOM=3 -DDARWIN_POMCLASS_FIXEDRATIO"
```

### Step 2: Configure the `darwin_6p4z.in` File

Edit the `darwin_6p4z.in` file.
1. Remove `wP_sink_pday`, `wN_sink_pday`, `wFe_sink_pday`, and `wSi_sink_pday` entirely -- there is no PON/POP/POSi/POFe tracer left to sink, so these keywords no longer exist under `DARWIN_POMCLASS_FIXEDRATIO`. Only `wC_sink_pday` remains, still taking `npom` values, one per POC class.
2. Add the four new ratio parameters, one value per POC class, for example (with `npom`=3):
```
! POM class N:C ratio (mmol N (mmol C)-1)

  R_NC_pom == 0.15d0 0.14d0 0.13d0

! POM class P:C ratio (mmol P (mmol C)-1)

  R_PC_pom == 0.012d0 0.011d0 0.010d0

! POM class Si:C ratio (mmol Si (mmol C)-1)

  R_SiC_pom == 0.05d0 0.04d0 0.03d0

! POM class Fe:C ratio (mmol Fe (mmol C)-1)

  R_FeC_pom == 0.0008d0 0.0007d0 0.0006d0

```
3. Since PON, POP, POSi, and POFe are not tracers at all under `DARWIN_POMCLASS_FIXEDRATIO`, the tracer count/index changes described in `DARWIN_POMCLASS` Step 2 only apply to POC: the configuration has `npom` biological tracers for POM in total (one fewer POM expansion than plain `DARWIN_POMCLASS`, where PON/POP/POSi/POFe are each also split into `npom` classes), and `LBC(isTvar)`, `Hadvection`, `Vadvection`, `TNU2`, and similar variables need to be updated accordingly.

### Step 3: Add Variables to the Initial and Boundary Files

Only POC needs new per-class tracer variables (`POC01`, `POC02`, etc., as in `DARWIN_POMCLASS` Step 3); do **not** add `PON`, `POP`, `POSi`, or `POFe` initial/boundary conditions at all -- they are not tracers under this option. `pomfrac_mort`, `pomfrac_graz` (and, with `DARWIN_POMCLASS_DEGFRAG`, `pom_deg`/`pom_frag`) are still required, exactly as described in `DARWIN_POMCLASS` Step 4 -- they only govern how POC is distributed/transferred between classes, with the N/P/Si/Fe implications reconciled against DON/DOP/DOFe/SiO2 using each class's fixed ratio (see the `DARWIN_POMCLASS_FIXEDRATIO` kernel changes above).

### Step 4: Run ROMS-Darwin

You should now be ready to start the new ROMS executable and perform a Darwin run with fixed-ratio POM classes.
