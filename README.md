# Compile
1. Make a clone of the repository.
2. Create a "build" folder inside the "Source" folder.
3. Change directory to "build" folder
4. Execute the CMake: cmake ..
5. Create a folder for execution, like "Exec" anyware.
6. Copy the executable "build/bin/WGEN_sa" to the execution folder
7. Run the code as example below

# Execution
Navigate to the executable folder (```WGEN_sa/Executable/WGEN_sa.exe```) and run with the following command line arguments:
StartDate EndDate RandomSeed Mode PathToCLI RHThreshold

## Example:
```>> WGEN_sa.exe --StartDate=1979001 --EndDate=1989001 --RSEED1=1 --MEWTH=W --CLIFILE=../Data/CHER.CLI --RHTHRESHOLD=95 --RHMETHOD=3```

--StartDate: Start date (I7), 

--EndDate: End date (I7), 

--RSEED1: Random number seed (I7), 

--MEWTH: Type of weather generator (W or S for WGEN or SIMMETEO), 

--CLIFILE: Name of the CLI file

--RHTHRESHOLD: RH Threshold to be used

--RHMETHOD: 

   Method 1: WGEN original and module in DSSAT. Uses equation P_wv/P_sat @ T = RH

   Method 2: Desiccation psychrometrics. Uses the general equation P_wv/P_sat @ T = RH. The method to calculate saturation vapor pressures is algebraically manipulated from: https://doi.org/10.1175/JAMC-D-17-0334.1

   Method 3: Uses the same equations as Method 2 for RH, but with recalculated TDEW from [Simulation of assimilation, respiration, and transpiration of crops] by C. T. de Wit (1978)

   
The output file will be created in same directory as the .CLI file with the same name as the .CLI file. In the example above, ```WGEN_sa\Data\CHER.WTG``` will be created.
