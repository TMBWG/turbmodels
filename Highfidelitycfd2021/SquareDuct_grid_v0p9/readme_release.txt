###############################################################
#
#         Duct-grid generator instruction.
#
# ---------------------------------------------------
#
###############################################################

###############################################################
# Compile the codes by gfortran as below, or ifort, g95, etc.
###############################################################

#------------------------------------------
# compile the grid generation progarm:

  echo " Compiling duct_v0p9.f90..."
  gfortran -O2 -o duct_executable duct_v0p9.f90

#----------------------------------------------------------------------
# Endianness will be automatically detected, but if you wish,
# you can specify endianness at compilation: e.g., as follows:
#  gfortran -O2 -fconvert=big-endian -o duct  duct_v0p9.f90
#     ifort -O2 -convert big_endian  -o duct  duct_v0p9.f90
#----------------------------------------------------------------------

###############################################################
#
# ----- Sample 1: Duct with leading edge.
#
# Generate structured (hex) grids with the sample input file:
#   input.nml_sample_le
#
# Note: the program reads 'input.nml', so rename or copy it.
#
# Or you can use input.nml_960x160x160_le instead of
# input.nml_sample_le, which is about the same size as the
# finest grid used in the supersonic duct case.
###############################################################

# Copy and create input.nml

   cp input.nml_sample_le  input.nml

# run the program

  ./duct_executable

# Visualize the grid by Tecplot to see how the domain boundary
# is split into different parts.



###############################################################
#
# ----- Sample 2: Duct with leading and trailing edges.
#
# Use input.nml_sample_lete or input.nml_960x160x160_lete
# to generate grids with a downstream symmetry-bc region.
# This is similar to a flat plate with LE and TE: the duct has
# a leading edge and a trailing edge.
#
###############################################################

# Copy and create input.nml (for hexa grids)

   cp input.nml_sample_lete  input.nml

# run the program

  ./duct_executable

# Visualize the grid by Tecplot to see how the domain boundary
# is split into different parts.



###############################################################
#
# ----- Sample 3: Duct with leading and trailing edges and
#                 with simple stretching.
#
# This uses a simple stretching to generate pts in z-direction,
# and will generate a very similar grid to the one available
# at NASA TMR website.
#
###############################################################

# Copy and create input.nml (for hexa grids)

   cp input.nml_672x192x192_type1_lete input.nml

# run the program

  ./duct_executable

# Visualize the grid by Tecplot to see how the domain boundary
# is split into different parts.





##################################################
# To generate prismatic grids

   cp input.nml_sample_lete_prs  input.nml
  ./duct_executable

##################################################
# To generate tetrahedral grids

   cp input.nml_sample_lete_tet  input.nml
  ./duct_executable





#######################################################################
#######################################################################
# NOTE:    To convert .ugrid+.mapbc file to a CGNS file.
#
#  Install the CGNS package (see https://linkprotect.cudasvc.com/url?a=http%3a%2f%2fcgns.github.io&c=E,1,C4bMfiuVwO581MW7FFf5OPD6e96ENO-sp_QAdTBwEl_TOF-qrFVgEjnGTWmo81hFBA6YhYmqktLLPbljY51xFLkBO6Zyw36_JMMnbAprPt0v6k3OaZLkK-_dBw,,&typo=1), and use 
#  the utility code 'aflr3_to_cgns': at a terminal (command prompt),
#
# %$HOME/cgnslib/bin/cgnstools/aflr3_to_cgns duct.1.b8.ugrid duct.cgns
#
#  where $HOME/cgnslib/ is the directory of the CGNS library installed.
#######################################################################
#######################################################################
