###########################################################################
#  Copyright (C) 2000-2013 LexiFi SAS. All rights reserved.               #
#                                                                         #
# This program is free software: you can redistribute it and/or modify    #
# it under the terms of the GNU General Public License as published       #
# by the Free Software Foundation, either version 3 of the License,       #
# or (at your option) any later version.                                  #
#                                                                         #
# This program is distributed in the hope that it will be useful,         #
# but WITHOUT ANY WARRANTY; without even the implied warranty of          #
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the           #
# GNU General Public License for more details.                            #
#                                                                         #
# You should have received a copy of the GNU General Public License       #
# along with this program.  If not, see <http://www.gnu.org/licenses/>.   #
###########################################################################

This directory contains a simplified and stripped down version of
LexiFi's calibration routine for the G2++ model, as defined in Theorem
6.14 of http://web.mst.edu/~bohner/fim-10/fim-chap6.pdf.

This program is released as an example of typical numerical code
implemented in OCaml, in order to provide a realistic benchmark to
people working on optimizing the OCaml compiler.

Usage: simply type 'make'.  This will compile and run a test program.
The code requires OCaml 4.00 or above.

The calibration routine is based on least-square optimization, using
the DE algorithm (in LexiFi's version, this global optimizer is
followed by a local one).  The optimizer is implemented in the
Optimization module.

The Math module provides:
 - an approximation formula for the cumulative distribution function
   for a normal variate
 - a root finder (using Brent's algorithm)
 - coefficients for a Gaussian quadrature (for quick integral based
   on sampling a few points of the integrand)

The G2pp_calibration module implements the core calibration routine.
