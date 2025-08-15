Re=2800, M=0.2, DNS, using compact scheme

Papers:  AIAA 2015-2783  "DNS/LES Simulations of Separated Flows at High Reynolds Numbers"
         June 2015  (P. Balakumar)

         AIAA-2013-2723  "DNS, Enstrophy Balance, and the Dissipation Equation in a
         Separated Turbulent Channel Flow," June 2013 (P. Balakumar, R. Rubinstein, C. Rumsey)
         (earlier related paper... this used high order WENO and M=0.5)

513x257x289 (streamwise, normal, spanwise)

streamwise spacing beteen hills = 9
span extent = 4.5
h=1

uvw-instant.dat - instantaneous u,v,w
dns-avg.dat     - average u,v,rho,production(P),dissipation(e),P/e,k,shear(-uv)
                    UAVG = u-velocity
                    VAVG = v-velocity
                    DAVG = density
                    PROD = production
                    DISSIP = dissipation
                    PRO/DISS = production/dissipation
                    KE = turbulenct kinetic energy
                    SHEAR = negative of turbulent shear stress (-u'v')
profiles.dat    - profiles at specific x-stations (approx 0,1,2,4,5,8)
                    UAVG = u-velocity
                    VAVG = v-velocity
                    UUAVG = u'u'
                    VVAVG = v'v'
                    WWAVG = w'w'
                    UVAVG = u'v'
balance.dat     - profiles of balance terms in tau11,tau22,tau33,tau12,k equations
                  at specific x-stations (approx 0,1,2,4,5,8)
                    Advec  = advection
                    Prod   = production
                    PreStr = pressure-strain
                    Diffu  = diffusion
                    Dissip = dissipation
                    Comp   = contribution from compressibility term
                    Force  = contribution from body force term
                    Total  = sum of all balance terms
                  Note that Comp and Force terms are generally very small compared to the other
                  terms in the balance for this particular problem.
CP.dat          - surface pressure coefficient on lower and upper walls
CF.dat          - surface skin friction coefficient on lower and upper walls

