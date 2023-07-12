!
! created by create_darwin_headerfiles (written by J. Paul Mattern)
!

/*
************************************************************************
**                                                                    **
**  Assigns metadata indices for Darwin ecosystem model variables     **
**  that are used in input and output NetCDF files. The metadata      **
**  information is read from the file "varinfo.dat".                  **
**                                                                    **
**  This file is included in the file "mod_ncparam.F".                **
**                                                                    **
************************************************************************
*/

/*
**  Model state biological tracers.
*/

            CASE ('idTvar(iDIC)')
              idTvar(iDIC)=varid
            CASE ('idTvar(iNH4)')
              idTvar(iNH4)=varid
            CASE ('idTvar(iNO2)')
              idTvar(iNO2)=varid
            CASE ('idTvar(iNO3)')
              idTvar(iNO3)=varid
            CASE ('idTvar(iPO4)')
              idTvar(iPO4)=varid
            CASE ('idTvar(iSiO2)')
              idTvar(iSiO2)=varid
            CASE ('idTvar(iFeT)')
              idTvar(iFeT)=varid
            CASE ('idTvar(iDOC)')
              idTvar(iDOC)=varid
            CASE ('idTvar(iDON)')
              idTvar(iDON)=varid
            CASE ('idTvar(iDOP)')
              idTvar(iDOP)=varid
            CASE ('idTvar(iDOFe)')
              idTvar(iDOFe)=varid
            CASE ('idTvar(iPOC)')
              idTvar(iPOC)=varid
            CASE ('idTvar(iPON)')
              idTvar(iPON)=varid
            CASE ('idTvar(iPOP)')
              idTvar(iPOP)=varid
            CASE ('idTvar(iPOSi)')
              idTvar(iPOSi)=varid
            CASE ('idTvar(iPOFe)')
              idTvar(iPOFe)=varid
            CASE ('idTvar(iPIC)')
              idTvar(iPIC)=varid
            CASE ('idTvar(ic_)')
              load=.TRUE.
            CASE ('idTvar(in_)')
              load=.TRUE.
            CASE ('idTvar(ip_)')
              load=.TRUE.
            CASE ('idTvar(isi)')
              load=.TRUE.
            CASE ('idTvar(ife)')
              load=.TRUE.
            CASE ('idTvar(iChl)')
              load=.TRUE.
#if defined DARWIN_TOTAL_CHLOROPHYLL
            CASE ('idTvar(iTChl)')
              idTvar(iTChl)=varid
#endif
#if defined DARWIN_CARBON
            CASE ('idTvar(iALK)')
              idTvar(iALK)=varid
            CASE ('idTvar(iO2)')
              idTvar(iO2)=varid
#endif
#if defined DARWIN_CDOM
            CASE ('idTvar(iCDOM)')
              idTvar(iCDOM)=varid
#endif

/*
**  Biological tracers open boundary conditions.
*/

            CASE ('idTbry(iwest,iDIC)')
              idTbry(iwest,iDIC)=varid
            CASE ('idTbry(ieast,iDIC)')
              idTbry(ieast,iDIC)=varid
            CASE ('idTbry(isouth,iDIC)')
              idTbry(isouth,iDIC)=varid
            CASE ('idTbry(inorth,iDIC)')
              idTbry(inorth,iDIC)=varid

            CASE ('idTbry(iwest,iNH4)')
              idTbry(iwest,iNH4)=varid
            CASE ('idTbry(ieast,iNH4)')
              idTbry(ieast,iNH4)=varid
            CASE ('idTbry(isouth,iNH4)')
              idTbry(isouth,iNH4)=varid
            CASE ('idTbry(inorth,iNH4)')
              idTbry(inorth,iNH4)=varid

            CASE ('idTbry(iwest,iNO2)')
              idTbry(iwest,iNO2)=varid
            CASE ('idTbry(ieast,iNO2)')
              idTbry(ieast,iNO2)=varid
            CASE ('idTbry(isouth,iNO2)')
              idTbry(isouth,iNO2)=varid
            CASE ('idTbry(inorth,iNO2)')
              idTbry(inorth,iNO2)=varid

            CASE ('idTbry(iwest,iNO3)')
              idTbry(iwest,iNO3)=varid
            CASE ('idTbry(ieast,iNO3)')
              idTbry(ieast,iNO3)=varid
            CASE ('idTbry(isouth,iNO3)')
              idTbry(isouth,iNO3)=varid
            CASE ('idTbry(inorth,iNO3)')
              idTbry(inorth,iNO3)=varid

            CASE ('idTbry(iwest,iPO4)')
              idTbry(iwest,iPO4)=varid
            CASE ('idTbry(ieast,iPO4)')
              idTbry(ieast,iPO4)=varid
            CASE ('idTbry(isouth,iPO4)')
              idTbry(isouth,iPO4)=varid
            CASE ('idTbry(inorth,iPO4)')
              idTbry(inorth,iPO4)=varid

            CASE ('idTbry(iwest,iSiO2)')
              idTbry(iwest,iSiO2)=varid
            CASE ('idTbry(ieast,iSiO2)')
              idTbry(ieast,iSiO2)=varid
            CASE ('idTbry(isouth,iSiO2)')
              idTbry(isouth,iSiO2)=varid
            CASE ('idTbry(inorth,iSiO2)')
              idTbry(inorth,iSiO2)=varid

            CASE ('idTbry(iwest,iFeT)')
              idTbry(iwest,iFeT)=varid
            CASE ('idTbry(ieast,iFeT)')
              idTbry(ieast,iFeT)=varid
            CASE ('idTbry(isouth,iFeT)')
              idTbry(isouth,iFeT)=varid
            CASE ('idTbry(inorth,iFeT)')
              idTbry(inorth,iFeT)=varid

            CASE ('idTbry(iwest,iDOC)')
              idTbry(iwest,iDOC)=varid
            CASE ('idTbry(ieast,iDOC)')
              idTbry(ieast,iDOC)=varid
            CASE ('idTbry(isouth,iDOC)')
              idTbry(isouth,iDOC)=varid
            CASE ('idTbry(inorth,iDOC)')
              idTbry(inorth,iDOC)=varid

            CASE ('idTbry(iwest,iDON)')
              idTbry(iwest,iDON)=varid
            CASE ('idTbry(ieast,iDON)')
              idTbry(ieast,iDON)=varid
            CASE ('idTbry(isouth,iDON)')
              idTbry(isouth,iDON)=varid
            CASE ('idTbry(inorth,iDON)')
              idTbry(inorth,iDON)=varid

            CASE ('idTbry(iwest,iDOP)')
              idTbry(iwest,iDOP)=varid
            CASE ('idTbry(ieast,iDOP)')
              idTbry(ieast,iDOP)=varid
            CASE ('idTbry(isouth,iDOP)')
              idTbry(isouth,iDOP)=varid
            CASE ('idTbry(inorth,iDOP)')
              idTbry(inorth,iDOP)=varid

            CASE ('idTbry(iwest,iDOFe)')
              idTbry(iwest,iDOFe)=varid
            CASE ('idTbry(ieast,iDOFe)')
              idTbry(ieast,iDOFe)=varid
            CASE ('idTbry(isouth,iDOFe)')
              idTbry(isouth,iDOFe)=varid
            CASE ('idTbry(inorth,iDOFe)')
              idTbry(inorth,iDOFe)=varid

            CASE ('idTbry(iwest,iPOC)')
              idTbry(iwest,iPOC)=varid
            CASE ('idTbry(ieast,iPOC)')
              idTbry(ieast,iPOC)=varid
            CASE ('idTbry(isouth,iPOC)')
              idTbry(isouth,iPOC)=varid
            CASE ('idTbry(inorth,iPOC)')
              idTbry(inorth,iPOC)=varid

            CASE ('idTbry(iwest,iPON)')
              idTbry(iwest,iPON)=varid
            CASE ('idTbry(ieast,iPON)')
              idTbry(ieast,iPON)=varid
            CASE ('idTbry(isouth,iPON)')
              idTbry(isouth,iPON)=varid
            CASE ('idTbry(inorth,iPON)')
              idTbry(inorth,iPON)=varid

            CASE ('idTbry(iwest,iPOP)')
              idTbry(iwest,iPOP)=varid
            CASE ('idTbry(ieast,iPOP)')
              idTbry(ieast,iPOP)=varid
            CASE ('idTbry(isouth,iPOP)')
              idTbry(isouth,iPOP)=varid
            CASE ('idTbry(inorth,iPOP)')
              idTbry(inorth,iPOP)=varid

            CASE ('idTbry(iwest,iPOSi)')
              idTbry(iwest,iPOSi)=varid
            CASE ('idTbry(ieast,iPOSi)')
              idTbry(ieast,iPOSi)=varid
            CASE ('idTbry(isouth,iPOSi)')
              idTbry(isouth,iPOSi)=varid
            CASE ('idTbry(inorth,iPOSi)')
              idTbry(inorth,iPOSi)=varid

            CASE ('idTbry(iwest,iPOFe)')
              idTbry(iwest,iPOFe)=varid
            CASE ('idTbry(ieast,iPOFe)')
              idTbry(ieast,iPOFe)=varid
            CASE ('idTbry(isouth,iPOFe)')
              idTbry(isouth,iPOFe)=varid
            CASE ('idTbry(inorth,iPOFe)')
              idTbry(inorth,iPOFe)=varid

            CASE ('idTbry(iwest,iPIC)')
              idTbry(iwest,iPIC)=varid
            CASE ('idTbry(ieast,iPIC)')
              idTbry(ieast,iPIC)=varid
            CASE ('idTbry(isouth,iPIC)')
              idTbry(isouth,iPIC)=varid
            CASE ('idTbry(inorth,iPIC)')
              idTbry(inorth,iPIC)=varid

            CASE ('idTbry(iwest,ic_)')
              load=.TRUE.
            CASE ('idTbry(ieast,ic_)')
              load=.TRUE.
            CASE ('idTbry(isouth,ic_)')
              load=.TRUE.
            CASE ('idTbry(inorth,ic_)')
              load=.TRUE.

            CASE ('idTbry(iwest,in_)')
              load=.TRUE.
            CASE ('idTbry(ieast,in_)')
              load=.TRUE.
            CASE ('idTbry(isouth,in_)')
              load=.TRUE.
            CASE ('idTbry(inorth,in_)')
              load=.TRUE.

            CASE ('idTbry(iwest,ip_)')
              load=.TRUE.
            CASE ('idTbry(ieast,ip_)')
              load=.TRUE.
            CASE ('idTbry(isouth,ip_)')
              load=.TRUE.
            CASE ('idTbry(inorth,ip_)')
              load=.TRUE.

            CASE ('idTbry(iwest,isi)')
              load=.TRUE.
            CASE ('idTbry(ieast,isi)')
              load=.TRUE.
            CASE ('idTbry(isouth,isi)')
              load=.TRUE.
            CASE ('idTbry(inorth,isi)')
              load=.TRUE.

            CASE ('idTbry(iwest,ife)')
              load=.TRUE.
            CASE ('idTbry(ieast,ife)')
              load=.TRUE.
            CASE ('idTbry(isouth,ife)')
              load=.TRUE.
            CASE ('idTbry(inorth,ife)')
              load=.TRUE.

            CASE ('idTbry(iwest,iChl)')
              load=.TRUE.
            CASE ('idTbry(ieast,iChl)')
              load=.TRUE.
            CASE ('idTbry(isouth,iChl)')
              load=.TRUE.
            CASE ('idTbry(inorth,iChl)')
              load=.TRUE.

#if defined DARWIN_TOTAL_CHLOROPHYLL
            CASE ('idTbry(iwest,iTChl)')
              idTbry(iwest,iTChl)=varid
            CASE ('idTbry(ieast,iTChl)')
              idTbry(ieast,iTChl)=varid
            CASE ('idTbry(isouth,iTChl)')
              idTbry(isouth,iTChl)=varid
            CASE ('idTbry(inorth,iTChl)')
              idTbry(inorth,iTChl)=varid

#endif
#if defined DARWIN_CARBON
            CASE ('idTbry(iwest,iALK)')
              idTbry(iwest,iALK)=varid
            CASE ('idTbry(ieast,iALK)')
              idTbry(ieast,iALK)=varid
            CASE ('idTbry(isouth,iALK)')
              idTbry(isouth,iALK)=varid
            CASE ('idTbry(inorth,iALK)')
              idTbry(inorth,iALK)=varid

            CASE ('idTbry(iwest,iO2)')
              idTbry(iwest,iO2)=varid
            CASE ('idTbry(ieast,iO2)')
              idTbry(ieast,iO2)=varid
            CASE ('idTbry(isouth,iO2)')
              idTbry(isouth,iO2)=varid
            CASE ('idTbry(inorth,iO2)')
              idTbry(inorth,iO2)=varid

#endif
#if defined DARWIN_CDOM
            CASE ('idTbry(iwest,iCDOM)')
              idTbry(iwest,iCDOM)=varid
            CASE ('idTbry(ieast,iCDOM)')
              idTbry(ieast,iCDOM)=varid
            CASE ('idTbry(isouth,iCDOM)')
              idTbry(isouth,iCDOM)=varid
            CASE ('idTbry(inorth,iCDOM)')
              idTbry(inorth,iCDOM)=varid

#endif

/*
**  Biological tracers point Source/Sinks (river runoff).
*/

            CASE ('idRtrc(iDIC)')
              idRtrc(iDIC)=varid
            CASE ('idRtrc(iNH4)')
              idRtrc(iNH4)=varid
            CASE ('idRtrc(iNO2)')
              idRtrc(iNO2)=varid
            CASE ('idRtrc(iNO3)')
              idRtrc(iNO3)=varid
            CASE ('idRtrc(iPO4)')
              idRtrc(iPO4)=varid
            CASE ('idRtrc(iSiO2)')
              idRtrc(iSiO2)=varid
            CASE ('idRtrc(iFeT)')
              idRtrc(iFeT)=varid
            CASE ('idRtrc(iDOC)')
              idRtrc(iDOC)=varid
            CASE ('idRtrc(iDON)')
              idRtrc(iDON)=varid
            CASE ('idRtrc(iDOP)')
              idRtrc(iDOP)=varid
            CASE ('idRtrc(iDOFe)')
              idRtrc(iDOFe)=varid
            CASE ('idRtrc(iPOC)')
              idRtrc(iPOC)=varid
            CASE ('idRtrc(iPON)')
              idRtrc(iPON)=varid
            CASE ('idRtrc(iPOP)')
              idRtrc(iPOP)=varid
            CASE ('idRtrc(iPOSi)')
              idRtrc(iPOSi)=varid
            CASE ('idRtrc(iPOFe)')
              idRtrc(iPOFe)=varid
            CASE ('idRtrc(iPIC)')
              idRtrc(iPIC)=varid
            CASE ('idRtrc(ic_)')
              load=.TRUE.
            CASE ('idRtrc(in_)')
              load=.TRUE.
            CASE ('idRtrc(ip_)')
              load=.TRUE.
            CASE ('idRtrc(isi)')
              load=.TRUE.
            CASE ('idRtrc(ife)')
              load=.TRUE.
            CASE ('idRtrc(iChl)')
              load=.TRUE.
#if defined DARWIN_TOTAL_CHLOROPHYLL
            CASE ('idRtrc(iTChl)')
              idRtrc(iTChl)=varid
#endif
#if defined DARWIN_CARBON
            CASE ('idRtrc(iALK)')
              idRtrc(iALK)=varid
            CASE ('idRtrc(iO2)')
              idRtrc(iO2)=varid
#endif
#if defined DARWIN_CDOM
            CASE ('idRtrc(iCDOM)')
              idRtrc(iCDOM)=varid
#endif

#ifdef DIAGNOSTICS_BIO

/*
**  Biological tracers term diagnostics.
*/
            CASE ('iDbio4(idGrazPr)')
              iDbio4(idGrazPr)=varid
#endif
