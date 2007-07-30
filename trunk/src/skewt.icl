c
c     $Id: skewt.icl,v 1.1 1996/04/25 16:25:32 harry Exp $
c
c     Common block carrying the pointers to the three vector components.
c
c     $Log: skewt.icl,v $
c     Revision 1.1  1996/04/25 16:25:32  harry
c     Add Skew-T plots and the beginning of trajectory plots.
c
c
      character *(80)att_var_skewt, skewt_component
      dimension skewt_component(3)
      common / skewt_pointers /att_var_skewt, skewt_component

