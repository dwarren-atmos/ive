c-----------------------------------------------------------------------
c     $Id: ovl_props.icl,v 1.6 2000/05/09 19:53:54 harry Exp $
c
c     $Log: ovl_props.icl,v $
c     Revision 1.6  2000/05/09 19:53:54  harry
c     Seperate contour and line colors.  Make patterns work like colors.
c
C Revision 1.5  1995/01/09  18:52:36  warren
C Moved the line color rotation selection from everywhere to makseg.
C Note: makseg now has one argument. it should be called with true if
C you only use hicolor and false if you use two colors in the type of plot.
C
C Revision 1.4  1994/04/14  01:14:52  warren
C Fixed problem where doing overlays in line plots changes the current line
C type for contours.
C
C Revision 1.3  1994/01/20  20:44:41  yuri
C Propagating of "overlay_patterns" stuff to newer version.
C
c-----------------------------------------------------------------------
c
C	This common block is for overlay colors table 
C       
      integer num_of_colors
      parameter(num_of_colors = 5)
      integer  total_colors, highs(5), lows(5)
      integer  total_line_colors, line_colors(5)
      common/ user_lines_colors / total_colors, highs,
     &     lows,  total_line_colors, line_colors
      logical just_set_hicl, just_set_locl
      common/save_ovl/ just_set_hicl, just_set_locl
C     
c     overlay patterns
c     
      integer num_of_ovl_patterns, ovl_patterns(7), total_patterns
      integer total_contour_patterns, highpat(7), lowpat(7)
      parameter(num_of_ovl_patterns = 7)
      common/ovl_ptns/ total_patterns, ovl_patterns, 
     &     total_contour_patterns, highpat, lowpat
              
      logical just_set_hipat, just_set_lopat
      common/c_ovl_patr_just_set/ just_set_hipat, just_set_lopat



      
