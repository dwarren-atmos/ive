      subroutine dr2dv (u, v, nu, nv, imap)
c-----------------------------------------------------------------------
c     $Id: dr2dv.F,v 1.6 2007-07-05 19:24:47 warren Exp $
c     Purpose:
c        This routine sets values in the velvct common block, and passes
c        data to the NCAR Graphics vector drawing routine.
c     Arguments:
c        u      real  input  the first vector component.
c        v      real  input  the second vector component.
c        nu     int   input  the first dimension of u and v.
c        nv     int   input  the second dimension of u and v.
c        imap   int   input  the map flag.
c     History:
c        $Log: dr2dv.F,v $
c        Revision 1.6  2007-07-05 19:24:47  warren
c        Final commit of everything pre svn
c
c        Revision 1.5  2006/08/24 22:08:40  warren
c        added vlabel command to get max vectors plotted even if label=off
c
c        Revision 1.4  1999/12/22 00:33:10  warren
c        Fixed Dales crashing bug when changing from point slicer to normal. Also
c        have some ifdef linuxes in there in case we ever find a fortran compiler.
c
c        Revision 1.3  1999/10/26 18:09:16  warren
c        get rid of duplicate define og lable and cal to set itself (getlvar) when the
c        .icl file is included instead.
c
c        Revision 1.2  1999/10/26 17:37:01  harry
c        Add call to new_field_trans in get2dv.  Add line width to vector plots.
c
c        Revision 1.1  1999/08/25 17:50:14  warren
c        swapped from dr2dv.f to dr2dv.F due to #ifdef sun needed as sun does not
c        support the floor function, but has floor_r instead.
c
c        Revision 1.10  1999/08/25 14:57:21  harryive
c        Fix vector scaling for values < 1.0.
c
c        Revision 1.9  1999/02/04 00:28:25  harry
c        Add vector scaling.
c
c        Revision 1.8  1997/02/21 20:02:08  harry
c        Fix vector interval to be floating point and have values less than one.
c        Handling of vector interval is now in the slicer.
c
c        Revision 1.7  1996/08/23 02:12:46  harry
c        More color fixes - specific to "interesting" behavior in NCAR graphics.
c
c        Revision 1.6  1996/05/28 19:06:25  harry
c        Add call to determin line index at beginning of routine.
c
c        Revision 1.5  1996/01/12 16:11:57  warren
c        Transfer of new pattern stuff from 3.x and beginning of color fixes
c        (color vectors, correct collors on the pulldowan menus)
c
c        Revision 1.4  1995/12/15 17:16:51  harry
c        Adjust the location of the maximum vector label.
c
c Revision 1.3  1994/11/22  22:47:45  harry
c Correct vector locking, add support for vector positioning, add
c support for filled contour overlays.  Use new NCAR vector plotting
c routine instead of modified old velvct.
c
c Revision 1.2  1994/03/22  21:32:05  warren
c fixed bad vectors at the bottom of Dale's plot and made vector labels
c behave better (they no longer overwrite the solid fill labels)
c
c Revision 1.1.1.1  1993/04/07  19:39:52  harry
c Initial CVS checkin of IVE.
c
c Revision 2.2  1992/12/07  23:54:22  millerp
c Make vectors black always.
c
c Revision 2.1  1992/09/08  19:53:31  millerp
c Added stuff for new slicers.
c
c Revision 2.0  1992/07/01  21:12:48  millerp
c Released version rereving to 2.0
c
c Revision 1.1  1992/04/21  00:11:26  millerp
c Initial revision
c
c-----------------------------------------------------------------------
c
      include 'ive_widgets.icl'
      include 'vector.icl'
      include 'color.icl'
      include 'plotops.icl'

      real yxfactor
      common /c_vvumxy/yxfactor
c
c
c     Argument declarations.
c
      integer       nu, nv, imap, ll, i, strend, oldclp, crec, n
      integer       text_index, outnum, outsig, outpow
      real          u, v, vrl, vl, vr, vb, vt, ul, ur, ub, ut, lbs
      real          dmx, vmx, vmx2, rnn
      logical       flag, veclock, mapflg, defmap, exact_fit
      character *(80) numlab
      dimension u(nu, nv), v(nu, nv)
c Function declarations
#ifdef sun
      real r_floor
#endif
c
c
c     Local variable declarations.
c
      integer       ierror, line_index
      EXTERNAL VVUDMV
c
c     Use special value feature.
c
      call gstxci(1)
      call gqplci (ierror, line_index)
      if(incx .ge. 1) call vvseti('XIN',incx)
      if(incy .ge. 1) call vvseti('YIN',incy)
      call vvseti('SVF', 3)
      call vvsetr('USV', VECT_SPVAL)
      call vvsetr('VSV', VECT_SPVAL)
      call vvseti('SET', 0)
      call vvseti('VPO', vecpos)
      call vvsetc('MNT', ' ')
      if (label .and. .not.scale_vector) then
         call vvseti('MXP', 4)
         call vvsetr('MXX', 1.0)
         call vvsetr('MXY', -0.04)
         call vvsetc('MXT', 'Maximum Vector')
      else
         call vvsetc('MXT', ' ')
      endif
      call getlvar("defmap",defmap,ierror)
      call getlvar("exact_fit",exact_fit,ierror)
      call getlvar ('mapflg', mapflg, ierror)

c      if (imap.eq.1 .and. .not. (defmap .and. exact_fit)) then
c         call vvseti('MAP', 4)
c      else
c         print *,'call vvseti(MAP, 0)'
c         call vvseti('MAP', 0)
c      endif

      if(mapflg .and. .not. defmap) then
         call vvseti('MAP', 4)
      else
         call vvseti('MAP', 3)
      end if
        
c      
c     Set line color and width
c
      call gsplci (hicolor)
      call vvsetr('LWD', linwdth)
c
c
      call vvinit(u, nu, v, nu, 0, 0, nu, nv, 0, 0)
      call vvgetr('VMX', vmx)
      call vvgetr('DMX', dmx)
      call getlvar('veclock', veclock, flag)
      call getset(vl,vr,vb,vt,ul,ur,ub,ut,ll)
      if (scale_vector) then
         call vvseti('TRT', 1)
         yxfactor = (abs(ur-ul)/(vr-vl))/(abs(ut-ub)/(vt-vb))
      else
         call vvseti('TRT', 0)
      endif
      if (veclock .and. vmax .ne. 0.0) then
         vrl = (vmx/vmax)*dmx/(vr-vl)
         call vvsetr('VRL', vrl)
      else
         vmax = vmx
         call ui_update(UI_VMAX)
         call update_all
         call vvsetr('VRL', 0.)
      endif
      call vvectr(u, v, 0, 0, 0, 0)
      call vvgetr('DMX', dmx)
      call vvgetr('VMX', vmx)
      if (scale_vector .and. (label .or. vlabel)) then
         call vvseti ('VPO', 1)
         call vvgetr ('LBS', lbs)
         call vvsetr ('LBS', 0.01)
         call gqtxci (i, text_index)
         call gqplci (i, line_index)
         call gstxci (1)
         call gsplci (1)
         call gqclip(i, oldclp, crec)
         call gsclip (0)
#ifdef sun
         n = r_floor(alog10(vmx))
#else
         n = floor(alog10(vmx))
#endif
         rnn = int(vmx/10.**n)
         if (vmx .gt. (rnn+0.8)*10.**n) then
            vmx2 = (rnn+1.)*10.**n
         else
            vmx2 = rnn*10.**n
         endif
         dmx = dmx * (vmx2/vmx)
         if (yxfactor. gt. 1) then
            call cpnumb(vmx2,4,-10000,4,0,'E',' ',' ',1,0,0,0,
     &           1,1,numlab,outnum,outsig,outpow)
         else
            call cpnumb(vmx2*yxfactor,4,-10000,4,0,'E',' ',' ',1,0,0,0,
     &           1,1,numlab,outnum,outsig,outpow)
         endif
         if(dmx .lt. .01) then
            call vvdraw(vr-dmx, vb-dmx-0.04, vr, vb-dmx-0.04, dmx,
     &           numlab, strend(numlab), 0, 0, 0)
         else
         call vvdraw(vr-dmx, vb-.02, vr, vb-.02, dmx,
     &        numlab, strend(numlab), 0, 0, 0)
      endif
         if (yxfactor .gt. 1) then
            call cpnumb(vmx2/yxfactor,4,-10000,4,0,'E',' ',' ',1,0,0,0,
     &           1,1,numlab,outnum,outsig,outpow)
         else
            call cpnumb(vmx2,4,-10000,4,0,'E',' ',' ',1,0,0,0,
     &           1,1,numlab,outnum,outsig,outpow)
         endif
         if(dmx .lt. .01) then
            call vvdraw(vr-dmx-0.01, vb-dmx-0.04, vr-dmx-0.01, vb-0.04,
     &           dmx, numlab, strend(numlab), 0, 0, 0)
         else
            call vvdraw(vr+.02, vb, vr+.02, vb+dmx,
     &           dmx, numlab, strend(numlab), 0, 0, 0)
         endif
         call gstxci(text_index)
         call gsplci(line_index)
c         call gsclip(oldclp)
         call vvsetr ('LBS', lbs)
      endif

c
c
c     Reset line color.
c
      call gsplci (line_index)
c
c
      return
      end


#ifdef linux
      integer function floor(r)
      real r
      if(r .ge. 0.) then
         floor = int(r)
      else
         floor = int(r) - 1
      endif
      return
      end
#endif
