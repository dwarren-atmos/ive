#ifdef SIXTY_FOUR_bptr
#define ive_ptr integer*8
#else
#define ive_ptr integer
#endif
c-----------------------------------------------------------------------
c     Expression Evaluation Utility
c     $Id: getvar.F,v 1.30 2001/04/11 19:11:03 harryive Exp $
c     $Log: getvar.F,v $
c     Revision 1.30  2001/04/11 19:11:03  harryive
c     Add new_field_trans and load_atts calls to the end of getvar.  Remove them
c     from other places where they are now no longer needed.
c
c     Revision 1.29  2001/03/26 20:34:42  warren
c     Added log10 function to getvar.
c
c     Revision 1.28  2000/12/14 00:46:57  warren
c     Only run through ive_fix_var_ (fpe catcher) if new field.
c
c     Revision 1.27  2000/12/06 22:47:59  warren
c     Fix fpe data once and for all
c
c     Revision 1.26  1997/04/11 18:48:20  warren
c     Fixed for sgi and also to update color widget more frequently.
c
c     Revision 1.25  1997/02/12 17:36:07  warren
c     Changed pointer type to ive_ptr to avoid interaction with F90 pointer type.
c
c     Revision 1.24  1996/03/05 18:21:58  harry
c     Isolate the use of "long" for dimension sizes to the netcdf routines.
c     Everywhere else we will use "int" or "integer".  Also change use of
c     long to nclong as a netCDF type as required by netCDF version 2.4.
c
c     Revision 1.23  1995/12/18 00:22:51  warren
c     Ultrix doesn't like multiple declarations
c
c     Revision 1.22  1995/11/14 18:31:43  harry
c     Code to properly change variables between two grids (a@b).
c
c Revision 1.21  1995/08/24  22:19:02  harry
c Ive_ptrs is as ive_ptrs does.
c
c Revision 1.19  1995/06/23  21:16:20  warren
c First part of new ive help
c Changed " to ' in fortran files
c Fixed the label spacing in 1d plots
c added frame_width command
c
c Revision 1.18  1995/05/16  16:38:27  harry
c Fix another problem with newstag.
c
c Revision 1.17  1995/05/05  22:23:10  harry
c Correct "@" operator for case when dimensions < 4.
c
c Revision 1.16  1995/03/01  23:16:37  warren
c Fixed Rajul's crashing on ALPHA bug and in the process found a couple of
c variables that were read before they were set.
c
c Revision 1.15  1994/04/11  21:57:58  harry
c Make sure the user is setting ndims to something other than zero in
c calc_field.
c
c Revision 1.14  1994/03/10  18:44:59  harry
c Correctly print out messages if the user asks for a non-existent field.
c
c Revision 1.13  1994/03/02  22:24:30  harry
c Call catnull for dim_names before calling add_field.
c
c Revision 1.12  1994/02/17  21:36:20  harry
c Add dim_names to getvar.  Also include modified definition of the flag.
c
c Revision 1.11  1993/11/29  19:39:25  warren
c Changes to 3.1 to make it work on alphas.
c FORTRAN: new data type ive_ptr ifdefed to either 32 bit or 64 bit.
c C: Change int to long where necessary.
c Files: Quite a few .f files are now .F
c
c Revision 1.10  1993/11/03  23:47:40  warren
c general cleanup for port to alpha including getting rid of uchar and ushort.
c
c Revision 1.9  1993/09/24  18:44:45  warren
c changed "" to " " for DEC compiler
c
c Revision 1.8  1993/09/23  23:02:14  harry
c Include dimension names in calculations.
c
c Revision 1.7  1993/09/10  20:44:12  warren
c fixed syntax error dealing with mismatched parens.
c
c Revision 1.6  1993/08/18  15:47:32  harry
c Change to new transform routines.
c
c Revision 1.5  1993/08/18  15:39:58  harry
c Minor bug fix by Christoph.
c
c Revision 1.4  1993/07/09  21:45:09  harry
c Clean up unused variable declarations.
c
c Revision 1.3  1993/07/09  20:33:27  harry
c Non-zero returns from calc_field are always "new".
c
c Revision 1.2  1993/06/29  17:27:07  harry
c New version of getvar from Christoph.
c
c Revision 1.1.1.1  1993/04/07  19:39:58  harry
c Initial CVS checkin of IVE.
c
c Revision 2.15  1993/03/22  18:26:08  warren
c Christoph's fixes for aliasing
c
c Revision 2.14  1993/03/05  18:41:35  warren
c changed to call calcfld directly and call compare_var to see if it is new.
c
c Revision 2.13  1993/01/13  20:07:29  millerp
c Remove multiple declaration of variable.
c
c Revision 2.12  1993/01/13  00:08:11  millerp
c Fix error in format statement.
c
c Revision 2.11  1993/01/12  23:53:06  millerp
c Add check on incoming variable string in getvar.
c
c Revision 2.10  1993/01/12  23:48:42  millerp
c Add check on incoming variable string in getvar.
c
c Revision 2.9  1993/01/11  23:58:18  millerp
c Blank out var for each call to getvar before filling with new string.
c
c Revision 2.8  1993/01/11  22:59:14  warren
c changed to delete vars as we go and copy the var name before changing the
c memory.
c
c Revision 2.7  1993/01/04  16:06:25  millerp
c Make field length 80 chars long.
c
c Revision 2.6  1992/12/31  18:49:44  millerp
c Make changes for new getvar.F.
c
c Revision 2.5  1992/10/02  01:03:41  warren
c fixed to only show the asked for field.
c
c Revision 2.4  1992/09/22  20:27:30  warren
c added Christoph's changes.
c
c Revision 2.3  1992/09/08  19:57:25  millerp
c Added stuff for new slicers.
c
c Revision 2.1  1992/07/13  21:45:23  millerp
c Null-terminate all strings before returning to a C routine.
c
c Revision 2.0  1992/07/01  21:15:28  millerp
c Released version rereving to 2.0
c
c Revision 1.5  1992/06/22  22:52:14  millerp
c Fixed up expression calculations so info is stored in data
c structure.
c
c Revision 1.4  1992/05/29  22:33:05  millerp
c Changed argument lists for units.
c /
c
c Revision 1.2  1992/04/28  23:52:47  millerp
c Modified to allow recursive calls. getvar now calls get_var or get_d_var,
c as appropriate.
c
c Revision 1.1  1992/04/21  00:11:26  millerp
c Initial revision
c
c
c     History:
c       Christoph Schar, 2/92, recursive structure, basic mathematical
c       functions and operations, interpolation-operation
c
c     The following functions and subroutines allow for the computation 
c     of almost arbitrary expressions in the field=something command. 
c     In particular, data-fields (also with missing data) in 
c     combination with scalar values are fully supported. There is no
c     limit to the complexity of any expression, except for the 
c     recursion-depth (depends on the compiler) and for the length
c     of the command string (20 characters right now, but will be
c     extended to a larger number). The utility cracks the
c     expression into subexpressions, using standard mathematical rules,
c     and tries 
c     -  to find the subexpression in memory (e.g., the result of
c        an earlier computation),
c     -  to find the subexpression on the data-file,
c     -  to compute the subexpresison by a user-written piece of code
c        in routine CALC_FIELD,
c     -  to evaluate the subexpression by mathematical operations.
c     
c
c     Supported Operations
c     --------------------
c     The expression evaluation utility supports the brackets (), {}
c     and []. Most mathematical standard operations are allowed. They
c     are here listed with ascending binding power.
c
c     +,-  Addition and subtraction.
c
c     *,/  Multiplication and Division.
c          The expression A/B*C is interpreted as (A/B)*C, i.e. as
c          in fortran. Divisions by zero cause the flagging of the
c          result as missing.
c
c     ^    Power-operation. Two version of this command are implemented:
c          - If you enter 'field=A^2', i.e. you take the whole field to
c            the same integer-power, then the fortran integer operation
c            is used. This operation is also defined for A(i,j,k)<0.
c          - If you enter 'field=A^B' or field=A^0.5, i.e. you take the
c            field to a real power, then the fortran real operation
c            is used. This requires A(i,j,k)>=0. For A(i,j,k)<0 the 
c            resulting data value is flaged as missing. 
c
c     @    Interpolation operation.
c          This command has maximum binding power and serves to 
c          interpolate one field to the grid of another field. 
c          All the aforementioned operations require fields with
c          the same attributes (exception see below). In case of a
c          staggered data-set, the fields used in an operation have
c          first to be interpolated on the same grid. This is done
c          with the operation @. For instance, A@B denotes the 
c          A-field on the grid of B, i.e. A at B. 
c
c          
c     Supported Functions
c     -------------------
c     In addition, the functions MAX[a:b], MIN[a:b] and ABS[a] are
c     supported. The delimiter ':' is used until the delimiter ',' 
c     becomes available (currently reserved for IVE). It is easy 
c     to add new functions to the current structure, as long as they
c     need 1 or 2 arguments only.
c
c
c     Restrictions
c     ------------
c     The @-operation requires that the relevant fields have the same
c     number of dimensions, but they can have arbitrarily different 
c     attributes otherwise.
c
c     All other operations between two fields do not required identical
c     number of dimensions. You can do an operation A*B, where A and
c     B are dimensioned as A(15,1,10) and B(15,12).  In this case it is 
c     assumed that A is uniform in the y-direction, and B is uniform 
c     in the z-direction. The operations however require that any 
c     dimensions which are greater than 1 do match, i.e. operations
c     between A(20,30) and B(50,30) are not possible directly, but only
c     after interpolation to the same grid, i.e. as A@B*B.
c
c     It follows that a mean-array in the YZ-plane (in absence of 
c     terrain-transformations) should preferrably  be defined 
c     as MEAN(1,NY,NZ), and not as MEAN(NY,NZ). 
c
c     Note that the operation A^-0.5 is not defined, but A^(-0.5) is.
c     The same is the case for A*-0.5 and A*(-0.5).
c
c     There are no overflow checks, i.e. you can stop the execution
c     by asking for field=something^1000.
c
c
c     Compatibility with user written functions
c     -----------------------------------------
c     There is full compatibility with user-written functions in
c     CALC_FIELD. The user can, for instance, provide the code to
c     compute the derivative (e.g. DER[U,X] = derivative of U in
c     the direction of X) and combine this syntax with any of the
c     here provided features.
c
c     The user has access to the fortran-callable routine GETVAR
c     (for use in subroutine CALC_FIELD) to simplify the derivation of
c     his own fields. The routine GETVAR can be called in order to
c     provide access to one particular field which is available on
c     the data file or in memory, or in order to compute an expression 
c     which can be handeled by this utility (or further recursive 
c     calls to CALC_FIELD). Further comments in subroutine CALC_FIELD.
c
c     The interpolation operation is fully general and uses 
c     the user-written routines INDEX_2_PHYS, PHYS_2_INDEX and NEW_FIELD. The 
c     resulting overhead might be noticable for three-dimensional
c     fields. The user can write faster routines (which can use
c     apecifics) within CALC_FIELD. Any such routine has priority 
c     over the standard interpolation routine, provided CALC_FIELD 
c     returns successfully.
c
c
c     Examples
c     --------
c     Assume that your datafile contains (or your version of
c     CALC_FIELD supports) the variables T,P,RH,PM,U,V,W. Some examples:
c
c     field=U-UM     or     field=U-10           
c           Display the wind-perturbation, or the wind with an offset.
c
c     field=T*[(P+PM)/1000]^0.286
c           Computes the sensible temperature from the potential
c           temperature T, the mean pressure PM and the perturbation
c           pressure P.
c
c     field=(U^2+V^2+W^2)^0.5
c           Computes the velocity from the wind-vector (U,V,W). Here 
c           it is assumed the components (U,V,W) are all defined 
c           on the same grid (i.e. have the same attributes). If
c           you use a staggered dataset, see the following example.
c
c     field=(U@T^2+V@T^2+W@T^2)^0.5   
c           As previous example, but the three velocity-components are
c           first interpolated from their (staggered) grid onto the
c           grid defined by the variable T (e.g. the thermodynamic
c           grid). 
c
c     field=MAX[0:U]
c           Cuts away all negative values of U.
c
c     field=(3^2+4.5^2)^0.5
c           You can also use this utility as a 'pocket calculator'.
c           The result of these operations are displayed, but not 
c           plotted. 
c-----------------------------------------------------------------------




      ive_ptr function addvar (var, but, ndims, dimlst, vstag, 
     &                 vphmin, vphmax, misdat,
     &                 vdata_units, vdata_display_units, 
     &	               dim_names, flag)
c-----------------------------------------------------------------------
c     Purpose:
c        This routine has the same purpose (and almost the same parameters)
c        as getvar, but in addition it does:
c        - add the variable to the variable-table, if the variable is new,
c        - install a new button (with a name that may differ from that
c          of the variable) if the variable is not yet available.
c        - print error messages
c     Arguments:
c        addvar  int   output  a ive_ptr to the memory location of the
c                              beginning of the data. If the routine
c                              returns with getvar=0 an error has
c                              occured.
c        var     char  input   the variable name.
c        but     char  input   the button name.
c        ndims   int   output  the number of dimensions of the variable.
c        dimlst  int   output  the number of data points along each 
c                              dimension. 
c                              For example, if dims(1)=nx, dims(2)=ny, 
c                              dims(3)=nz, this is a 3D data
c                              set that is nx X ny X nz.
c        vstag   real  output  the grid staggering along each dimension.
c        vphmin  real  output  the location of the origin of the data
c                              in physical space.
c        vphmax  real  output  the extent of the data in physical space
c                              along each dimension.
c        misdat  real  output  the missing data value. Any point whose
c                              value is misdat will be ignored by the
c                              plotting routines.
c        flag    logi  input/  On input, .true -> write error message if
c                              variable is not found or computed.
c                      output  Is .true. on exit if new memory was 
c                              allocated for this variable, .false.
c                              otherwise.
c     History:
c-----------------------------------------------------------------------
c
      include 'message.icl'
      include 'data.icl'
c      include 'units.icl'

      logical made_newbut
      common  /newbut/ made_newbut

#ifdef sun
      implicit automatic (a-z)
#endif

c     Argument declarations.
c
      integer    UNIT_LEN
      parameter (UNIT_LEN = 80)
      character*(*)  var,but
      character *(UNIT_LEN) vdata_units, vdata_display_units,
     &	         dim_names(MAXDIM)
      integer        ndims
      integer	     dimlst(MAXDIM)
      real           vstag(MAXDIM), vphmin(MAXDIM), vphmax(MAXDIM), 
     &               misdat
      logical        flag


c     Local variable declarations.
c
      character*(MAX_FIELD_LENGTH)  but0, var0
      integer        varid
      ive_ptr        getvar
      integer        strbeg,strend,getvid,isperm
      real           r
      logical        error, print_message

c     assign strings with constant field length
      var0(1:len(var0)) = ' '
      but0(1:len(but0)) = ' '
      var0 = var(strbeg(var):strend(var))
      but0 = but(strbeg(but):strend(but))

c     The message-string 'message' is initially set to " " indicating
c     no error. Error messages are displayed prior to completion of the
c     routine (if an error occurs), and only by addvar (and not by
c     the other routines called by addvar and getvar). The daughter
c     routines may however define the error-message through writing
c     to the variable 'message', after checking that an earlier error 
c     has not yet occured (signaled by message=" "). This makes 
c     sure that only the first occurence of an error (which is the one
c     to cause the problem) is displayed to the user.

      print_message = flag
      message=' '

c     call the function getvar to do the computations or get the
c     variable from memory or file.

      addvar=getvar (var0, ndims, dimlst, vstag, vphmin, vphmax, 
     &               misdat,
     &               vdata_units, vdata_display_units, dim_names, flag)


      if (addvar.gt.0) then
c     
c        the call to getvar was succsessful
c     
         if ((ndims.eq.1).and.(dimlst(1).eq.1)) then
c     
c           the variable is a scalar
c     
            call scalass(%val(addvar),r)
            if (r.ne.misdat) then 
               write (message, 110) var0(strbeg(var0):strend(var0)),r
            else
               write (message, 111) var0(strbeg(var0):strend(var0))
            endif
c           free this variable, since it cannot be plotted
            call freemem(addvar)
            addvar=0

         else if (isperm(var0(strbeg(var0):strend(var0))).eq.0) then
c           This is a new variable. Install a new button. 

c           Don't through away a good var due to name conflict.
c           First check whether the button-name 'but' is available for 
c           use.
 5          varid = getvid (but0(strbeg(but0):strend(but0)))
            if (varid.ge.0 .and. but0(strbeg(but0):strend(but0)) .ne. 
     &           var0(strbeg(var0):strend(var0))) then
c     
c              This button-name is already in use. 
               but0=but0(strbeg(but0):strend(but0))//'_'
               go to 5
            endif
c     
            call catnull (but0, len(but0), error)
            
c           Set name
            call set_button_name (var0(strbeg(var0):strend(var0)), but0)
c           Keep it (don't collect with garbage)
            varid = getvid (var0(strbeg(var0):strend(var0)))
            call keep_var( varid)
            made_newbut=.true.

cc           testoutput
c	     write (message, 999) var0(strbeg(var0):strend(var0)),
c     &         varid,char(10),but0(strbeg(but0):strend(but0))
c 999        format (1x, 'New Variable ',a,' in ADDVAR with id=',i8,
c     &              a,1x,'for button ',a)
c            call write_message

         endif
      endif
c     
c      
      if (addvar.eq.0) then
c        the variable was not found, show error message
         if (message.eq.' ') then
c           An error message has not yet been defined, do it now.
            write (message, 100) var0(strbeg(var0):strend(var0))
         endif
	 if (print_message) call write_message
      endif
      
c     Format statements.
 100  format (1x, 'The variable ',a,' cannot be found in the data ',
     &     'file and cannot be computed.')
 110  format (1x,'The scalar ',a,' = ',f,' cannot be plotted.')
 111  format (1x,'The scalar ',a,' cannot be computed.')
 120  format (1x, 'The button-name ',a,' is already in use.')
      end



      ive_ptr function getvar (dummyv, ndims, dims, stag, 
     &                         datmin, datmax, misdat, 
     &                         data_units, data_display_units, 
     &	                       dim_names, flag)
c-----------------------------------------------------------------------
c     Purpose:
c        This routine tries to provide variable var by
c        - getting it from memory,
c        - getting it from the data file,
c        - computing it with user-written routine CALC_FIELD, or
c        - evaluating it with mathematical operations.
c     Arguments:
c        getvar  int   output  a ive_ptr to the memory location of the
c                              beginning of the data. If the routine
c                              returns with getvar=0 an error has
c                              occured.
c        dummyv  char  input   the variable name.
c                              if  eliminated from calc_field)
c        ndims   int   output  the number of dimensions of the variable.
c        dims    int   output  the number of data points along each 
c                              dimension. 
c                              For example, if dims(1)=nx, dims(2)=ny, 
c                              dims(3)=nz, this is a 3D data
c                              set that is nx X ny X nz.
c        stag    real  output  the grid staggering along each dimension.
c        datmin  real  output  the location of the origin of the data
c                              in physical space.
c        datmax  real  output  the extent of the data in physical space
c                              along each dimension.
c        misdat  real  output  the missing data value. Any point whose
c                              value is misdat will be ignored by the
c                              plotting routines.
c        flag    logi  input/  On input, .true -> write error message if
c                              variable is not found or computed.
c                      output  Is .true. on exit if new memory was 
c                              allocated for this variable, .false.
c                              otherwise.
c     History:
c-----------------------------------------------------------------------
c
#ifdef sun
      implicit automatic (a-z)
#endif

      include 'message.icl'

c     Argument declarations.
c
      integer          MAXDIM, MAX_FIELD_LENGTH, UNIT_LEN
      parameter (MAXDIM = 4, 
     &           MAX_FIELD_LENGTH = 80, 
     &           UNIT_LEN = 80)
      character *(UNIT_LEN) data_units, data_display_units,
     &           dim_names(MAXDIM)
      character *(*) dummyv
      integer        ndims, total_dims
      integer dims(MAXDIM)
      real           stag(MAXDIM), datmin(MAXDIM), datmax(MAXDIM), 
     &               misdat
      logical        flag

c
c     Local variable declarations.
c
      character*(MAX_FIELD_LENGTH) var,var0,var1,var2
      character*20   oper
      integer        i, varid, status
      integer        ibeg1,iend1
      logical        error
      
c     External function declarations.
c     
      ive_ptr        read_var, calc_field_trans, add_field,
     &               getexpr, getnumb, fixvar
      integer        getvid, strbeg, strend

      if (flag) then
	 message=' '
      else
	 message='ok'
      endif

c     copy input string into string we really use.

      var(1:len(var)) = ' '
      ibeg1 = strbeg(dummyv)
      iend1 = strend(dummyv)
      if ( (iend1-ibeg1+1) .lt. MAX_FIELD_LENGTH ) then
         var = dummyv(ibeg1:iend1)
      else
	 write (message, 910) dummyv(ibeg1:iend1)
 910     format (1x, 'Input string ',a,' to getvar too long.')
         call write_message
      endif

c     
c     Initialize return values. 
c     
      do 10 i = 1, MAXDIM
         dims(i) = 1
         stag(i) = 0.0
         datmin(i) = 0.0
         datmax(i) = 0.0
	 dim_names(i) = ' '
 10   continue
      misdat = 0.0
      data_units(1:len(data_units)) = ' '
      data_display_units(1:len(data_units)) = ' '
c     
c     
c     Mark the ive_ptr as unused.
      getvar=0
      
c     get rid of unnessesary brackets and check the syntax     
      call bracket(var,ibeg1,iend1,error)
      if (error) then
c        ------------------------------------------------
c        Subroutine bracket found inconsistent structure.
c        ------------------------------------------------
         getvar=0
         if (message.eq.' ') then
            write (message,800) var(1:strend(var))
         endif
 800     format ('Syntax error: Unbalanced Parenthesis in FIELD=',a)
         goto 900
      endif
      var0=var(ibeg1:iend1)
      
      
c     Check whether the variable is a number.
      getvar= getnumb(var0,ndims,dims,stag,datmin,datmax,misdat)
      if (getvar.ne.0) then
c        ------------------------
c        The variable is a number
c        ------------------------
c         flag=.true.
         flag=.false.
         goto 900
      endif
      
      
c     Check whether the variable is in memory.
      varid = getvid (var0(strbeg(var0):strend(var0)))
      if (varid.ge.0) then
c        -----------------------------------------------------------
c        The variable is in memory already. 
c        -----------------------------------------------------------
c        Get its ive_ptr and 
c        attributes.  The starting address of the data is returned 
c        in getvar. The number of dimensions is returned in ndims, 
c        and the dimensions are returned in dimlst. Note that since 
c        c programs are doing the actual reading, the list of 
c        dimensions is backwards with respect to fortran. For 
c        example, suppose the data is in an array ARRAY (nx,ny,nz). 
c        Then dimlst(0)=nz, dimlst(1)=ny, and dimlst(2)=nx. Here
c        we also get the grid staggering and the physical domain
c        limits.

         call catnull (data_units, len(data_units), error)
         call catnull (data_display_units, len(data_display_units), 
     &        error)
	 do i = 1, MAXDIM
	    call catnull (dim_names(i), len(dim_names(i)), error)
	 enddo
         getvar = read_var (varid, ndims, dims, stag, 
     &        datmin, datmax, misdat,
     &        data_units, data_display_units, dim_names)
         flag=.false.
         goto 900
      endif
      
      
c     Check whether the variable is one that the user wants to compute.
c     This is done before the code attempts to compute an expression,
c     since the user might install a shortcut for some of the 
c     computations.
c
      ndims = 0
      call catnull (VAR0, len(VAR0), error)
      getvar = calc_field_trans (var0(strbeg(var0):strend(var0)), 
     &      ndims, dims, stag, 
     &      datmin, datmax, misdat, data_units(1:UNIT_LEN), 
     &      data_display_units(1:UNIT_LEN),
     &	    dim_names)  
      if (getvar .gt. 0 .and. ndims .gt.0 ) then
c        -----------------------------------------------------------
c        This is a variable which is provided by the user.
c        -----------------------------------------------------------
	 flag = .true.
c         goto 850
      endif
      
      
c     Check whether the variable is an expression. 
      call fnddel(var0,'+-*/^@',var1,var2,oper)
c     Make sure that there is a second argument (the first argument is
c     optional for some of the operations and is checked later in getexpr)
      if ((oper.ne.'#').and.(var2.eq.'#')) oper='#'
c     If oper='@', make sure the second argument does not start with a
c     digit.
      if ((oper.eq.'@').and.(var2(1:1).ge.'0').and.(var2(1:1).le.'9'))
     &   oper='#'
c     If no operation, check whether the variable is a function call
      if (oper.eq.'#') call fndfun(var0,var1,var2,oper)
      if (oper.ne.'#') then
c        -----------------------------------------------------------
c        var is an expression. Compute the two terms and evaluate the
c        expression.
c        -----------------------------------------------------------
         getvar=getexpr(oper,var1,var2,ndims,dims,stag,datmin,datmax,
     &        misdat, data_units, data_display_units, dim_names)
         if ( getvar.gt. 0 ) then
c           Add the new variable to the data structure.
            flag=.true.
c            goto 850     
         endif
      endif

      if(flag) goto 850
      
c     the variable was not found, go to exit
      goto 900


 850  continue
c     --------------------------------------
c     add new variable to the data-structure
c     --------------------------------------
      if (getvar.gt.0) then
c       do not add scalers to the data-structure
        if ((ndims.gt.1).or.(dims(1).gt.1)) then
          call catnull (data_units, len(data_units), error)
          call catnull (data_display_units, len(data_display_units),
     &           error)
          call catnull (var0, len(var0), error)
          total_dims=1
	  do i = 1, ndims
	     call catnull (dim_names(i), len(dim_names(i)), error)
	  enddo
          status = add_field (getvar, varid, var0, ndims, dims,
     &           stag, datmin, datmax, misdat, data_units,
     &           data_display_units, dim_names) 
          if ( status .eq. 0 ) then
            if (flag) call freemem(getvar)
            flag=.false.
            getvar = 0
          endif
        endif
      endif

       
 900  continue
c     ------------
c     exit routine
c     ------------
      if (getvar.eq.0) then
         if (message.eq.' ') then
            write (message, 110) var(strbeg(var):strend(var))
         endif
 110     format (1x, 'The variable ',a,' cannot be found in the data ',
     &        'file and cannot be computed.')
      else
c     Check for NaNs
         total_dims=1
         do i = 1, ndims
            total_dims = total_dims * dims(i)
         enddo
         if(flag) then
            fixvar=getvar
            call ive_fixdata(%VAL(fixvar), misdat, total_dims)
         endif
C
C     Call load_atts and new_field_trans
C
         call load_atts(ndims, dims, stag, datmin, datmax, misdat,
     &        data_units, data_display_units, dim_names)
         call new_field_trans(dummyv, %val(getvar), dims(1), dims(2),
     &        dims(3), dims(4))
cc        test-ouptut
c         if (flag) then
c           write (message, 155) var(strbeg(var):strend(var))
c         else
c           write (message, 156) var(strbeg(var):strend(var))
c         endif
c 155     format (1x, 'Leaving GETVAR with ',a,', new=true')
c 156     format (1x, 'Leaving GETVAR with ',a,', new=false')
c         call write_message
      endif
      end



      subroutine bracket(fld,ibeg,iend,error)
c-----------------------------------------------------------------------
c     this subroutines takes a command-string, checks for 
c     consistent bracket notation and removes unnessesary brackets.
c     The implemented brackets are (),[],{}. The string fld(ibeg:iend)
c     is the relevant substring on output.
c-----------------------------------------------------------------------

      character*(*)  fld
      character*(1)  bo(3), bc(3)
      integer        cb(3),i,k,ibeg,iend,strbeg,strend
      logical        error,nested

      data           bo / '(','{','['/
      data           bc / ')','}',']'/

      ibeg=strbeg(fld)
      iend=strend(fld)
      error=.false.

c     check for balanced brackets
      do i=1,3
        cb(i)=0
      enddo
      do k=ibeg,iend
        do i=1,3
          if (fld(k:k).eq.bo(i)) cb(i)=cb(i)+1
          if (fld(k:k).eq.bc(i)) cb(i)=cb(i)-1
          if (cb(i).lt.0) error=.true.
        enddo
      enddo
      if ((cb(1).ne.0).or.(cb(2).ne.0).or.(cb(3).ne.0)
     &  .or.(error)) then
        error=.true.
        return
      endif

c     remove unnessary brackets
 100  if (iend-ibeg.gt.1) then
        k=0
        do i=1,3
          if ((fld(ibeg:ibeg).eq.bo(i)).and.(fld(iend:iend).eq.bc(i)))
     &      k=i
        enddo
        if (k.gt.0) then
          cb(k)=1
          nested=.false.
          do i=ibeg+1,iend-1
            if (fld(i:i).eq.bo(k)) cb(k)=cb(k)+1
            if (fld(i:i).eq.bc(k)) cb(k)=cb(k)-1
            if (cb(k).eq.0) nested=.true. 
          enddo
          if (.not.nested) then
            ibeg=ibeg+1
            iend=iend-1
            goto 100  
          endif
        endif
      endif
      end


      logical function isfunc(expr,fun,arg1,arg2,arg3)
c-----------------------------------------------------------------------
c     Purpose:
c        This function checks whether the expression 'expr' 
c        represents a function involving the function-name 'fun'. 
c        The arguments of the function must be delimited by (), [], 
c        or {}. Valid argument-delimiters are ',' and ':'. A maximum 
c        of 3 arguments can be specified. If less then 3
c        arguments are specified, the unspecified arguments are
c        returned as '#'. 
c     Arguments:
c        expr   chrs  input   the expression to be tested
c        fun    chrs  input   the function-name to be screend for
c        arg1   chrs  output  the first  argument
c        arg2   chrs  output  the second argument
c        arg3   chrs  output  the third  argument
c        isfunc logi  output  .true. if 'expr' is of the form
c                             fun(arg1,arg2,arg3), where either (), []
c                             or {} can be used to delimit the arguments,
c                             and either ',' or ':' to separate the
c                             arguments.
c     Examples:
c        isfunc('ABS[expression]','ABS',..)   returns isfunc=.true.
c                                                     arg1='expression'
c                                                     arg2='#'
c                                                     arg3='#'
c        isfunc('ABS(exp1,exp2)' ,'ABS',..)   returns isfunc=.true.
c        isfunc('ABS()'          ,'ABS',..)   returns isfunc=.true.
c        isfunc('ABS(exp1)*(c+d)','ABS',..)   returns isfunc=.false.
c        isfunc('ABS'            ,'ABS',..)   returns isfunc=.false.
c        isfunc('ABSOLUTELY'     ,'ABS',..)   returns isfunc=.false.
c-----------------------------------------------------------------------

c     parameter declaration
      integer   LENVAR
      parameter (LENVAR=80)

c     argument declaration
      character*(*)  expr,fun,arg1,arg2,arg3

c     variable declaration
      integer        strbeg,strend
      integer        ibeg,iend,fbeg,fend,flen,abeg,aend,alen1,alen2
      logical        err
      character*(LENVAR) hargs,harg1,harg2,harg3
      character*(1)  del

c     initially set result to .false.
      isfunc=.false.

c     check for length of string
      ibeg=strbeg(expr)
      iend=strend(expr)
      fbeg=strbeg(fun)
      fend=strend(fun)
      flen=fend-fbeg+1
      if (iend-ibeg+1.lt.flen+2) return

c     check for presence of function-name
      if (expr(ibeg:ibeg+flen-1).ne.(fun(fbeg:fend))) return

c     check for consistent structure of arguments, alen1 is the length of
c     the arguments including enclosing (),{},[]. alen2 without enclosing
c     brackets.
      alen1=iend-(ibeg+flen)+1
      call bracket(expr(ibeg+flen:iend),abeg,aend,err)
      alen2=aend-abeg+1
      if ((err).or.(alen1.eq.alen2)) return

c     the expression 'expr' is a function involving 'fun'
      isfunc=.true.

c     parse out arguments
      hargs=expr(ibeg+flen+1:iend-1)
      call fnddel(hargs,':,',harg2,harg3,del)
      if (del.eq.'#') then
c       the function has just 1 argument
        arg1=harg2
        arg2='#'
        arg3='#'
      else
c       the function has 2 or 3 arguments
        hargs=harg2
        call fnddel(hargs,':,',harg1,harg2,del)
        if (del.eq.'#') then
c         the function has 2 arguments
          arg1=harg1
          arg2=harg3
          arg3='#'
        else
c         the function has 3 arguments
          arg1=harg1
          arg2=harg2
          arg3=harg3
        endif
      endif

      end




      subroutine fndfun(fld,arg1,arg2,func)
c-----------------------------------------------------------------------
c     this subroutine takes a command-string and checks whether
c     it is a function call. Currently implemented functions  
c          ABS[arg1], SIN[arg1], COS[arg1], LOG[arg1], EXP[arg1], 
c          MAX[arg1,arg2], MIN[arg1,arg2], LOG10[arg1] 
c     are defined. The routine returns with func='ABS', etc, if a 
c     function call is identified, and appropriately sets the
c     arguments arg1,arg2,etc. An empty argument (for a function
c     with one argument only) is flagged with arg2='#' on exit.
c     If no function call is found, the routine returns with func='#'. 
c-----------------------------------------------------------------------
      include 'message.icl'

c     argument declarations
      character*(*)   fld,arg1,arg2,func

c     local variables declarations
      integer         MAX_FIELD_LENGTH,NUMFUNC
      parameter       (NUMFUNC=8,MAX_FIELD_LENGTH=80)
      character*10    funcnam(NUMFUNC),functst
      integer         funcarg(NUMFUNC)
      integer         i,strbeg,strend
      character*(MAX_FIELD_LENGTH) arg3
      logical         isfunc
       
c     funcnam contains the function-name, and funcarg the number of 
c     arguments in the function (1 or 2). The parameter NUMFUN gives 
c     the number of defined functions.
      data  funcnam/'MIN','MAX','ABS','SIN','COS','LOG','EXP','LOG10' /
      data  funcarg/   2  ,  2 ,   1 ,   1 ,   1 ,   1 ,   1 ,  1/     

c     initialize
      func='#'

      do i=1,NUMFUNC
        functst=funcnam(i)
        functst=functst(strbeg(functst):strend(functst))
        if (isfunc(fld,functst,arg1,arg2,arg3)) then
c         the function-name was found, check for consistent number
c         of arguments
          if (((funcarg(i).eq.1).and.((arg1.eq.'#').or.(arg2.ne.'#')))
     &    .or.((funcarg(i).eq.2).and.((arg1.eq.'#').or.(arg2.eq.'#'))))
     &    then
c           found inconsistent number of arguments
            if (message.eq.' ') then
              write (message, 100) fld(strbeg(fld):strend(fld))
            endif
            arg1='#'
            arg2='#'
            func='#'
          else
            func=functst
          endif
          return
        endif
      enddo

c     no function was found
      arg1='#'
      arg2='#'
      func='#'

c     Format statements.
 100  format (1x, 'Inconsistent number of arguments in function ',
     &            'call ',a,'.')
      end



      subroutine fnddel(expr,delis,expr1,expr2,del)
c-----------------------------------------------------------------------
c     Purpose:
c       This subroutines takes a string and cracks it into
c       two strings and a delimiter-string. The possible delimiter-
c       characters are defined through delis which contains all delimiters
c       with ascending binding-power (descending search-order). The 
c       brackets (),[],{} are also implemented, a delimiter which is inside
c       a closed bracket is not selected. The search for delimiters is
c       from right to left. The routine returns with expr1=expr2=expr
c       and del='#' if no delimiter was detected.
c           For parsing out mathematical operations (with recursive code)
c       call the routine with delis='+-*/^'. For parsing out the arguments
c       of an argument string call the routine with delis=',:' (note that
c       the delimiter ',' is currently reserved for IVE, and an alternative
c       deliminter ':' is recommended)
c        
c     Arguments:
c        expr   chrs    input   the expression to be tested
c        delis  chrs    input   the delimiters to be screend for.
c        expr1  chrs    output  left-hand side expression
c        expr2  chrs    output  right-hand side expression
c        del    chrs    output  the detected delimiter
c
c     Examples for delis=',:'
c       expr='arg1:arg2'           expr1='arg1'
c                                  expr2='arg2'
c                                  del=':'
c       expr='arg1,arg2,arg3'      expr1='arg1,arg2'
c                                  expr2='arg3'
c                                  del=','
c     Examples for delis='+-*/^'
c       expr='alpha*beta'          expr1='alpha'
c                                  expr2='beta'
c                                  del='*'
c       expr='test'                expr1='test'
c                                  expr2='test'
c                                  del='#'
c       expr='alpha*beta+gamma'    expr1='alpha*beta'
c                                  expr2='gamma'
c                                  del='+'
c       expr='alpha*(beta+gamma)'  expr1='alpha'
c                                  expr2='(beta+gamma)'
c                                  del='*'
c-----------------------------------------------------------------------

c     argument declaration
      character*(*)  expr,delis,expr1,expr2,del

c     variable declaration
      integer        ibeg1,iend1,ibeg2,iend2

c     call routine chkdel
      call chkdel(expr,ibeg1,iend1,ibeg2,iend2,delis,del)
      if (del.eq.'#') then 
        expr1=expr
        expr2=expr
      else
         if (iend1.ge.ibeg1) then
            expr1=expr(ibeg1:iend1)
         else
c           expr1 has zero length. This is flaged by '#'.
            expr1='#'
         endif
         if (iend2.ge.ibeg2) then
            expr2=expr(ibeg2:iend2)
         else
c           expr2 has zero length. This is flaged by '#'.
            expr2='#'
         endif
      endif
      end


      subroutine chkdel(fld,ibeg1,iend1,ibeg2,iend2,delis,del)
c-----------------------------------------------------------------------
c     This is an internal subroutine called by fnddel.
c-----------------------------------------------------------------------

      character*(*)  fld,delis,del
      character*(1)  bo(3), bc(3)
      integer        cb(3),i,k,l,ibeg1,iend1,ibeg2,iend2,ibeg,iend
      integer        lfound,strbeg,strend

      data           bo / '(','{','['/
      data           bc / ')','}',']'/

      ibeg=strbeg(fld)
      iend=strend(fld)
      del='#'

      ibeg1=ibeg
      iend1=iend
      ibeg2=ibeg
      iend2=iend

c     check for presence of a delimiter
      do k=1,3 
        cb(k)=0
      enddo
      lfound=strend(delis)+1
      do i=iend,ibeg,-1
        do k=1,3
          if (fld(i:i).eq.bo(k)) cb(k)=cb(k)+1
          if (fld(i:i).eq.bc(k)) cb(k)=cb(k)-1
        enddo
        if ((cb(1).eq.0).and.(cb(2).eq.0).and.(cb(3).eq.0)) then
          do l=1,lfound-1 
            if (fld(i:i).eq.delis(l:l)) then
              lfound=l
              del=delis(l:l)
              ibeg1=ibeg
              iend1=i-1
              ibeg2=i+1
              iend2=iend
            endif
          enddo
        endif
      enddo
      end



      ive_ptr function getnumb(chrs,
     &                ndim,dims,stag,datmin,datmax,misdat)
c---------------------------------------------------------------
c     This function takes the string chrs and converts it into a
c     number, if chrs represents a number. Getnumb itself is the
c     ive_ptr to that number. In every respect the number is treated
c     as a field, i.e. dimensions and attributes are defined, and 
c     memory management is done by GETMEM and FREEMEM. This is done
c     in order to simplify the code and allow for the mixed use of
c     scalars and fields.
c---------------------------------------------------------------
c
      integer       MAXDIM
      parameter     (MAXDIM=4)
      include 'message.icl'
c
      character*(*) chrs
      real          r,stag(MAXDIM),datmin(MAXDIM),datmax(MAXDIM),misdat
      ive_ptr       getmem
      integer       ndim
      integer       dims(MAXDIM)
      integer       i,ibeg,iend,cp,ce,cf,strbeg,strend

c     mark ive_ptr as unused
      getnumb=0

c     Check whether string is a number
      ibeg=strbeg(chrs)
      iend=strend(chrs)
      if (ibeg.gt.iend) return
      cp=0
      ce=0
      cf=1
      do i=ibeg,iend
c       check for + and -
        if ((cf.eq.1).and.
     &    ((chrs(i:i).eq.'-').or.(chrs(i:i).eq.'+'))) then
          cf=0
        else
          cf=0        
          if (chrs(i:i).eq.'.') cp=cp+1
c         check for format with exponents
          if ((chrs(i:i).eq.'E').or.(chrs(i:i).eq.'e')) then
            ce=ce+1
            cf=1
            if (cp.le.1) cp=1
          else
            if (((chrs(i:i).lt.'0').or.(chrs(i:i).gt.'9')).and.
     &        (chrs(i:i).ne.'.')) return
          endif
        endif
      enddo
      if ((cp.gt.1).or.(ce.gt.1)) return
       
c     Convert the character-string into a real number.
      read (chrs,*,err=100) r

c     define number
      getnumb=getmem(1)
      if (getnumb.eq.0) then
        if (message.eq.' ') then
          write(message, 110)
        endif
        return
      endif
      call scalass(r,%val(getnumb))
c     define attributes
      ndim=1

      dims(1)=1
      stag(1)=0.
      datmin(1)=0.
      datmax(1)=0.
      misdat=0.
 100  continue
c
c
c     Format statements.
c
 110  format (1x, 'Internal error: Memory overflow in getnumb')
      end



      subroutine scalass(var1,var2)
c--------------------------------------------------------------------
c     This routine does a scalar assign. It is only used to assign 
c     scalar variables which are only available as a ive_ptrs. The 
c     routine can be called as scalass(%val(ptr),r) or as 
c     scalass(r,%val(ptr)). 
c--------------------------------------------------------------------
      real var1,var2
      var2=var1
      end


      ive_ptr function getexpr(op,var1,var2,ndim0,dim0,stag0,dmin0,
     &                         dmax0,misdat0,
     &                         data_units1, data_display_units1,
     &                         dim_names1)
c--------------------------------------------------------------------
c     computes expression
c--------------------------------------------------------------------
c
      include 'message.icl'
c
c
#ifdef sun
      implicit automatic (a-z)
#endif

      integer       MAXDIM, UNIT_LEN
      parameter     (MAXDIM=4,
     &               UNIT_LEN = 80)
      integer       ndim1
      integer       ndim2
      integer       ndim0
      integer	    dim0(MAXDIM),dim1(MAXDIM),dim2(MAXDIM)
      ive_ptr       ptr1, ptr2
      real          stag1(MAXDIM),dmin1(MAXDIM),dmax1(MAXDIM),misdat1
      real          stag2(MAXDIM),dmin2(MAXDIM),dmax2(MAXDIM),misdat2
      real          stag0(MAXDIM),dmin0(MAXDIM),dmax0(MAXDIM),misdat0
      character*(*) op,var1,var2
      character *(UNIT_LEN) data_units1, data_display_units1,
     &                      data_units2, data_display_units2,
     &                      dim_names1(MAXDIM), dim_names2(MAXDIM)
      logical       local1,local2,error,onearg
      integer       k,strbeg,strend,
     &              fvid, getvid
      ive_ptr       getmem,getvar,ptrw1,ptrw2,ptrw3

cc     testoutput
c      write (message,899) var1(1:strend(var1)),var2(1:strend(var2)),
c     &                    op(1:strend(op))
c 899  format (' GETEXPR ',a,' ',a,' ',a)
c      call write_message

c     mark ive_ptr as unused
      getexpr=0
      ptr1=0
      ptr2=0
      ptrw1=0
      ptrw2=0
      ptrw3=0
      local1=.true.
      local2=.true.

      data_units1(1:len(data_units1)) = ' '
      data_display_units1(1:len(data_display_units1)) = ' '
      data_units2(1:len(data_units2)) = ' '
      data_display_units2(1:len(data_display_units2)) = ' '

c     check whether operation has one element
      onearg=(var2.eq.'#')

c     get first term of the expression
      if ((var1.eq.'#').and.((op.eq.'-').or.(op.eq.'+'))) then
c       The first term does not exist. This makes sense only if the 
c       operation is the minus-sign. The lefthandside of the expression
c       is then to be set to a scalar zero.
        ptr1=getmem(1)
        if (ptr1.eq.0) then
          if (message.eq.' ') then
            write(message, 100) 1
          endif
          goto 900
        endif
        local1=.true.
        ndim1=1
        do k=1,MAXDIM
          dim1(k)=1
        enddo
        dmin1(1)=0.
        dmax1(1)=0.
        misdat1=0.
        call scalass(0.,%val(ptr1))
      else if (var1.ne.'#') then
c       The first term is computed with a recursive call to getvar.
        ptr1=getvar(var1, ndim1, dim1, stag1, dmin1, dmax1, misdat1,
     &              data_units1, data_display_units1, dim_names1,
     &              local1)
        if (ptr1.eq.0) goto 900
        if (ndim1.lt.MAXDIM) then
          do k=ndim1+1,MAXDIM
            dim1(k)=1
          enddo
        endif
      endif


c     get second term of the expression
      if (var2.ne.'#') then
c       The second term is computed with a recursive call to getvar.
        ptr2=getvar(var2, ndim2, dim2, stag2, dmin2, dmax2, misdat2, 
     &              data_units2, data_display_units2, dim_names2,
     &              local2)
        if (ptr2.eq.0) goto 900
        if (ndim2.lt.MAXDIM) then
          do k=ndim2+1,MAXDIM
            dim2(k)=1
          enddo
        endif
      endif


c     check for error
      if ( (ptr1.eq.0) .or. ((ptr2.eq.0).and.(.not.onearg)) ) goto 900


      if (onearg) then
c       ----------------------------------------------------------------
c       evaluate operation with one argument
c       ----------------------------------------------------------------
c       assign attributes and dimensions
        ndim0=ndim1
        do k=1,ndim0
          dim0(k)=dim1(k)
          stag0(k)=stag1(k)
          dmin0(k)=dmin1(k)
          dmax0(k)=dmax1(k)
        enddo
        if (ndim0.lt.MAXDIM) then
          do k=ndim0+1,MAXDIM
            dim0(k)=1
          enddo
        endif

c       get memory
        getexpr=getmem( dim0(1)*dim0(2)*dim0(3)*dim0(4) )
        if (getexpr.eq.0) then
          if (message.eq.' ') then
            write(message, 100) dim0(1)*dim0(2)*dim0(3)*dim0(4)
          endif
          goto 900
        endif

        call cmpexp1(%val(ptr1),misdat1,
     &    %val(getexpr),ndim0,dim0(1),dim0(2),dim0(3),dim0(4),misdat0,  
     &    op )


      else if (op.eq.'@') then
c       ----------------------------------------------------------------
c       evaluate interpolation-operation
c       ----------------------------------------------------------------
c       assign attributes and check whether fields are compatible
        if (ndim1.ne.ndim2) then
          if (message.eq.' ') then
           write(message, 110) var1(1:strend(var1)),var2(1:strend(var2))
          endif
          goto 900
        endif
        ndim0=ndim2
        do k=1,ndim0
          dim0(k)=dim2(k)
          stag0(k)=stag2(k)
          dmin0(k)=dmin2(k)
          dmax0(k)=dmax2(k)
	  dim_names1(k) = dim_names2(k)
        enddo
        if (ndim0.lt.MAXDIM) then
          do k=ndim0+1,MAXDIM
            dim0(k)=1
          enddo
        endif

c       get memory
        getexpr=getmem( dim0(1)*dim0(2)*dim0(3)*dim0(4) )
        ptrw1=getmem( dim0(1)*dim0(2)*ndim0 )
        ptrw2=getmem( dim0(1)*dim0(2)*ndim0 )
        ptrw3=getmem( dim0(1)*dim0(2)*ndim0 )
        if ((getexpr.eq.0).or.
     &    (ptrw1.eq.0).or.(ptrw2.eq.0).or.(ptrw3.eq.0)) then
          if (message.eq.' ') then
            write(message, 100) dim0(1)*dim0(2)*dim0(3)*dim0(4)
          endif
          goto 900
        endif


        call newstag(%val(ptrw1),%val(ptrw2),%val(ptrw3),
     &    %val(ptr1),   ndim1,dim1(1),dim1(2),dim1(3),dim1(4),
     &                                      dmin1,dmax1,stag1,misdat1,
     &    %val(getexpr),ndim0,dim0(1),dim0(2),dim0(3),dim0(4),
     &                                      dmin0,dmax0,stag0,misdat0,
     &    var1(strbeg(var1):strend(var1)),
     &	  var2(strbeg(var2):strend(var2)),
     &    error )
        if (error) then
          call freemem(getexpr)
          goto 900
        endif
      else
c       ----------------------------------------------------------------
c       evaluate operation with two arguments
c       ----------------------------------------------------------------
c       assign attributes and check whether fields are compatible
        ndim0=max0(ndim1,ndim2)
        error=.false.
        do k=1,ndim0
          if ( (ndim1.ge.k).and.(ndim2.ge.k) ) then
            dim0(k)=max0(dim1(k),dim2(k))
            if (dim1(k).gt.1) then
              stag0(k)=stag1(k)
              dmin0(k)=dmin1(k)
              dmax0(k)=dmax1(k)
            else 
              stag0(k)=stag2(k)
              dmin0(k)=dmin2(k)
              dmax0(k)=dmax2(k)
	      dim_names1(k) = dim_names2(k)
            endif
            if ((dim1(k).gt.1).and.(dim2(k).gt.1)) then 
c              error=error.or.(abs(stag1(k)-stag2(k)).gt.1.e-5)
c              error=error.or.(abs(dmin1(k)-dmin2(k)).gt.1.e-5)
c              error=error.or.(abs(dmax1(k)-dmax2(k)).gt.1.e-5)
              error=error.or.(abs(stag1(k)-stag2(k)).gt.1.e-2)
              error=error.or.
     &        (abs((dmin1(k)-dmin2(k))/(dmax2(k)-dmin2(k))).gt.1.e-3)
              error=error.or.
     &        (abs((dmax1(k)-dmax2(k))/(dmax2(k)-dmin2(k))).gt.1.e-3)
            endif
          else if (ndim1.ge.k) then
            dim0(k) =dim1(k)
            stag0(k)=stag1(k)
            dmin0(k)=dmin1(k)
            dmax0(k)=dmax1(k)
          else
            dim0(k) =dim2(k)
            stag0(k)=stag2(k)
            dmin0(k)=dmin2(k)
            dmax0(k)=dmax2(k)
	    dim_names1(k) = dim_names2(k)
          endif
        enddo
        if (error) then
          if (message.eq.' ') then
          write (message, 120) 
     &          var1(1:strend(var1)),var2(1:strend(var2))
          endif
          goto 900
        endif

c       assign dimensions
        if (ndim0.lt.MAXDIM) then
          do k=ndim0+1,MAXDIM
            dim0(k)=1
	    dim_names1(k) = ' '
          enddo
        endif

c       get memory
        getexpr=getmem( dim0(1)*dim0(2)*dim0(3)*dim0(4) )
        if (getexpr.eq.0) then
!     Try again to see if we are on th verger and some came available
           getexpr=getmem( dim0(1)*dim0(2)*dim0(3)*dim0(4) )
           if (getexpr.eq.0) then
              if (message.eq.' ') then
                 write(message, 100) dim0(1)*dim0(2)*dim0(3)*dim0(4)
              endif
              goto 900
           endif
        endif
c       call routine to perform the operation
        call cmpexp2(
     &    %val(ptr1)   ,ndim1,dim1(1),dim1(2),dim1(3),dim1(4),misdat1,
     &    %val(ptr2)   ,ndim2,dim2(1),dim2(2),dim2(3),dim2(4),misdat2,
     &    %val(getexpr),ndim0,dim0(1),dim0(2),dim0(3),dim0(4),misdat0, 
     &    op)
      endif

c     free temporary memory and exit routine
 900  continue
      if (ptrw1.ne.0) call freemem(ptrw1)
      if (ptrw2.ne.0) call freemem(ptrw2)
      if (ptrw3.ne.0) call freemem(ptrw3)
      if ((local1).and.(ptr1.ne.0)) then
         fvid = getvid(var1(strbeg(var1):strend(var1))//char(0))
         if (fvid.ge.0) call free_var(fvid)
      endif	
      if ((local2).and.(ptr2.ne.0)) then
         fvid = getvid(var2(strbeg(var2):strend(var2))//char(0))
         if (fvid.ge.0) call free_var(fvid)
      endif
c
c
c     Format statements.
c
 100  format (1x, 'Internal error: Memory overflow in GETEXPR.',
     &            ' Size was ', i5)
 110  format (1x, 'The terms in ',a,'@',a,'have a different number ',
     &            'of dimensions.')
 120  format (1x, 'The terms ',a,' and ',a,' have incompatible ',
     &            'attributes.')
c
      end



      subroutine newstag(comp0,comp1,phypt,
     &       ar1,ndim1,nx1,ny1,nz1,nt1,dmi1,dma1,sta1,misdat1,
     &       ar0,ndim0,nx0,ny0,nz0,nt0,dmi0,dma0,sta0,misdat0,
     &	     name1,name0,error)
c--------------------------------------------------------------------
c     This subroutine takes the field '1' and interpolates it onto a
c     new grid given by the attributes of field '0'. Output is ar0,
c     misdat0 and error. All other parameters are input. The variables
c     comp0,comp1,phypt are work-arrays, they must have at least the
c     length of ndim0*nx0
c--------------------------------------------------------------------

      include 'units.icl'
      include 'message.icl'

      integer       MAXDIM
      parameter     (MAXDIM=4)
      integer       ndim1,nx1,ny1,nz1,nt1
      integer       ndim0,nx0,ny0,nz0,nt0
      real          ar1(nx1,ny1,nz1,nt1),dmi1(MAXDIM),dma1(MAXDIM),
     &              sta1(MAXDIM),misdat1
      real          ar0(nx0,ny0,nz0,nt0),dmi0(MAXDIM),dma0(MAXDIM),
     &              sta0(MAXDIM),misdat0
      real          comp0(ndim0,nx0,ny0),comp1(ndim0,nx0,ny0),
     &		    phypt(ndim0,nx0,ny0)
      double precision extrap, interp
      character*(*) name1, name0     
      character*(UNIT_LEN)       dim_names(MAXDIM)
      integer       dims(MAXDIM)
      real	    stag(MAXDIM), datmin(MAXDIM), datmax(MAXDIM), misdat
      integer       ndims
      logical       error
      ive_ptr       getvar

c     local variables
      integer       iflagc(MAXDIM),iflagp(MAXDIM)
      integer       dim1(MAXDIM)
      integer       i,j,k,l
      logical       missing
      real          mmin,mmax
      ive_ptr       junk

c     define dim1
      dim1(1)=nx1
      dim1(2)=ny1
      dim1(3)=nz1
      dim1(4)=nt1

c     initialize output missing dat vaalue
      missing=.false.
      if (misdat1.ne.0.) then
        misdat0=misdat1
      else
        misdat0=-999.99
      endif

c     fill the arrays iflagc and iflagp
      do i=1,ndim0
         iflagp(i)=1
         if (dim1(i).eq.1) then
            iflagc(i) = 0
         else
            iflagc(i)=1
         endif
      enddo
      do i=ndim0+1,MAXDIM
        iflagc(i)=0
        iflagp(i)=0
      enddo
      error=.false.
      ar0=0.
      do l=1,nt0
         do k=1,nz0
            do j=1, ny0
               do i=1,nx0
                  comp0(4,i,j)=real(l)
                  comp0(3,i,j)=real(k)
                  comp0(2,i,j)=real(j)
                  comp0(1,i,j)=real(i)
!     write(6,*)'comp0',i,comp0(:,i)
               enddo
            enddo
c     compute the computational positions in ar1 of the points
c     ar0(i=1,nx0;j;k;l). In order to save function-calls, the
c     code is arranged such that every call to the transforms 
c     returns the transformation for nx0*ny0 points.
               
c     call user routine to change fields.
            junk = getvar(name0, ndims, dims, stag,
     &           datmin, datmax, misdat, data_units, data_display_units,
     &           dim_names, error)
c     
            dmi0=datmin
            dma0=datmax
            if(junk .eq. 0) then
               error = .true.
               return
            endif
c     get the position of the point (i,j,k,l) of ar0 in 
c     physical space
c     
            phypt=0.
!     call index_2_phys_trans(phypt,comp0,iflagp,ndim0,nx0*ny0)
            call setrvar('spval', misdat0, error)
            call index_2_phys_trans(phypt,comp0,iflagp,ndim0,nx0*ny0)
            
c     call user routine to change fields.
            
            junk = getvar(name1, ndims, dims, stag,
     &           datmin, datmax, misdat, data_units, data_display_units,
     &           dim_names, error)
            
            if(junk .eq. 0) then
               error = .true.
               return
            endif
c     
c     get the position of the point in computational space of ar1
c     
!     call phys_2_index_trans(phypt,comp1,iflagp,ndim1,nx0*ny0)
            call setrvar('spval', misdat0, error)
            call phys_2_index_trans(phypt,comp1,iflagp,ndim1,nx0*ny0)
            
c     start interpolation/extrapolation
            
            do j=1, ny0
               do i=1,nx0
                  ar0(i,j,k,l) = extrap(ar1,nx1,ny1,nz1,nt1,
     &                 comp1(1,i,j),comp1(2,i,j),
     &                 comp1(3,i,j),
     &                 comp1(4,i,j),misdat0)
                  if (ar0(i,j,k,l).eq.misdat0) then
                     missing = .true.
!                     print *,i,j,k,'missing',ar1(i,j,k,l),comp1(:,i,j)
!                  else
!                     print *,i,j,k,ar0(i,j,k,l)
                  endif
               enddo            ! end of i-loop
            enddo               ! end of j-loop
         enddo                  ! end of k-loop
      enddo                     !end of l-loop
c     redefine missing data flag, if no data is missing
      if ((.not.missing).and.(misdat1.eq.0.)) misdat0=0.
c     exit routine newstag
      end

      subroutine oldstag(comp0,comp1,phypt,
     &       ar1,ndim1,nx1,ny1,nz1,nt1,dmi1,dma1,sta1,misdat1,
     &       ar0,ndim0,nx0,ny0,nz0,nt0,dmi0,dma0,sta0,misdat0,
     &	     name1,name0,error)
c--------------------------------------------------------------------
c     This subroutine takes the field '1' and interpolates it onto a
c     new grid given by the attributes of field '0'. Output is ar0,
c     misdat0 and error. All other parameters are input. The variables
c     comp0,comp1,phypt are work-arrays, they must have at least the
c     length of ndim0*nx0
c--------------------------------------------------------------------

      include 'units.icl'
      include 'message.icl'

      integer       MAXDIM
      parameter     (MAXDIM=4)
      integer       ndim1,nx1,ny1,nz1,nt1
      integer       ndim0,nx0,ny0,nz0,nt0
      real          ar1(nx1,ny1,nz1,nt1),dmi1(MAXDIM),dma1(MAXDIM),
     &              sta1(MAXDIM),misdat1
      real          ar0(nx0,ny0,nz0,nt0),dmi0(MAXDIM),dma0(MAXDIM),
     &              sta0(MAXDIM),misdat0
      real          comp0(ndim0,nx0,ny0),comp1(ndim0,nx0,ny0),
     &		    phypt(ndim0,nx0,ny0)
      double precision extrap
      character*(*) name1, name0     
      character*(UNIT_LEN)       dim_names(MAXDIM)
      integer       dims(MAXDIM)
      real	    stag(MAXDIM), datmin(MAXDIM), datmax(MAXDIM), misdat
      integer       ndims
      logical       error

c     local variables
      integer       iflagc(MAXDIM),iflagp(MAXDIM)
      integer       dim1(MAXDIM)
      integer       i,j,k,l
      logical       missing

c     define dim1
      dim1(1)=nx1
      dim1(2)=ny1
      dim1(3)=nz1
      dim1(4)=nt1

c     initialize output missing dat vaalue
      missing=.false.
      if (misdat1.ne.0.) then
        misdat0=misdat1
      else
        misdat0=-999.99
      endif

c     fill the arrays iflagc and iflagp
      do i=1,ndim0
         iflagp(i)=1
         if (dim1(i).eq.1) then
            iflagc(i) = 0
         else
            iflagc(i)=1
         endif
      enddo
      do i=ndim0+1,MAXDIM
        iflagc(i)=0
        iflagp(i)=0
      enddo

      error=.false.

      do l=1,nt0
        if (ndim0 .gt. 3) then
	   do j=1, ny0
           do i=1,nx0
              comp0(4,i,j)=real(l)
           enddo
	   enddo
        endif
        do k=1,nz0
          if (ndim0 .gt. 2) then
       	     do j=1,ny0
             do i=1,nx0
                comp0(3,i,j)=real(k)
             enddo
             enddo
          endif
          do j=1,ny0
	      do i = 1, nx0
              comp0(2,i,j)=real(j)
              comp0(1,i,j)=real(i)
	  enddo
	  enddo

c           compute the computational positions in ar1 of the points
c           ar0(i=1,nx0;j;k;l). In order to save function-calls, the
c           code is arranged such that every call to the transforms 
c           returns the transformation for nx0*ny0 points.

c           call user routine to change fields.
	    call getvar(name0, ndims, dims, stag,
     &	       datmin, datmax, misdat, data_units, data_display_units,
     &	       dim_names, error)
c
c           get the position of the point (i,j,k,l) of ar0 in 
c           physical space
c
            call index_2_phys_trans(phypt,comp0,iflagp,ndim0,nx0*ny0)

c           call user routine to change fields.

	    call getvar(name1, ndims, dims, stag,
     &	       datmin, datmax, misdat, data_units, data_display_units,
     &	       dim_names, error)
c
c           get the position of the point in computational space of ar1
c
            call phys_2_index_trans(phypt,comp1,iflagp,ndim1,nx0*ny0)

c           start interpolation/extrapolation
            
            do j=1, ny0
            do i=1,nx0
               ar0(i,j,k,l) = extrap(ar1,nx1,ny1,nz1,nt1,comp1(1,i,j),
     &              comp1(2,i,j), comp1(3,i,j), comp1(4,i,j), misdat0)
               if (ar0(i,j,k,l).eq.misdat0) missing = .true.
            enddo    ! end of i-loop
          enddo    ! end of j-loop
        enddo    ! end of k-loop
      enddo    ! end of l-loop

c     redefine missing data flag, if no data is missing
      if ((.not.missing).and.(misdat1.eq.0.)) misdat0=0.

c     exit routine newstag
      end


      subroutine cmpexp1(ar1,                      misdat1,
     &                   ar0,ndim0,nx0,ny0,nz0,nt0,misdat0,
     &                   op)
c--------------------------------------------------------------------
c     Purpose:
c         This subroutine computes the one-arguments function 
c         AR0=OP[AR1], where OP is one of the functions ABS, SIN, COS
c         LOG or EXP.
c     Arguments:
c         ar1,    misdat1  input     the field used in the operation
c         ar0,...,misdat0  output    the field ar1, after the operation
c                                    has been performed. It has the same
c                                    attributes as ar1, except for misdat.
c         op               input     the function to be evaluated
c--------------------------------------------------------------------
      integer       ndim0,nx0,ny0,nz0,nt0
      real          ar1(nx0,ny0,nz0,nt0),misdat1
      real          ar0(nx0,ny0,nz0,nt0),misdat0
      character*(*) op

c     local declarations
      integer   i0,j0,k0,l0
      real      maxreal,argmax
      logical   missing

      data maxreal /1.e20/

      if (misdat1.eq.0.) then

c       field ar1 is defined everywhere

        if ((op.ne.'LOG').or.(op.ne.'EXP') .or. (op .ne. 'LOG10')) then
c         the function is possibly not always defined
          misdat0=-999.99
        else
c         the function is always defined
          misdat0=0.
        endif
        if (op.eq.'EXP') then
c         do the computation only for arguments smaller than argmax
          argmax=log(maxreal)
        endif

        missing=.false.

        do l0=1,nt0
         do k0=1,nz0
          do j0=1,ny0
           do i0=1,nx0
            if (op.eq.'ABS') then
              ar0(i0,j0,k0,l0)=abs(ar1(i0,j0,k0,l0))
            else if (op.eq.'SIN') then
              ar0(i0,j0,k0,l0)=sin(ar1(i0,j0,k0,l0))
            else if (op.eq.'COS') then
              ar0(i0,j0,k0,l0)=cos(ar1(i0,j0,k0,l0))
            else if (op.eq.'LOG') then
              if (ar1(i0,j0,k0,l0).gt.0.) then
                ar0(i0,j0,k0,l0)=log(ar1(i0,j0,k0,l0))
              else
                ar0(i0,j0,k0,l0)=misdat0
                missing=.true.
              endif
            else if (op.eq.'LOG10') then
              if (ar1(i0,j0,k0,l0).gt.0.) then
                ar0(i0,j0,k0,l0)=log10(ar1(i0,j0,k0,l0))
              else
                ar0(i0,j0,k0,l0)=misdat0
                missing=.true.
              endif
            else if (op.eq.'EXP') then
              if (ar1(i0,j0,k0,l0).lt.argmax) then
                ar0(i0,j0,k0,l0)=exp(ar1(i0,j0,k0,l0))
              else
                ar0(i0,j0,k0,l0)=misdat0
                missing=.true.
              endif
            endif
           enddo
          enddo
         enddo
        enddo

        if (.not.missing) misdat0=0.

      else
        if (op.eq.'EXP') then
c       do the computation only for arguments smaller than argmax
           argmax=log(maxreal)
        endif
c       field ar1 has missing data
        misdat0=-999.99
        do l0=1,nt0
         do k0=1,nz0
          do j0=1,ny0
           do i0=1,nx0
            if (ar1(i0,j0,k0,l0).ne.misdat1) then
c             do operation
              if (op.eq.'ABS') then
                ar0(i0,j0,k0,l0)=abs(ar1(i0,j0,k0,l0))
              else if (op.eq.'SIN') then
                ar0(i0,j0,k0,l0)=sin(ar1(i0,j0,k0,l0))
              else if (op.eq.'COS') then
                ar0(i0,j0,k0,l0)=cos(ar1(i0,j0,k0,l0))
              else if (op.eq.'LOG') then
                if (ar1(i0,j0,k0,l0).gt.0.) then
                  ar0(i0,j0,k0,l0)=log(ar1(i0,j0,k0,l0))
                else
                  ar0(i0,j0,k0,l0)=misdat0
                endif
              else if (op.eq.'LOG10') then
                if (ar1(i0,j0,k0,l0).gt.0.) then
                  ar0(i0,j0,k0,l0)=log10(ar1(i0,j0,k0,l0))
                else
                  ar0(i0,j0,k0,l0)=misdat0
                endif
              else if (op.eq.'EXP') then
                if (ar1(i0,j0,k0,l0).lt.argmax) then
                  ar0(i0,j0,k0,l0)=exp(ar1(i0,j0,k0,l0))
                else
                  ar0(i0,j0,k0,l0)=misdat0
                endif
              endif
            else
              ar0(i0,j0,k0,l0)=misdat0
            endif
           enddo
          enddo
         enddo
        enddo
      endif
      end



      subroutine cmpexp2(ar1,ndim1,nx1,ny1,nz1,nt1,misdat1,
     &                   ar2,ndim2,nx2,ny2,nz2,nt2,misdat2,
     &                   ar0,ndim0,nx0,ny0,nz0,nt0,misdat0,
     &                   oper)
c--------------------------------------------------------------------
c     Purpose:
c         This subroutine computes the operations AR0=AR1 OPER AR2
c         where OPER is one of the operations +, -, *, /, ^; 
c         and two-argument functions AR0=OPER[AR1,AR2], where OPER is
c         one of the functions MAX or MIN.
c     Arguments
c         ar1,...,misdat1  input     term1 of the operation
c         ar2,...,misdat2  input     term2 of the operation
c         ar0,...,misdat0            result of the operation, where
c                                    ar0 and misdat0 are output, while
c                                    other arguments are input
c         op               input     the operation to be evaluated
c--------------------------------------------------------------------
      integer       ndim1,nx1,ny1,nz1,nt1
      integer       ndim2,nx2,ny2,nz2,nt2
      integer       ndim0,nx0,ny0,nz0,nt0
      real          ar1(nx1,ny1,nz1,nt1),misdat1
      real          ar2(nx2,ny2,nz2,nt2),misdat2
      real          ar0(nx0,ny0,nz0,nt0),misdat0
      character*(*) oper
      character*10  op

c     local declarations
      integer i1,j1,k1,l1
      integer i2,j2,k2,l2
      integer i0,j0,k0,l0
      logical missing

c     Check whether integer-function can be used for power-operation.
c     In the operation a**b, the fortran-compiler takes log(a) 
c     whenever b is real (even if b=2.), such that the operation is 
c     only defined for a>0. To avoid this restriction, the operation 
c     a**i with i=integer is called, whenever ar2 is a scalar and close
c     to an integer.  
      op=oper
      if ((oper.eq.'^').and.(ndim2.eq.1).and.(nx2.eq.1)) then
        if (abs(ar2(1,1,1,1)-real(nint(ar2(1,1,1,1)))).lt.1.e-5) 
     &          op='^I'
      endif

      if ((misdat1.ne.0.).and.(misdat2.ne.0.)) then

c       both of the arrays have missing data
        misdat0=misdat1

        do l0=1,nt0
        l1=Min(nt1,l0)
        l2=Min(nt2,l0)
        do k0=1,nz0
        k1=Min(nz1,k0)
        k2=Min(nz2,k0)
        do j0=1,ny0
        j1=Min(ny1,j0)
        j2=Min(ny2,j0)
        do i0=1,nx0
          i1=Min(nx1,i0)
          i2=Min(nx2,i0)
          if ((ar1(i1,j1,k1,l1).ne.misdat1).and.
     &      (ar2(i2,j2,k2,l2).ne.misdat2)) then
c           do operation
            if (op.eq.'+') then
              ar0(i0,j0,k0,l0)=ar1(i1,j1,k1,l1)+ar2(i2,j2,k2,l2)
            else if (op.eq.'-') then
              ar0(i0,j0,k0,l0)=ar1(i1,j1,k1,l1)-ar2(i2,j2,k2,l2)
            else if (op.eq.'*') then
              ar0(i0,j0,k0,l0)=ar1(i1,j1,k1,l1)*ar2(i2,j2,k2,l2)
            else if (op.eq.'/') then
             if (ar2(i2,j2,k2,l2).ne.0.) then
              ar0(i0,j0,k0,l0)=ar1(i1,j1,k1,l1)/ar2(i2,j2,k2,l2)
             else
              ar0(i0,j0,k0,l0)=misdat0
             endif
            else if (op.eq.'^') then
             if (ar1(i1,j1,k1,l1).ge.0.) then
              ar0(i0,j0,k0,l0)=ar1(i1,j1,k1,l1)**ar2(i2,j2,k2,l2)
             else
              ar0(i0,j0,k0,l0)=misdat0
             endif
            else if (op.eq.'^I') then
              ar0(i0,j0,k0,l0)=ar1(i1,j1,k1,l1)**nint(ar2(i2,j2,k2,l2))
            else if (op.eq.'MAX') then
              ar0(i0,j0,k0,l0)=amax1(ar1(i1,j1,k1,l1),ar2(i2,j2,k2,l2))
            else if (op.eq.'MIN') then
              ar0(i0,j0,k0,l0)=amin1(ar1(i1,j1,k1,l1),ar2(i2,j2,k2,l2))
            endif
          else
            ar0(i0,j0,k0,l0)=misdat0
          endif
         enddo
        enddo
        enddo
        enddo

      else if (misdat1.ne.0.) then

c       only array 1 has missing data
        misdat0=misdat1

        do l0=1,nt0
        l1=Min(nt1,l0)
        l2=Min(nt2,l0)
        do k0=1,nz0
        k1=Min(nz1,k0)
        k2=Min(nz2,k0)
        do j0=1,ny0
        j1=Min(ny1,j0)
        j2=Min(ny2,j0)
        do i0=1,nx0
          i1=Min(nx1,i0)
          i2=Min(nx2,i0)
          if (ar1(i1,j1,k1,l1).ne.misdat1) then
c           do operation
            if (op.eq.'+') then
              ar0(i0,j0,k0,l0)=ar1(i1,j1,k1,l1)+ar2(i2,j2,k2,l2)
            else if (op.eq.'-') then
              ar0(i0,j0,k0,l0)=ar1(i1,j1,k1,l1)-ar2(i2,j2,k2,l2)
            else if (op.eq.'*') then
              ar0(i0,j0,k0,l0)=ar1(i1,j1,k1,l1)*ar2(i2,j2,k2,l2)
            else if (op.eq.'/') then
             if (ar2(i2,j2,k2,l2).ne.0.) then
              ar0(i0,j0,k0,l0)=ar1(i1,j1,k1,l1)/ar2(i2,j2,k2,l2)
             else
              ar0(i0,j0,k0,l0)=misdat0
             endif
            else if (op.eq.'^') then
             if (ar1(i1,j1,k1,l1).ge.0.) then
              ar0(i0,j0,k0,l0)=ar1(i1,j1,k1,l1)**ar2(i2,j2,k2,l2)
             else
              ar0(i0,j0,k0,l0)=misdat0
             endif
            else if (op.eq.'^I') then
              ar0(i0,j0,k0,l0)=ar1(i1,j1,k1,l1)**nint(ar2(i2,j2,k2,l2))
            else if (op.eq.'MAX') then
              ar0(i0,j0,k0,l0)=amax1(ar1(i1,j1,k1,l1),ar2(i2,j2,k2,l2))
            else if (op.eq.'MIN') then
              ar0(i0,j0,k0,l0)=amin1(ar1(i1,j1,k1,l1),ar2(i2,j2,k2,l2))
            endif
          else
            ar0(i0,j0,k0,l0)=misdat0
          endif
        enddo
        enddo
        enddo
        enddo

      else if (misdat2.ne.0.) then

c       only array 2 has missing data
        misdat0=misdat2

        do l0=1,nt0
        l1=Min(nt1,l0)
        l2=Min(nt2,l0)
        do k0=1,nz0
        k1=Min(nz1,k0)
        k2=Min(nz2,k0)
        do j0=1,ny0
        j1=Min(ny1,j0)
        j2=Min(ny2,j0)
        do i0=1,nx0
          i1=Min(nx1,i0)
          i2=Min(nx2,i0)
          if (ar2(i2,j2,k2,l2).ne.misdat2) then
c           do operation
            if (op.eq.'+') then
              ar0(i0,j0,k0,l0)=ar1(i1,j1,k1,l1)+ar2(i2,j2,k2,l2)
            else if (op.eq.'-') then
              ar0(i0,j0,k0,l0)=ar1(i1,j1,k1,l1)-ar2(i2,j2,k2,l2)
            else if (op.eq.'*') then
              ar0(i0,j0,k0,l0)=ar1(i1,j1,k1,l1)*ar2(i2,j2,k2,l2)
            else if (op.eq.'/') then
             if (ar2(i2,j2,k2,l2).ne.0.) then
              ar0(i0,j0,k0,l0)=ar1(i1,j1,k1,l1)/ar2(i2,j2,k2,l2)
             else
              ar0(i0,j0,k0,l0)=misdat0
             endif
            else if (op.eq.'^') then
             if (ar1(i1,j1,k1,l1).ge.0.) then
              ar0(i0,j0,k0,l0)=ar1(i1,j1,k1,l1)**ar2(i2,j2,k2,l2)
             else
              ar0(i0,j0,k0,l0)=misdat0
             endif
            else if (op.eq.'^I') then
              ar0(i0,j0,k0,l0)=ar1(i1,j1,k1,l1)**nint(ar2(i2,j2,k2,l2))
            else if (op.eq.'MAX') then
              ar0(i0,j0,k0,l0)=amax1(ar1(i1,j1,k1,l1),ar2(i2,j2,k2,l2))
            else if (op.eq.'MIN') then
              ar0(i0,j0,k0,l0)=amin1(ar1(i1,j1,k1,l1),ar2(i2,j2,k2,l2))
            endif
          else
            ar0(i0,j0,k0,l0)=misdat0
          endif
        enddo
        enddo
        enddo
        enddo

      else

c       no missing data in ar1 and ar2 (but ar0 may have)
        misdat0=-999.99
        missing=.false.

        do l0=1,nt0
        l1=Min(nt1,l0)
        l2=Min(nt2,l0)
        do k0=1,nz0
        k1=Min(nz1,k0)
        k2=Min(nz2,k0)
        do j0=1,ny0
        j1=Min(ny1,j0)
        j2=Min(ny2,j0)
        do i0=1,nx0
          i1=Min(nx1,i0)
          i2=Min(nx2,i0)

          if (op.eq.'+') then
            ar0(i0,j0,k0,l0)=ar1(i1,j1,k1,l1)+ar2(i2,j2,k2,l2)
          else if (op.eq.'-') then
            ar0(i0,j0,k0,l0)=ar1(i1,j1,k1,l1)-ar2(i2,j2,k2,l2)
          else if (op.eq.'*') then
            ar0(i0,j0,k0,l0)=ar1(i1,j1,k1,l1)*ar2(i2,j2,k2,l2)
          else if (op.eq.'/') then
           if (ar2(i2,j2,k2,l2).ne.0.) then
            ar0(i0,j0,k0,l0)=ar1(i1,j1,k1,l1)/ar2(i2,j2,k2,l2)
           else
            ar0(i0,j0,k0,l0)=misdat0
            missing=.true.
           endif
          else if (op.eq.'^') then
           if (ar1(i1,j1,k1,l1).ge.0.) then
            ar0(i0,j0,k0,l0)=ar1(i1,j1,k1,l1)**ar2(i2,j2,k2,l2)
           else
            ar0(i0,j0,k0,l0)=misdat0
            missing=.true.
           endif
          else if (op.eq.'^I') then
            ar0(i0,j0,k0,l0)=ar1(i1,j1,k1,l1)**nint(ar2(i2,j2,k2,l2))
          else if (op.eq.'MAX') then
            ar0(i0,j0,k0,l0)=amax1(ar1(i1,j1,k1,l1),ar2(i2,j2,k2,l2))
          else if (op.eq.'MIN') then
            ar0(i0,j0,k0,l0)=amin1(ar1(i1,j1,k1,l1),ar2(i2,j2,k2,l2))
          endif

        enddo
        enddo
        enddo
        enddo
        if (.not.missing) misdat0=0.
      endif
      end

