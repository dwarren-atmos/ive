/*
 * $Id: Window_Dump.c,v 1.10 2003/07/02 20:23:02 warren Exp $
 * This software was originally taken from the xwd client in the standard
 * X11 distribution.  It has been turned into a subroutine.
 * Currently produces only ZPixmap or gif format.
 * 
 * Calling sequence:
 *
 *   Window_Dump(dpy, window, pixmap, bell, type, out, buf)
 *
 *   Display *dpy  - Display of window to dump
 *   Window window - Window id of window to dump
 *   Pixmap pixmap - Pixmap to dump if different from contents of window
 *   Bool bell     - Ring bell at start and end of dump
 *   char *type    - XWD or GIF
 *   FILE *out     - File opened via fopen to dump window to.
 *   char *buf     - Array for error messages.
 *
 *   Author:      Harry Edmon, University of Washington
 *
 * $Log: Window_Dump.c,v $
 * Revision 1.10  2003/07/02 20:23:02  warren
 * Fixes for slanted window dumps on alphas with 8 bit screens, call info_2d
 * with correct mins and maxs when returning to slicer widget from points.
 *
 * Revision 1.9  2003/03/27 21:40:40  davidive
 * Fixed gif output to work from pixmap and not crash o 2 colors
 *
 * Revision 1.8  2002/12/26 22:48:10  warren
 * New gif encoder (faster and easier to use), also fixed gif encoding
 * to work from truecolor screens.
 *
 * Revision 1.7  2001/08/15 22:23:19  davidive
 * Cleaned up code to reduce warnings about type mismatches, and added buttons
 * and callbacks to the 1d form widget to select which access to label against
 * for diagonal plots.
 *
 * Revision 1.6  1996/10/23 15:48:30  harry
 * Fixes for picky SGI compiler.
 *
 * Revision 1.5  1996/01/18 18:21:22  harry
 * Fix for non-iconified windows.
 *
 * Revision 1.4  1996/01/10 17:08:52  harry
 * Fix dumping routines to use proper colormap.
 *
 * Revision 1.3  1995/12/04  21:44:15  harry
 * Properly dump windows that are partially off the screen.
 *
 * Revision 1.2  1994/06/16  20:17:26  warren
 * update from 3.1
 *
 * Revision 1.1.1.1  1993/04/07  19:41:24  harry
 * Initial CVS checkin of IVE.
 *
 * Revision 1.9  1993/02/17  22:03:34  warren
 * added code for memory debugging and cleaned up freeing bugs
 *
 * Revision 1.8  1993/02/16  20:42:50  warren
 * added #ifndef MEMDBG before char *calloc();
 *
 * Revision 1.7  1993/02/16  20:39:50  warren
 * moved MEMDBG includes lower so prerequisits are first
 *
 * Revision 1.6  1993/02/13  00:56:07  warren
 * added ifdef to use dbmalloc header and library
 *
 * Revision 1.5  1993/02/03  20:59:09  harry
 * Do not call XFree(win_name) for a GIF dump file.
 *
 * Revision 1.4  1992/12/22  18:47:05  harry
 * Change strchr calls to strcmp.
 *
 * Revision 1.3  1992/12/14  18:25:45  harry
 * Change "type" argument from int to char.
 *
 * Revision 1.2  1992/12/02  00:25:01  harry
 * Add ability to dump pixmap.
 *
 * Revision 1.1  1992/11/20  00:02:25  harry
 * Initial revision
 *
 */

/* The following is the original copyright in the xwd software. */

/* Copyright 1987 Massachusetts Institute of Technology */

/*
 * xwd.c MIT Project Athena, X Window system window raster image dumper.
 *
 * This program will dump a raster image of the contents of a window into a 
 * file for output on graphics printers or for other uses.
 *
 *  Author:	Tony Della Fera, DEC
 *		17-Jun-85
 *
 *  Modification history:
 *
 *  11/14/86 Bill Wyatt, Smithsonian Astrophysical Observatory
 *    - Removed Z format option, changing it to an XY option. Monochrome 
 *      windows will always dump in XY format. Color windows will dump
 *      in Z format by default, but can be dumped in XY format with the
 *      -xy option.
 *
 *  11/18/86 Bill Wyatt
 *    - VERSION 6 is same as version 5 for monchrome. For colors, the 
 *      appropriate number of Color structs are dumped after the header,
 *      which has the number of colors (=0 for monochrome) in place of the
 *      V5 padding at the end. Up to 16-bit displays are supported. I
 *      don't yet know how 24- to 32-bit displays will be handled under
 *      the Version 11 protocol.
 *
 *  6/15/87 David Krikorian, MIT Project Athena
 *    - VERSION 7 runs under the X Version 11 servers, while the previous
 *      versions of xwd were are for X Version 10.  This version is based
 *      on xwd version 6, and should eventually have the same color
 *      abilities. (Xwd V7 has yet to be tested on a color machine, so
 *      all color-related code is commented out until color support
 *      becomes practical.)
 */

#ifndef lint
/* static char *rcsid_xwd_c = "$XConsortium: xwd.c,v 1.51 89/12/10 16:49:07 rws Exp $"; */
#endif

static char ident[] = "$Id: Window_Dump.c,v 1.10 2003/07/02 20:23:02 warren Exp $";

#include <X11/Xos.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/XWDFile.h>
#include <X11/Intrinsic.h>
#include <stdlib.h>
#include <stdio.h>
#include <signal.h>
#include <ive_gks.h>
#ifdef MEMDBG
#include <mnemosyne.h>
#else
#include <malloc.h>
#endif
#ifdef DOPNG
#include "png.h"        /* libpng header; includes zlib.h and setjmp.h */
#endif
extern void update_all_(),GifEncode();
extern unsigned long IveGetPixel();

extern Dimension loop_height, loop_width; /* Loop window dimensions */

#define FEEP_VOLUME 0

#ifdef DOPNG
static void png_error_handler(png_structp png_ptr, png_const_charp msg)
{

    /* This function, aside from the extra step of retrieving the "error
     * pointer" (below) and the fact that it exists within the application
     * rather than within libpng, is essentially identical to libpng's
     * default error handler.  The second point is critical:  since both
     * setjmp() and longjmp() are called from the same code, they are
     * guaranteed to have compatible notions of how big a jmp_buf is,
     * regardless of whether _BSD_SOURCE or anything else has (or has not)
     * been defined. */

    fprintf(stderr, "writepng error: %s\n", msg);
    fflush(stderr);

    longjmp(png_ptr->jmpbuf, 1);
}
#endif

static int minbit(it)
     CARD32 it;
{
  int i;
  for(i=0; i<32 && !((it >> i)&1); i++);
  if(i==32)i=-1; /*should never happen*/
  return i;
}

static Putword( w, fp )                                 /* Write out a word to the GIF file */
int w;
FILE *fp;
{
        fputc( w & 0xff, fp );
        fputc( (w / 256) & 0xff, fp );
}
 
int _swapshort (bp, n)
    register char *bp;
    register unsigned n;
{
    register char c;
    register char *ep = bp + n;

    while (bp < ep) {
	c = *bp;
	*bp = *(bp + 1);
	bp++;
	*bp++ = c;
    }
    return(1);
}

int _swaplong (bp, n)
    register char *bp;
    register unsigned n;
{
    register char c;
    register char *ep = bp + n;
    register char *sp;

    while (bp < ep) {
	sp = bp + 3;
	c = *sp;
	*sp = *bp;
	*bp++ = c;
	sp = bp + 1;
	c = *sp;
	*sp = *bp;
	*bp++ = c;
	bp += 2;
    }
    return(1);
}

int Image_Size(image)
     XImage *image;
{
/*     if (format != ZPixmap)
      return(image->bytes_per_line * image->height * image->depth); */

    return(image->bytes_per_line * image->height);
}

/*
 * Window_Dump: dump a window to a file which must already be open for
 *              writting.
 */

#ifndef MEMDBG
void *calloc();
#endif


int Window_Dump(dpy, window, pixmap, bell, type, out, buf, cmap)
    Display *dpy;
    Window window;
    Pixmap pixmap;
    Bool bell;
    char *type; /* XWD or GIF */
    FILE *out;
    char *buf;
    Colormap cmap;
{
    int screen;
    unsigned long swaptest = 1;
    XColor *colors=(XColor *)NULL;
    unsigned int buffer_size = 0;
    int win_name_size;
    int ncolors, i, j;
    char *win_name;
    unsigned char *new;
    Bool got_win_name;
    XWindowAttributes win_info;
    XImage *image = (XImage *)NULL;
    int absx, absy, x, y;
    unsigned int width, height;
    int dwidth, dheight;

    Window dummywin=(Window)NULL;
    void (*func)();

    func = signal(SIGIO, SIG_DFL);
    screen = DefaultScreen(dpy);

    /*
     * Inform the user not to alter the screen.
     */
    if (bell) XBell(dpy, 50);

    /*
     * Get the parameters of the window being dumped.
     */
    if(!XGetWindowAttributes(dpy, window, &win_info)) {
	sprintf(buf,"Can't get target window attributes.");
	signal(SIGIO, func);
	return(1);
    }
    /* handle any frame window */
    if (!XTranslateCoordinates (dpy, window, RootWindow (dpy, screen), 0, 0,
				&absx, &absy, &dummywin)) {
	sprintf(buf, "unable to translate window coordinates (%d,%d)\n",
		 absx, absy);
	signal(SIGIO, func);
	return(1);
    }
    win_info.x = absx;
    win_info.y = absy;
    width = win_info.width;
    height = win_info.height;
    dwidth = DisplayWidth (dpy, screen);
    dheight = DisplayHeight (dpy, screen);

    /* clip to window */
/* not here we don't
    if (absx < 0) width += absx, absx = 0;
    if (absy < 0) height += absy, absy = 0;
    if (absx + width > dwidth) width = dwidth - absx;
    if (absy + height > dheight) height = dheight - absy;
*/
    if (strcmp(type,"XWD") == 0) {
	XFetchName(dpy, window, &win_name);
	if (!win_name || !win_name[0]) {
	    win_name = "xwdump";
	    got_win_name = False;
	} else {
	    got_win_name = True;
	}

	/* sizeof(char) is included for the null string terminator. */
	win_name_size = strlen(win_name) + sizeof(char);
    }

    /*
     * Snarf the pixmap with XGetImage.
     */

    x = absx - win_info.x;
    y = absy - win_info.y;
    if (pixmap == 0) {
      
      /* Treat all windows as unmapped to handle case when 
	 window is partially off the screen.
      */
      /* 	if (win_info.map_state != IsViewable || absx+width > dwidth
	 || absy-height < 0) {*/
      /*
       * If window is unmapped, cannot use XGetImage directly.  Try to
       * create a pixmap and copy the image into it first, then perform
       * a GetImage on the pixmap.
       */
      
      if (win_info.backing_store != Always) {
	sprintf(buf, "Window must be mapped to get image\n");
	signal(SIGIO, func);
	return(1);
      }
      XRaiseWindow(dpy, window);
      (void)update_all_();
      XSync(dpy, False);
      XSync(dpy, False);
      XSync(dpy, False);
      XRaiseWindow(dpy, window);
      XSync(dpy, False);
      XSync(dpy, False);
      XSync(dpy, False);
      pixmap = XCreatePixmap(dpy, window, width, height, win_info.depth);
      XCopyArea(dpy, window, pixmap, DefaultGC(dpy, screen),
		x, y, width, height, 0, 0);
      image = XGetImage(dpy, pixmap, 0, 0, width, height,
			AllPlanes,ZPixmap);
      XFreePixmap(dpy, pixmap);

    }
    /*	else image = XGetImage (dpy, window, x, y, width, height, 
				AllPlanes, ZPixmap);

				}*/

    else {
      image = XGetImage (dpy, pixmap, 0, 0, loop_width, loop_height,
			 AllPlanes, ZPixmap);
    }
    if (!image) {
	sprintf (buf, "unable to get image at %dx%d+%d+%d\n",
		 width, height, x, y);
	signal(SIGIO, func);
	return(1);
    }

    /*
     * Determine the pixmap size.
     */
    buffer_size = Image_Size(image);
    /*    ncolors = Get_XColors(dpy, &win_info, &colors, cmap);*/
    /*
     * Inform the user that the image has been retrieved.
     */
    signal(SIGIO, func);
    if (bell) {
	XBell(dpy, FEEP_VOLUME);
	XBell(dpy, FEEP_VOLUME);
    }
    XFlush(dpy);
    if (strcmp(type,"GIF") == 0) {
	int *red, *green, *blue, tfile;
	unsigned char B;
	char *swap;
	CARD32 rshift,gshift,bshift;
	CARD32 rmask,gmask,bmask;
	char *line;
	char first = 0;
	line = 
	  (char *)malloc(image->bytes_per_line + 1);
	new = 
	  (unsigned char *)malloc(image->width * image->height + 1);
	/* get mask from windows if pixmap set them to 0 */
	if(image->red_mask)
	  rmask=image->red_mask;
	else
	  rmask=
	    DefaultVisualOfScreen(DefaultScreenOfDisplay(dpy))->red_mask;
	if(image->green_mask)
	  gmask=image->green_mask;
	else
	  gmask=
	    DefaultVisualOfScreen(DefaultScreenOfDisplay(dpy))->green_mask;
	if(image->blue_mask)
	  bmask=image->blue_mask;
	else
	  bmask=
	    DefaultVisualOfScreen(DefaultScreenOfDisplay(dpy))->blue_mask;
	rshift=minbit(rmask);
	gshift=minbit(gmask);	
	bshift=minbit(bmask);
	/*	if(image->bits_per_pixel != 32){
		free(line);
		free(new);
		printf("bad bits per pixel. We can do 8 or 32. You have %d\n",
		image->bytes_per_line);
		XDestroyImage(image);
		return(0);
		}*/
	ncolors = user_colors_.max_traj_color + 1; /* always less than 256*/
	red = (int *)malloc(sizeof(int) * 256);
	green = (int *)malloc(sizeof(int) * 256);
	blue = (int *)malloc(sizeof(int) * 256);

        if(image->bits_per_pixel>8){
          for (i=0; i < ncolors; i++) {
            XColor c;
            c.pixel=IveGetPixel(i);
            XQueryColor(dpy, cmap, &c);
            red[i] = c.red/256;
            green[i] = c.green/256;
            blue[i] = c.blue/256;
          }
          for (i=ncolors; i<256; i++) {
            red[i] = green[i] =blue[i] = 255;
          }
        }
        else{
          for (i=0; i<256; i++) {
            XColor c;
            c.pixel=(unsigned long)i;
            XQueryColor(dpy, cmap, &c);
            red[i] = c.red/256;
            green[i] = c.green/256;
            blue[i] = c.blue/256;
          }
        }

 	swap=image->data + image->xoffset;
	/*loop through data and fix it*/
	for(i=0; i<image->height; i++){
	  int j, k;
	  memcpy(line, swap, image->bytes_per_line);
	  swap += image->bytes_per_line;
	  switch(image->bits_per_pixel){
	  case 32:{
	    CARD32 *rgb;
	    rgb=(CARD32 *)line;
	    for(j=0; j<image->width; j++){
	      int r,g,b;
	      r=(*rgb & rmask) >>rshift;
	      g=(*rgb & gmask) >>gshift;
	      b=(*rgb & bmask) >>bshift;
	      
	      k=0;
	      while(k<ncolors){
		if(r == red[k] && g == green[k] && b == blue[k]){
		  new[j + i*image->width] = (char)k;
		  break;
		}
		k++;
		if(k==ncolors)printf("Oops no color match %d,%d,%d\n",r,g,b);
	      }
	      rgb++;
	    }
	  }
	  break;
	  case 16:{
	    CARD16 *rgb;
	    rgb=(CARD16 *)line;
	    for(j=0; j<image->width; j++){
	      int r,g,b;
	      r=(*rgb & rmask) >>rshift;
	      g=(*rgb & gmask) >>gshift;
	      b=(*rgb & bmask) >>bshift;
	      
	      k=0;
	      while(k<ncolors){
		if(r == red[k] && g == green[k] && b == blue[k]){
		  new[j + i*image->width] = (char)k;
		  break;
		}
		k++;
		if(k==ncolors)printf("Oops no color match %d,%d,%d\n",r,g,b);
	      }
	      rgb++;
	    }
	  }
	  break;
	  case 8:{
	    CARD8 *rgb;
	    rgb=(CARD8 *)line;
	    for(j=0; j<image->width; j++){
	      new[j + i*image->width] = (char)*rgb;
	      rgb++;
	    }
	  }
	  break;
	  }
	}
	image->depth=8;
	image->bytes_per_line=image->width;
	image->xoffset = 0;
	/*free(image->data);*/
	/*image->data=new;*/
	/*} old 8/32 bit else*/
	
	/*write out header*/
	
	fwrite( "GIF87a", 1, 6, out );           /* Write the Magic header */
	
        Putword( image->width, out );                  /* Write out the screen width and height */
        Putword( image->height, out );
	
        B = (1)  << 7;                        /* Yes, there is a color map */
	B |= (1) << 6;      /* OR in the Resolution*/
	B |= (1) << 5;
	B |= (1) << 4;
	B |= (1) << 2;               /*OR in the Bits per Pixel */
	B |= (1) << 1;
	B |= (1);	/*printf("B=%d\n",B);*/
        fputc( B,  out );                                         /* Write it out */
	
        fputc( 1, out );                        /* Write out the Background colour */
	
        fputc( 0, out );                                         /* Byte of 0's (future expansion) */
	
        for( i=0; i<256; i++ )         /* Write out the Global Colour Map */
	  {
	    fputc( red[i], out );
                        fputc( green[i], out );
                        fputc( blue[i], out );
	  }
	
        fputc( ',', out );                                               /* Write an Image separator */
	
        Putword( 0, out );                         /* Write the Image header */
        Putword( 0, out );
        Putword( image->width, out );
        Putword( image->height, out );
	/*not interlaced*/
	/*	fputc( 0x00, out );
		
		fputc( image->bits_per_pixel, out ); */                     /* Write out the initial code size */
	fputc(7,out); /*  000007 - MI000bpp M-> global colors I->sequential pixels 7->8bpp*/
	(void)GifEncode(out, new, (int)image->depth, (int)(image->width * image->height));
	/*end the file*/
	if(new != (unsigned char *)image->data && new != NULL)free(new);
	fflush(out);
	fputc( 0, out );                                         /* Write out Zero-length(to end series) */
	fputc( 0x3B, out );                                      /* Write the GIF file terminator */
	
	free(red);
	free(green);
	free(blue);
	red=NULL,green=NULL;blue=NULL;
    }
#ifdef DOPNG
    else if (strcmp(type,"PNG") == 0) {
      int *red, *green,* blue, tfile;
      unsigned char B;
      char *swap;
      CARD32 rshift,gshift,bshift;
      CARD32 rmask,gmask,bmask;
      char *line;
      char first = 0;
      png_structp  png_ptr;       /* note:  temporary variables! */
      png_infop  info_ptr;
      png_color *palette;


      line = 
	(char *)malloc(image->bytes_per_line + 1);
      new = 
	(unsigned char *)malloc(image->width*image->height);
      png_ptr = png_create_write_struct(PNG_LIBPNG_VER_STRING,
					(void *)NULL, png_error_handler, 
					(void *)NULL);
      if (png_ptr == NULL)
	{
	  return(1);
	}
      info_ptr = png_create_info_struct(png_ptr);
      if (info_ptr == NULL)
	{
	  png_destroy_write_struct(&png_ptr,  (png_infopp)NULL);
	  return(1);
	}
      if (setjmp(png_ptr->jmpbuf))
	{
	  /* If we get here, we had a problem reading the file */
	  png_destroy_write_struct(&png_ptr,  (png_infopp)NULL);
	  return(1);
	}
      png_init_io(png_ptr, out);
      png_set_IHDR(png_ptr, info_ptr, image->width, image->height, 
		   8, PNG_COLOR_TYPE_PALETTE,
		   PNG_INTERLACE_NONE, PNG_COMPRESSION_TYPE_BASE, 
		   PNG_FILTER_TYPE_BASE);
      
      /* get mask from windows if pixmap set them to 0 */
      if(image->red_mask)
	rmask=image->red_mask;
      else
	rmask=
	  DefaultVisualOfScreen(DefaultScreenOfDisplay(dpy))->red_mask;
      if(image->green_mask)
	gmask=image->green_mask;
      else
	gmask=
	  DefaultVisualOfScreen(DefaultScreenOfDisplay(dpy))->green_mask;
      if(image->blue_mask)
	bmask=image->blue_mask;
      else
	bmask=
	  DefaultVisualOfScreen(DefaultScreenOfDisplay(dpy))->blue_mask;
      rshift=gshift=bshift=0;
      while (!(rmask & 1)) {
	rmask >>= 1;
	rshift++;
      }
      while (!(gmask & 1)) {
	gmask >>= 1;
	gshift++;
      }
      while (!(bmask & 1)) {
	bmask >>= 1;
	bshift++;
      }
      
      red = (int *)malloc(sizeof(int) * 256);
      green = (int *)malloc(sizeof(int) * 256);
      blue = (int *)malloc(sizeof(int) * 256);
      ncolors=2;
      memset(red,0,256*sizeof(int));
      memset(green,0,256*sizeof(int));
      memset(blue,0,256*sizeof(int));
      red[1]=green[1]=blue[1]=255;
      palette = png_malloc(png_ptr, 256*sizeof(png_color));
      memset(palette,255,3*256);
      
      swap=image->data + image->xoffset;
      for(i=0; i<image->height; i++){
	int j;
	for(j=0; j< image->width; j++){
	  int k;
	  unsigned int r,g,b;
	  XColor c;
	  c.pixel=XGetPixel(image, j, i);
	  if(image->bits_per_pixel ==8){
	    XQueryColor(dpy, cmap, &c);
	    r = c.red;
	    g = c.green;
	    b = c.blue;
	  }
	  else{
	    r=((c.pixel >> rshift) & rmask);
	    g=((c.pixel >> gshift) & gmask);
	    b=((c.pixel >> bshift) & bmask);
	  }
	  for(k=0; k<ncolors; k++){
	    if(r == red[k] && g == green[k] && b == blue[k]){
	      new[j + i*image->width] = (unsigned char)k;
	      break;
	    }
	  }
	  if(k==ncolors && ncolors < 255){
	      red[ncolors] = r;
	      green[ncolors]=g;
	      blue[ncolors]=b;
	      new[j + i*image->width] = (unsigned char)k;
	      ncolors++;
	  }
	}
      }

      for(i=0; i<ncolors; i++){
	palette[i].red=red[i];
	palette[i].green=green[i];
	palette[i].blue=blue[i];
      }
      png_set_PLTE(png_ptr, info_ptr, palette, ncolors);
      png_write_info(png_ptr, info_ptr);


      for(i=0; i<image->height; i++){
	png_bytep row_pointer = new + i*image->width;
	png_write_rows(png_ptr,&row_pointer, 1);
      }
      png_write_end(png_ptr, info_ptr);
      free(info_ptr->palette);
      png_destroy_write_struct(&png_ptr, (png_infopp)NULL);


      }
#endif
      else {
      XWDFileHeader header;
      int header_size;
      
      /*
	 * Calculate header size.
	 */
      header_size = sizeof(header) + win_name_size;
      
      /*
       * Write out header information.
       */
      header.header_size = (CARD32) header_size;
      header.file_version = (CARD32) XWD_FILE_VERSION;
      header.pixmap_format = (CARD32) ZPixmap;
      header.pixmap_depth = (CARD32) image->depth;
      header.pixmap_width = (CARD32) image->width;
      header.pixmap_height = (CARD32) image->height;
      header.xoffset = (CARD32) image->xoffset;
      header.byte_order = (CARD32) image->byte_order;
      header.bitmap_unit = (CARD32) image->bitmap_unit;
      header.bitmap_bit_order = (CARD32) image->bitmap_bit_order;
      header.bitmap_pad = (CARD32) image->bitmap_pad;
      header.bits_per_pixel = (CARD32) image->bits_per_pixel;
      header.bytes_per_line = (CARD32) image->bytes_per_line;
      header.visual_class = (CARD32) win_info.visual->class;
      header.red_mask = (CARD32) win_info.visual->red_mask;
      header.green_mask = (CARD32) win_info.visual->green_mask;
      header.blue_mask = (CARD32) win_info.visual->blue_mask;
      header.bits_per_rgb = (CARD32) win_info.visual->bits_per_rgb;
      header.colormap_entries = (CARD32) win_info.visual->map_entries;
      header.ncolors = ncolors;
      header.window_width = (CARD32) win_info.width;
      header.window_height = (CARD32) win_info.height;
      header.window_x = absx;
      header.window_y = absy;
      header.window_bdrwidth = (CARD32) win_info.border_width;
      
      colors = (XColor *)malloc(ncolors*sizeof(XColor));
      for (i=0; i < ncolors; i++) {
	colors[i].pixel=IveGetPixel(i);
      }
      
      if (*(char *) &swaptest) {
	_swaplong((char *) &header, (unsigned)sizeof(header));
	for (i = 0; i < ncolors; i++) {
	  _swaplong((char *) &colors[i].pixel, (unsigned)sizeof(long));
	  _swapshort((char *) &colors[i].red, 3 * (unsigned)sizeof(short));
	}
      }
      
      (void) fwrite((char *)&header, sizeof(header), 1, out);
      (void) fwrite(win_name, win_name_size, 1, out);
      
      /*
       * Write out the color maps, if any
       */
      
      (void) fwrite((char *) colors, sizeof(XColor), ncolors, out);
      
      /*
       * Write out the buffer.
       *
       *    This copying of the bit stream (data) to a file is to be replaced
       *  by an Xlib call which hasn't been written yet.  It is not clear
       *  what other functions of xwd will be taken over by this (as yet)
       *  non-existant X function.
       */
      (void) fwrite(image->data, (int) buffer_size, 1, out);
      
      /*
       * Free window name string.
       */
      if (got_win_name) XFree(win_name);
      
    }
    /*
     * free the color buffer.
     */
    
    /*    if(ncolors > 0) free(colors);*/
    if(colors != NULL) free(colors);
    colors = NULL;
    /*
     * Free image
     */
    XDestroyImage(image);
    return(0);
}


/*
 * Determine the pixmap size.
 */


#define lowbit(x) ((x) & (~(x) + 1))

/*
 * Get the XColors of all pixels in image - returns # of colors
 */
int Get_XColors(dpy, win_info, colors, cmap)
     Display *dpy;
     XWindowAttributes *win_info;
     XColor **colors;
     Colormap cmap;
{
    int i, ncolors=0;

    if (!cmap)
	return(0);

    if (win_info->visual->class == TrueColor)
	return(0);    /* colormap is not needed */

    ncolors = win_info->visual->map_entries;
    if (!(*colors = (XColor *) malloc (sizeof(XColor) * ncolors))) {
      return(0);
    }
    if (win_info->visual->class == DirectColor) {
	Pixel red, green, blue, red1, green1, blue1;

	red = green = blue = 0;
	red1 = lowbit(win_info->visual->red_mask);
	green1 = lowbit(win_info->visual->green_mask);
	blue1 = lowbit(win_info->visual->blue_mask);
	for (i=0; i<ncolors; i++) {
	  (*colors)[i].pixel = red|green|blue;
	  (*colors)[i].pad = 0;
	  red += red1;
	  if (red > win_info->visual->red_mask)
	    red = 0;
	  green += green1;
	  if (green > win_info->visual->green_mask)
	    green = 0;
	  blue += blue1;
	  if (blue > win_info->visual->blue_mask)
	    blue = 0;
	}
    } else {
	for (i=0; i<ncolors; i++) {
	  (*colors)[i].pixel = i;
	  (*colors)[i].pad = 0;
	}
    }

    XQueryColors(dpy, cmap, *colors, ncolors);
    
    return(ncolors);
}

