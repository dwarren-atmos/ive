### Version v1.2.1
Stable release. Fully backward compatible. Licensing changed.

+ Switch to dual licensing system: GPLv3 or MIT as user preference;

### Version v1.2.0
Stable release. Fully backward compatible. Modern compiler required.

+ Switch to the new Lib_Base64, [BeFoR64](https://github.com/szaghi/BeFoR64) project: the use of *deferred length string*, namely `character(len=:), allocatabale:: var`, imposes a modern compiler supporting automatic (re)allocation of LHS on assignment.

### Version v1.1.1

+ Extend VTM_WRF_XML function, now it is an interface to two functions, one for files list passed as an array and one for files list passed a single string. If a single string is used, the delimiter of each file can be customized, while the default values is '&';
+ The internal API of Type_VTM_File is changed: the `blk` member is now an array instead of a scalar to allow nested blocks.

##### Examples of usage

###### Example with array files list: 3 files block with default delimiter
```fortran
 E_IO = VTK_WRF_XML(flist=['file_1.vts','file_2.vts','file_3.vtu'])
```
###### Example with single string files list: 3 files block with default delimiter
```fortran
 E_IO = VTK_WRF_XML(flist='file_1.vts&file_2.vts&file_3.vtu')
```
###### Example with single string files list: 2 files block with custom delimiter (!!)
```fortran
 E_IO = VTK_WRF_XML(flist='file_1.vts!!file_2.vts',delimiter='!!')
```

### Version v1.1.0

+ Added packed API and 3D(or higher) arrays for VTK_VAR_XML function: this avoids the necessity of explicit reshape of multi-dimensional arrays containing saved variables in VAR callings; the following inputs are now available:
  + scalar input:
    + input is 1D-rank array: var[1:NC_NN];
    + input is 3D-rank array: var[nx1:nx2,ny1:ny2,nz1:nz2];
  + vectorial inputs:
    + inputs are 1D-rank arrays: varX[1:NC_NN],varY[1:NC_NN],varZ[1:NC_NN];
    + inputs are 3D-rank arrays: varX[nx1:nx2,ny1:ny2,nz1:nz2],varY[nx1:nx2,ny1:ny2,nz1:nz2],varX[nx1:nx2,ny1:ny2,nz1:nz2];
  + 3D(or higher) vectorial inputs:
    + input is 1D-rank (packed API): var[1:N_COL,1:NC_NN];
    + input is 3D-rank (packed API): var[1:N_COL,nx1:nx2,ny1:ny2,nz1:nz2].
+ Added packed API and 3D arrays for VTK_GEO and VTK_GEO_XML function: this avoids the necessity of explicit reshape of multi-dimensional arrays containing X, Y and Z coordinates in GEO callings; the following inputs are now available:
  + StructuredGrid (NN is the number of grid points, n\#1-n\#2, \#x,y,z are the domain extents):
    + 1D arrays of size NN: X[1:NN],Y[1:NN],Z[1:NN];
    + 3D arrays of size NN: X[nx1:nx2,ny1:ny2,nz1:nz2],Y[nx1:nx2,ny1:ny2,nz1:nz2],Z[nx1:nx2,ny1:ny2,nz1:nz2];
    + 1D array of size 3*NN (packed API): XYZ[1:3,1:NN];
    + 3D array of size 3*NN (packed API): XYZ[1:3,nx1:nx2,ny1:ny2,nz1:nz2].
  + UnStructuredGrid (NN is the number of grid points):
    + 1D arrays of size NN: X[1:NN],Y[1:NN],Z[1:NN];
    + 1D array of size 3*NN (packed API): XYZ[1:3,1:NN].
+ Added base64 encoding format: the output format specifier of VTK_INI_XML has been changed:
  + output_format = 'ascii' means \b ascii data, the same as the previous version;
  + output_format = 'binary' means \b base64 encoded data, different from the previous version where it meant appended raw-binary data; base64 encoding was missing in the previous version;
  + output_format = 'raw' means \b appended \b raw-binary data, as 'binary' of the previous version;
+ Added support for OpenMP multi-threads framework;
+ Correct bug affecting binary output;
+ implement concurrent multiple files IO capability;
+ implement FieldData tag for XML files, useful for tagging dataset with global auxiliary data, e.g. time, time step, ecc;
+ implement Parallel (Partitioned) XML files support (.pvtu,.pvts,.pvtr);
+ implement Driver testing program for providing practical examples of @libvtk usage;
+ added support for parallel framework, namely OpenMP (thread-safe) and MPI (process-safe).
