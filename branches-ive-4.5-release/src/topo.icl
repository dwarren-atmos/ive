c 	$Id: topo.icl,v 1.1 1998/07/28 18:22:36 harryive Exp $
c	Purpose:
c		To pass information between get2d(cv) and gettraj
c	for get_topo call.
c	$Log: topo.icl,v $
c	Revision 1.1  1998/07/28 18:22:36  harryive
c	New fixes for trajectories when they are the first overlay.
c	Also, remove old Yuri routine.
c	
c
	integer topo_ni, topo_nj
	real topo_pt1(MAXDIM), topo_pt3(MAXDIM)

	common/topo/ topo_ni, topo_nj, topo_pt1, topo_pt3
