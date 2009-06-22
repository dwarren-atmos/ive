#define KNITHIGH(v1,v2,p0,p1,p2,p3) { \
  if(v1 < v2){  \
    tripts[numtri*3].x = p3.x;  \
    tripts[numtri*3].y = p3.y;  \
    tripts[numtri*3].x = p3.z;  \
	    \
    tripts[numtri*3 + 1].x = p2.x;  \
    tripts[numtri*3 + 1].y = p2.y;  \
    tripts[numtri*3 + 1].x = p2.z;  \
      \
    tripts[numtri*3 + 2].x = mid.x;  \
    tripts[numtri*3 + 2].y = mid.y;  \
    tripts[numtri*3 + 2].x = mid.z;  \
    numtri++;  \
      \
    tripts[numtri*3].x = p0.x;  \
    tripts[numtri*3].y = p0.y;  \
    tripts[numtri*3].x = p0.z;  \
	    \
    tripts[numtri*3 + 1].x = p1.x;  \
    tripts[numtri*3 + 1].y = p1.y;  \
    tripts[numtri*3 + 1].x = p1.z;  \
	    \
    tripts[numtri*3 + 2].x = mid.x;  \
    tripts[numtri*3 + 2].y = mid.y;  \
    tripts[numtri*3 + 2].x = mid.z;  \
    numtri++;  \
  }  \
  else{  \
    tripts[numtri*3].x = p3.x;  \
    tripts[numtri*3].y = p3.y;  \
    tripts[numtri*3].x = p3.z;  \
        \
    tripts[numtri*3 + 1].x = p0.x;  \
    tripts[numtri*3 + 1].y = p0.y;  \
    tripts[numtri*3 + 1].x = p0.z;  \
        \
    tripts[numtri*3 + 2].x = mid.x;  \
    tripts[numtri*3 + 2].y = mid.y;  \
    tripts[numtri*3 + 2].x = mid.z;  \
    numtri++;  \
  \
    tripts[numtri*3].x = p2.x;  \
    tripts[numtri*3].y = p2.y;  \
    tripts[numtri*3].x = p2.z;  \
        \
    tripts[numtri*3 + 1].x = p1.x;  \
    tripts[numtri*3 + 1].y = p1.y;  \
    tripts[numtri*3 + 1].x = p1.z;  \
        \
    tripts[numtri*3 + 2].x = mid.x;  \
    tripts[numtri*3 + 2].y = mid.y;  \
    tripts[numtri*3 + 2].x = mid.z;  \
    numtri++;  \
  }  \
} 
#define KNITLOW(v1,v2,p0,p1,p2,p3) {  \
  if(v1 > v2){  \
    tripts[numtri*3].x = p3.x;  \
    tripts[numtri*3].y = p3.y;  \
    tripts[numtri*3].x = p3.z;  \
	    \
    tripts[numtri*3 + 1].x = p2.x;  \
    tripts[numtri*3 + 1].y = p2.y;  \
    tripts[numtri*3 + 1].x = p2.z;  \
      \
    tripts[numtri*3 + 2].x = mid.x;  \
    tripts[numtri*3 + 2].y = mid.y;  \
    tripts[numtri*3 + 2].x = mid.z;  \
    numtri++;  \
      \
    tripts[numtri*3].x = p0.x;  \
    tripts[numtri*3].y = p0.y;  \
    tripts[numtri*3].x = p0.z;  \
	    \
    tripts[numtri*3 + 1].x = p1.x;  \
    tripts[numtri*3 + 1].y = p1.y;  \
    tripts[numtri*3 + 1].x = p1.z;  \
	    \
    tripts[numtri*3 + 2].x = mid.x;  \
    tripts[numtri*3 + 2].y = mid.y;  \
    tripts[numtri*3 + 2].x = mid.z;  \
    numtri++;  \
  }  \
  else{  \
    tripts[numtri*3].x = p3.x;  \
    tripts[numtri*3].y = p3.y;  \
    tripts[numtri*3].x = p3.z;  \
        \
    tripts[numtri*3 + 1].x = p0.x;  \
    tripts[numtri*3 + 1].y = p0.y;  \
    tripts[numtri*3 + 1].x = p0.z;  \
        \
    tripts[numtri*3 + 2].x = mid.x;  \
    tripts[numtri*3 + 2].y = mid.y;  \
    tripts[numtri*3 + 2].x = mid.z;  \
    numtri++;  \
  \
    tripts[numtri*3].x = p2.x;  \
    tripts[numtri*3].y = p2.y;  \
    tripts[numtri*3].x = p2.z;  \
        \
    tripts[numtri*3 + 1].x = p1.x;  \
    tripts[numtri*3 + 1].y = p1.y;  \
    tripts[numtri*3 + 1].x = p1.z;  \
        \
    tripts[numtri*3 + 2].x = mid.x;  \
    tripts[numtri*3 + 2].y = mid.y;  \
    tripts[numtri*3 + 2].x = mid.z;  \
    numtri++;  \
  }  \
}
