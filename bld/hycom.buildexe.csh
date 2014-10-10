#! /bin/csh -f 

set objdir = $OBJROOT/ocn/obj
set srcdir = $OBJROOT/ocn/src
mkdir -p $srcdir
mkdir -p $objdir
cd $srcdir
#cp -p -r $CODEROOT/ocn/hycom .
#cd ./hycom/sorc
cd $CODEROOT/ocn/hycom/sorc

gmake ARCH=AintelIFC TYPE=nuopc USER_DEFS=-DHYCOM_IN_CESM nuopc

cp -p *.mod $objdir
cp -p *.mod $LIBROOT/include
cp -p libhycom_nuopc.a $LIBROOT/libocn.a
