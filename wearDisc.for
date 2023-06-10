C     子程序接口
      SUBROUTINE UMESHMOTION(UREF,ULOCAL,NODE,NNDOF,
     $     LNODETYPE,ALOCAL,NDIM,TIME,DTIME,PNEWDT,
     $     KSTEP,KINC,KMESHSWEEP,JMATYP,JGVBLOCK)
      include 'ABA_PARAM.INC'
C     定义矩阵或常量
      CHARACTER*80 PARTNAME
      DIMENSION ARRAY(1000)
      DIMENSION OLDSLIPZ(200000),TEMPSLIPZ(200000)
      COMMON OLDSLIPZ,TEMPSLIPZ
      DIMENSION ULOCAL(*)
      DIMENSION UGLOBAL(NDIM)
      DIMENSION JGVBLOCK(*),JMATYP(*)
      DIMENSION ALOCAL(NDIM,*)
      DIMENSION WVLOCAL(3),WVGLOBAL(3)
      PARAMETER (NELEMMAX=15000)
      DIMENSION JELEMLIST(NELEMMAX),JELEMTYPE(NELEMMAX)
      DIMENSION TIME(2)
      DOUBLE PRECISION PNEWDT,SURFV
      common /block/ INITIAL1，SURFV
      INITIAL1 = 1
      SURFV = 0.0D0
C     打开文本记录相关数据(我在文件夹里创建了几个txt文件用来记录计算中的数据）
      open(unit=11,file='F:\wear\0603\0604\cslipA.txt')
      open(unit=12,file='F:\wear\0603\0604\cstressA.txt')
      open(unit=13,file='F:\wear\0603\0604\surfvA.txt')
      open(unit=24,file='F:\wear\0603\0604\tempA.txt')
	  open(unit=14,file='F:\wear\0603\0604\cslipB.txt')
      open(unit=15,file='F:\wear\0603\0604\cstressB.txt')
      open(unit=16,file='F:\wear\0603\0604\surfvB.txt')
      open(unit=17,file='F:\wear\0603\0604\node1.txt')
      NELEMS = NELEMMAX
      LOCNUM = 0
      JRCD = 0
      PARTNAME = ' '
      JTYP=0
      
C     调用通用程序，获取节点信息（获取每个节点在每个增量步计算收敛后的接触压力、滑移增量）     
      CALL GETNODETOELEMCONN(NODE,NELEMS,JELEMLIST,JELEMTYPE,
     $     JRCD,JGVBLOCK)

       JRCD = 0
       LOCNUM = 0
       PARTNAME = ' '
      CALL GETPARTINFO(NODE,0,PARTNAME,LOCNUM,JRCD)

      CALL GETVRMAVGATNODE(NODE,JTYP,'CSTRESS',ARRAY,JRCD,
     $     JELEMLIST,NELEMS,JMATYP,JGVBLOCK)
      CPRESS = ARRAY(1)
      
      CALL GETVRMAVGATNODE(NODE,JTYP,'CDISP',ARRAY,JRCD,
     $     JELEMLIST,NELEMS,JMATYP,JGVBLOCK)
      CSLIP = SQRT(ARRAY(2)**2+ARRAY(3)**2)

      TEMPSLIPZ(NODE)=ABS(CSLIP-OLDSLIPZ(NODE))
      OLDSLIPZ(NODE)=CSLIP

      IF(CSLIP /= 0)THEN
	   WRITE(11,'(2(I6,5X),2(E12.5,5X))') NODE,KINC,TEMPSLIPZ(NODE),
     * OLDSLIPZ(NODE)
         IF(NODE .EQ.17)THEN
             WRITE(17,'(3(I6,5X),3(E12.5,5X))') NODE,INITIAL1,KINC,       
     $        CSLIP,TEMPSLIPZ(NODE),OLDSLIPZ(NODE)
          END IF
      END IF
      
      SURFV = 0.0D0
      IF (TIME(1) .GE. 0.01D0) THEN
          SURFV = 1000*2.88e-8*CPRESS*TEMPSLIPZ(NODE)
      ELSE
          SURFV = SURFV + 0.0D0  
      END IF
      
C     计算磨损量
C      SURFV=500*2.88e-8*CPRESS*TEMPSLIPZ(NODE)
      IF(CPRESS /= 0.0d0)THEN
          WRITE(12,'(2(I6,5X),2(E12.5,5X),I6)') NODE,KINC,CPRESS,
     * TEMPSLIPZ(NODE),LNODETYPE
	    WRITE(13,'(3(I6,5X),4(E12.5,5X))') NODE,KSTEP,KINC,SURFV,ULOCAL(1),
     * ULOCAL(2),ULOCAL(3)
      END IF
      
      IF(CPRESS /= 0.0d0)THEN
          WRITE(16,'(2(I6,5X),3(E12.5,5X),I6)') NODE,KINC,
     * TEMPSLIPZ(NODE),TIME(2),OLDSLIPZ(NODE),LNODETYPE
          WRITE(24,'(2(I6,5X),3(E12.5,5X),(I6))') NODE,KINC,
     * TEMPSLIPZ(NODE),SURFV,TIME(2),LNODETYPE
      END IF
      
C     计算配合内部节点的磨损方向
      ULOCAL(3)=ULOCAL(3)-SURFV

      Return
      End
        
