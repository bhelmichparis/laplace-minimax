#!/bin/sh
#
# This is the script for testing 
#
#    laplace-minimax.a 
#
#       by running 
#
#      test_laplace
#
# For the .check file ksh or bash is preferred, otherwise use sh
# (and hope it is not the old Bourne shell, which will not work)
#
if [ -x /bin/ksh ]; then
   CHECK_SHELL='/bin/ksh'
elif [ -x /bin/bash ]; then
   CHECK_SHELL='/bin/bash'
else
   CHECK_SHELL='/bin/sh'
fi

CHECK_SCRIPT='laplace-minimax.check'
LOG_FILE='/tmp/laplace-minimax.log'
TEST_PROG='./bin/test_laplace'

#######################################################################
# run the test
#######################################################################
if [ -x "${TEST_PROG}"  ]; then
 echo "\nTest convergence of the mimimax algorithm ...\n"
# ${TEST_PROG} > ${LOG_FILE}
 ${TEST_PROG} | tee ${LOG_FILE}
else
 echo "Cannot find test executable!"
 exit 1
fi

#######################################################################
#  CHECK SCRIPT
#######################################################################
echo "" > "${CHECK_SCRIPT}"
cat >> "${CHECK_SCRIPT}" <<'%EOF%'
log=$1

if [ `uname` = Linux ]; then
   GREP="grep -i -a"
else
   GREP="grep -i"
fi

# OED range
# Wave function:
CRIT1=`$GREP "range or orbital energy denominator:\ *0.400E+01\ *0.800E+01" $log | wc -l`
CRIT2=`$GREP "range or orbital energy denominator:\ *0.680E+01\ *0.920E+02" $log | wc -l`
CRIT3=`$GREP "range or orbital energy denominator:\ *0.320E+00\ *0.920E+04" $log | wc -l`
CRIT4=`$GREP "range or orbital energy denominator:\ *0.435E+02\ *0.464E+02" $log | wc -l`
CRIT5=`$GREP "range or orbital energy denominator:\ *0.360E+01\ *0.860E+01" $log | wc -l`
TEST[1]=`expr $CRIT1  \+ $CRIT2  \+ $CRIT3  \+ $CRIT4 \+ $CRIT5`
CTRL[1]=5
ERROR[1]="INCORRECT ORBITAL ENERGY DENOMINATOR RANGE NOT CORRECT"

# max error:
CRIT1=`$GREP "maximum absolute error of distribution:\ *0.183E-05" $log | wc -l`
CRIT2=`$GREP "maximum absolute error of distribution:\ *0.754E-08" $log | wc -l`
CRIT3=`$GREP "maximum absolute error of distribution:\ *0.589E-10" $log | wc -l`
CRIT4=`$GREP "maximum absolute error of distribution:\ *0.169E-11" $log | wc -l`
CRIT5=`$GREP "maximum absolute error of distribution:\ *0.844E-07" $log | wc -l`
TEST[2]=`expr $CRIT1  \+ $CRIT2  \+ $CRIT3  \+ $CRIT4  \+ $CRIT5`
CTRL[2]=5
ERROR[2]="MAXIMUM ABSOLUTE ERROR NOT CORRECT"

# RMSD error:
#CRIT1=`$GREP "RMSD error of distribution:\ *0.372E-07" $log | wc -l`
#CRIT2=`$GREP "RMSD error of distribution:\ *0.116E-09" $log | wc -l`
#CRIT3=`$GREP "RMSD error of distribution:\ *0.388E-10" $log | wc -l`
#CRIT4=`$GREP "RMSD error of distribution:\ *0.974E-14" $log | wc -l`
#TEST[3]=`expr $CRIT1  \+ $CRIT2  \+ $CRIT3  \+ $CRIT4`
#CTRL[3]=4
#ERROR[3]="RMSD ERROR NOT CORRECT"

# Laplace points (1):
CRIT1=`$GREP "1\ *0.0718733276\ *0.1867648544" $log | wc -l`
CRIT2=`$GREP "2\ *0.4011592651\ *0.4897225836" $log | wc -l`
CRIT3=`$GREP "3\ *1.1266216172\ *1.0404470994" $log | wc -l`
TEST[4]=`expr $CRIT1  \+ $CRIT2  \+ $CRIT3  \+ $CRIT4`
CTRL[4]=4
ERROR[4]="1ST SET OF QUADRATURE POINTS NOT CORRECT"

# Laplace points (2):
CRIT1=`$GREP "1\ *0.0048668703\ *0.0125691396" $log | wc -l`
CRIT2=`$GREP "2\ *0.0263616711\ *0.0309289959" $log | wc -l`
CRIT3=`$GREP "3\ *0.0682809545\ *0.0540252512" $log | wc -l`
CRIT4=`$GREP "4\ *0.1375783219\ *0.0867298102" $log | wc -l`
CRIT5=`$GREP "5\ *0.2476309385\ *0.1372836182" $log | wc -l`
CRIT6=`$GREP "6\ *0.4226716810\ *0.2198332379" $log | wc -l`
CRIT7=`$GREP "7\ *0.7073431760\ *0.3641556991" $log | wc -l`
CRIT8=`$GREP "8\ *1.2030465340\ *0.6778500284" $log | wc -l`
TEST[5]=`expr $CRIT1  \+ $CRIT2  \+ $CRIT3  \+ $CRIT4  \+ $CRIT5  \
           \+ $CRIT6  \+ $CRIT7  \+ $CRIT8`
CTRL[5]=8
ERROR[5]="2ND SET OF QUADRATURE POINTS NOT CORRECT"

# Laplace points (3):
CRIT1=`$GREP " 1\ * 0.0000567494\ * 0.0001469099" $log | wc -l`
CRIT2=`$GREP " 2\ * 0.0003106954\ * 0.0003693775" $log | wc -l`
CRIT3=`$GREP " 3\ * 0.0008221133\ * 0.0006730326" $log | wc -l`
CRIT4=`$GREP " 4\ * 0.0017133953\ * 0.0011484850" $log | wc -l`
CRIT5=`$GREP " 5\ * 0.0032245215\ * 0.0019426055" $log | wc -l`
CRIT6=`$GREP " 6\ * 0.0057771354\ * 0.0032759395" $log | wc -l`
CRIT7=`$GREP " 7\ * 0.0100651294\ * 0.0054813492" $log | wc -l`
CRIT8=`$GREP " 8\ * 0.0171984480\ * 0.0090717111" $log | wc -l`
CRIT9=`$GREP " 9\ * 0.0289293446\ * 0.0148381889" $log | wc -l`
CRIT10=`$GREP "10\ * 0.0479969786\ * 0.0239916129" $log | wc -l`
CRIT11=`$GREP "11\ * 0.0786435878\ * 0.0383684097" $log | wc -l`
CRIT12=`$GREP "12\ * 0.1273806420\ * 0.0607302985" $log | wc -l`
CRIT13=`$GREP "13\ * 0.2041177907\ * 0.0951980645" $log | wc -l`
CRIT14=`$GREP "14\ * 0.3238152880\ * 0.1478758062" $log | wc -l`
CRIT15=`$GREP "15\ * 0.5088877799\ * 0.2277450338" $log | wc -l`
CRIT16=`$GREP "16\ * 0.7926824362\ * 0.3479417620" $log | wc -l`
CRIT17=`$GREP "17\ * 1.2244920757\ * 0.5275833719" $log | wc -l`
CRIT18=`$GREP "18\ * 1.8767740637\ * 0.7944097216" $log | wc -l`
CRIT19=`$GREP "19\ * 2.8556015341\ * 1.1887157389" $log | wc -l`
CRIT20=`$GREP "20\ * 4.3160792262\ * 1.7696003867" $log | wc -l`
CRIT21=`$GREP "21\ * 6.4861350104\ * 2.6261576538" $log | wc -l`
CRIT22=`$GREP "22\ * 9.7068110990\ * 3.9014600347" $log | wc -l`
CRIT23=`$GREP "23\ *14.5126669046\ * 5.8570041856" $log | wc -l`
CRIT24=`$GREP "24\ *21.8390582637\ * 9.1039053934" $log | wc -l`
CRIT25=`$GREP "25\ *33.8353980668\ *15.9876940125" $log | wc -l`
TEST[6]=`expr $CRIT1  \+ $CRIT2  \+ $CRIT3  \+ $CRIT4  \+ $CRIT5  \
           \+ $CRIT6  \+ $CRIT7  \+ $CRIT8  \+ $CRIT9  \+ $CRIT10 \
           \+ $CRIT1  \+ $CRIT12 \+ $CRIT13 \+ $CRIT14 \+ $CRIT15 \
           \+ $CRIT16 \+ $CRIT17 \+ $CRIT18 \+ $CRIT19 \+ $CRIT20 \
           \+ $CRIT21 \+ $CRIT22 \+ $CRIT23 \+ $CRIT24 \+ $CRIT25`
CTRL[6]=25
ERROR[6]="3RD SET OF QUADRATURE POINTS NOT CORRECT"

# Laplace points (4):
CRIT1=`$GREP "1\ *0.0092540259\ *0.0239870689" $log | wc -l`
CRIT2=`$GREP "2\ *0.0510695061\ *0.0614912141" $log | wc -l`
CRIT3=`$GREP "3\ *0.1400400119\ *0.1247440969" $log | wc -l`
TEST[7]=`expr $CRIT1  \+ $CRIT2  \+ $CRIT3`
CTRL[7]=3
ERROR[7]="4TH SET OF QUADRATURE POINTS NOT CORRECT"

# Laplace points (5):
CRIT1=`$GREP "1\ *0.0557756372\ *0.1443139699" $log | wc -l`
CRIT2=`$GREP "2\ *0.3048285366\ *0.3619075837" $log | wc -l`
CRIT3=`$GREP "3\ *0.8075314691\ *0.6665573902" $log | wc -l`
CRIT4=`$GREP "4\ *1.7280901336\ *1.2665223550" $log | wc -l`
TEST[8]=`expr $CRIT1  \+ $CRIT2  \+ $CRIT3  \+ $CRIT4`
CTRL[8]=4
ERROR[8]="5TH SET OF QUADRATURE POINTS NOT CORRECT"

PASSED=1
#for i in 1 2 3 4 5 6 7
for i in 1 2 4 5 6 7 8
do
   if [ ${TEST[i]} -ne ${CTRL[i]} ]; then
     echo "${ERROR[i]} ( test = ${TEST[i]}; control = ${CTRL[i]} ); "
     PASSED=0
   fi
done

if [ $PASSED -eq 1 ]
then
  echo TEST ENDED PROPERLY
  exit 0
else
  echo THERE IS A PROBLEM
  exit 2
fi

%EOF%

#######################################################################
# do the check
#######################################################################
${CHECK_SHELL} ${CHECK_SCRIPT} ${LOG_FILE}


#######################################################################
# clean up
#######################################################################

if [ -x /bin/rm ]; then
 /bin/rm -f ${CHECK_SCRIPT} ${LOG_FILE}
else
 echo "/bin/rm not available?"
 exit 3
fi


