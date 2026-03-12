# docker build -t coronasurveys:debugging .

F=0.01
S=300
r=100

E=0.01
S=300
GNAME=PrefA4000-50
/coronasurveys/bin/scaleUpDirectSurveyComparator -G $GNAME -s /coronasurveys/seeds.txt -f $F -S $S -r $r -e $E >$GNAME-f$f-S$S-r$r-e$E.out &
GNAME=PrefA10000-50
/coronasurveys/bin/scaleUpDirectSurveyComparator -G $GNAME -s /coronasurveys/seeds.txt -f $F -S $S -r $r -e $E >$GNAME-f$f-S$S-r$r-e$E.out &
GNAME=PrefA100000-50
/coronasurveys/bin/scaleUpDirectSurveyComparator -G $GNAME -s /coronasurveys/seeds.txt -f $F -S $S -r $r -e $E >$GNAME-f$f-S$S-r$r-e$E.out &
GNAME=RndG3000-40000
/coronasurveys/bin/scaleUpDirectSurveyComparator -G $GNAME -s /coronasurveys/seeds.txt -f $F -S $S -r $r -e $E >$GNAME-f$f-S$S-r$r-e$E.out &
GNAME=RndG3000000-40000
/coronasurveys/bin/scaleUpDirectSurveyComparator -G $GNAME -s /coronasurveys/seeds.txt -f $F -S $S -r $r -e $E >$GNAME-f$f-S$S-r$r-e$E.out &
GNAME=facebook_combined
/coronasurveys/bin/scaleUpDirectSurveyComparator -g /coronasurveys/graphs/$GNAME.txt -s /coronasurveys/seeds.txt -f $F -S $S -r $r >$GNAME-f$f-S$S-r$r-e$E.out &
S=3000
GNAME=com-dblp.ungraph
/coronasurveys/bin/scaleUpDirectSurveyComparator -g /coronasurveys/graphs/$GNAME.txt -s /coronasurveys/seeds.txt -f $F -S $S -r $r >$GNAME-f$f-S$S-r$r-e$E.out &
GNAME=ringmodel-100k
/coronasurveys/bin/scaleUpDirectSurveyComparator -g /coronasurveys/graphs/$GNAME.txt -s /coronasurveys/seeds.txt -f $F -S $S -r $r >$GNAME-f$f-S$S-r$r-e$E.out &


E=0
S=300
GNAME=PrefA4000-50
/coronasurveys/bin/scaleUpDirectSurveyComparator -G $GNAME -s /coronasurveys/seeds.txt -f $F -S $S -r $r -e $E >$GNAME-f$f-S$S-r$r-e$E.out &
GNAME=PrefA10000-50
/coronasurveys/bin/scaleUpDirectSurveyComparator -G $GNAME -s /coronasurveys/seeds.txt -f $F -S $S -r $r -e $E >$GNAME-f$f-S$S-r$r-e$E.out &
GNAME=PrefA100000-50
/coronasurveys/bin/scaleUpDirectSurveyComparator -G $GNAME -s /coronasurveys/seeds.txt -f $F -S $S -r $r -e $E >$GNAME-f$f-S$S-r$r-e$E.out &
GNAME=RndG3000-40000
/coronasurveys/bin/scaleUpDirectSurveyComparator -G $GNAME -s /coronasurveys/seeds.txt -f $F -S $S -r $r -e $E >$GNAME-f$f-S$S-r$r-e$E.out &
GNAME=RndG3000000-40000
/coronasurveys/bin/scaleUpDirectSurveyComparator -G $GNAME -s /coronasurveys/seeds.txt -f $F -S $S -r $r -e $E >$GNAME-f$f-S$S-r$r-e$E.out &
GNAME=facebook_combined
/coronasurveys/bin/scaleUpDirectSurveyComparator -g /coronasurveys/graphs/$GNAME.txt -s /coronasurveys/seeds.txt -f $F -S $S -r $r >$GNAME-f$f-S$S-r$r-e$E.out &
S=3000
GNAME=com-dblp.ungraph
/coronasurveys/bin/scaleUpDirectSurveyComparator -g /coronasurveys/graphs/$GNAME.txt -s /coronasurveys/seeds.txt -f $F -S $S -r $r >$GNAME-f$f-S$S-r$r-e$E.out &
GNAME=ringmodel-100k
/coronasurveys/bin/scaleUpDirectSurveyComparator -g /coronasurveys/graphs/$GNAME.txt -s /coronasurveys/seeds.txt -f $F -S $S -r $r >$GNAME-f$f-S$S-r$r-e$E.out &

r=150

E=0.01
S=300
GNAME=PrefA4000-50
/coronasurveys/bin/scaleUpDirectSurveyComparator -G $GNAME -s /coronasurveys/seeds.txt -f $F -S $S -r $r -e $E >$GNAME-f$f-S$S-r$r-e$E.out &
GNAME=PrefA10000-50
/coronasurveys/bin/scaleUpDirectSurveyComparator -G $GNAME -s /coronasurveys/seeds.txt -f $F -S $S -r $r -e $E >$GNAME-f$f-S$S-r$r-e$E.out &
GNAME=PrefA100000-50
/coronasurveys/bin/scaleUpDirectSurveyComparator -G $GNAME -s /coronasurveys/seeds.txt -f $F -S $S -r $r -e $E >$GNAME-f$f-S$S-r$r-e$E.out &
GNAME=RndG3000-40000
/coronasurveys/bin/scaleUpDirectSurveyComparator -G $GNAME -s /coronasurveys/seeds.txt -f $F -S $S -r $r -e $E >$GNAME-f$f-S$S-r$r-e$E.out &
GNAME=RndG3000000-40000
/coronasurveys/bin/scaleUpDirectSurveyComparator -G $GNAME -s /coronasurveys/seeds.txt -f $F -S $S -r $r -e $E >$GNAME-f$f-S$S-r$r-e$E.out &
GNAME=facebook_combined
/coronasurveys/bin/scaleUpDirectSurveyComparator -g /coronasurveys/graphs/$GNAME.txt -s /coronasurveys/seeds.txt -f $F -S $S -r $r >$GNAME-f$f-S$S-r$r-e$E.out &
S=3000
GNAME=com-dblp.ungraph
/coronasurveys/bin/scaleUpDirectSurveyComparator -g /coronasurveys/graphs/$GNAME.txt -s /coronasurveys/seeds.txt -f $F -S $S -r $r >$GNAME-f$f-S$S-r$r-e$E.out &
GNAME=ringmodel-100k
/coronasurveys/bin/scaleUpDirectSurveyComparator -g /coronasurveys/graphs/$GNAME.txt -s /coronasurveys/seeds.txt -f $F -S $S -r $r >$GNAME-f$f-S$S-r$r-e$E.out &


E=0
S=300
GNAME=PrefA4000-50
/coronasurveys/bin/scaleUpDirectSurveyComparator -G $GNAME -s /coronasurveys/seeds.txt -f $F -S $S -r $r -e $E >$GNAME-f$f-S$S-r$r-e$E.out &
GNAME=PrefA10000-50
/coronasurveys/bin/scaleUpDirectSurveyComparator -G $GNAME -s /coronasurveys/seeds.txt -f $F -S $S -r $r -e $E >$GNAME-f$f-S$S-r$r-e$E.out &
GNAME=PrefA100000-50
/coronasurveys/bin/scaleUpDirectSurveyComparator -G $GNAME -s /coronasurveys/seeds.txt -f $F -S $S -r $r -e $E >$GNAME-f$f-S$S-r$r-e$E.out &
GNAME=RndG3000-40000
/coronasurveys/bin/scaleUpDirectSurveyComparator -G $GNAME -s /coronasurveys/seeds.txt -f $F -S $S -r $r -e $E >$GNAME-f$f-S$S-r$r-e$E.out &
GNAME=RndG3000000-40000
/coronasurveys/bin/scaleUpDirectSurveyComparator -G $GNAME -s /coronasurveys/seeds.txt -f $F -S $S -r $r -e $E >$GNAME-f$f-S$S-r$r-e$E.out &
GNAME=facebook_combined
/coronasurveys/bin/scaleUpDirectSurveyComparator -g /coronasurveys/graphs/$GNAME.txt -s /coronasurveys/seeds.txt -f $F -S $S -r $r >$GNAME-f$f-S$S-r$r-e$E.out &
S=3000
GNAME=com-dblp.ungraph
/coronasurveys/bin/scaleUpDirectSurveyComparator -g /coronasurveys/graphs/$GNAME.txt -s /coronasurveys/seeds.txt -f $F -S $S -r $r >$GNAME-f$f-S$S-r$r-e$E.out &
GNAME=ringmodel-100k
/coronasurveys/bin/scaleUpDirectSurveyComparator -g /coronasurveys/graphs/$GNAME.txt -s /coronasurveys/seeds.txt -f $F -S $S -r $r >$GNAME-f$f-S$S-r$r-e$E.out &
