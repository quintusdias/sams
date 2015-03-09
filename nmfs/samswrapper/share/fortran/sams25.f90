program   SAMS

!use Numerical_Libraries
implicit none    

!DECLARATIONS ********************************************************************************************
Integer, Parameter :: InitialHarvestYears = 4
Integer, Parameter :: Access1 = -1, Access2 = -3,Access3 = -4 !access areas for first years 
Real*8, Parameter :: AccessF1 = 0.25, AccessF2 = 0.45, AccessF3 = 0.0           
Real*8, Parameter :: AccessF4 = 0.0, AccessF5 = 0.3, AccessF6 = 0.4             
Real*8, Parameter :: NewClF1 = 0.32,NewClF2 = 0.4,NewClF3 = 0.5
Real*8, Parameter :: Access2F1 = 0.1, Access2F2 = 0.0, Access2F3 = 0.0,  Access2F4 = 0.0, Access2F5 = 0.0, Access2F6 = 0.3
Real*8, Parameter :: Access3F1 = 0.0, Access3F2 =  0.0, Access3F3 = 0.15, Access3F4 = 0.4, Access3F5 = 0.5
Real*8, Parameter :: NewClsdF1 = 0.3, NewClsdF2 = 0.36,NewClsdF3 = 0.4
Real*8, Parameter :: NewClsd2F1 = 0.3, NewClsd2F2 = 0.36, NewClsd2F3 = 0.4
Integer, Parameter :: NumBycatch = 0
Logical, Parameter :: FixedStart = .False.
Logical, Parameter :: Closures = .TRUE.
Logical, Parameter :: ETlong = .TRUE.
Integer, Parameter :: FirstClsdReg = -1
Integer, Parameter :: SecondClsdReg = -2
Integer, Parameter :: ThirdClsdReg = -1
Integer, Parameter :: FirstClsdSub = -1
Integer, Parameter :: SecondClsdSub = -7
Integer, Parameter :: ThirdClsdSub = -6
Integer, Parameter :: Opnum = 7  !First open subarea in GB
Integer,Parameter  :: NewClosure = -1 !sumregion of new closure in MA 
Integer, Parameter :: Startyear = 0
Integer, Parameter :: GBRegion = 2

TYPE PopSummaryRecord
  Real*8 :: Bms, EBms,EBms4, BmsMT,EBmsMT,Num, ENum,ENum4, AvWt, AvEWt,AvEWt4
  Real*8 :: Fbm,Fn,Catch,CatchMT,CatchNum,DAS, LPUE,Eggs,AbsEggs,Growth
  Real*8 :: DredgeTime,DredgeArea,Recruits,Area,AvMC,MeatCounts(1:5),PctClosed
  Real*8 :: Price,Revenue,bycatch(1:NumBycatch),PctEBms
  Integer :: YearsClosed,Block,Region,Subarea
  Character*64 :: Name
END TYPE

TYPE DataNode
   Real*8 :: Growth, EBmsMT,PctEBms,cdf
   Integer :: Region,Subarea
   Type(DataNode), Pointer :: Link
END TYPE

Real*8 :: MaxPercentageClosed = 1.0
Real*8 :: Fudge = 1.0
Real*8, Parameter :: eps = 1e-5, StartingSH = 40.0
!Real*8, Parameter :: Dredge = 0.001152074, DredgeEff = 2.5 !=1/.4 - Estimated Dredge Eff 
Real*8, Parameter :: Discount = 0.07
Real*8 :: HSat = 74.31, Maxindv = 53000, breakpoint = 28.0 !parameters for LPUE/Biomass relationship
Real*8, Parameter :: MaxBottomTime = 19.5 !max hours per day on bottom
Real*8, Parameter :: TowingSpeed = 4.5 !speed while towing in nm/h
Real*8, Parameter :: DredgeWidth = 0.00493746
!Real*8, Parameter :: GMean = 3.8, theta = 0.5, GStd = 0.55,LStd = 0.97
Integer, Parameter :: LongTime = 20
Integer, Parameter :: NumMeatCounts = 5
Integer  :: NumClasses, TCount, MaxTCount, Region, NumRegions, SubArea, NumYears,RCount,RunCount,rotperiod,modyear,CurrentTime,YearCount,Count,year
Integer :: MASubareas !number of subareas in mid-atlantic
Integer :: FirstClass,FullClass !first class vul to fishery, and first class fully vul to fishery
Integer :: Ringsize,NumRuns,outputcode,NumBoots,gfoption,GrowthPeriod
Integer :: FirstExploit  !first size category capable of being exploited
Integer :: EndFirstYear !for short first year, negative if first year usual length
Integer :: RotStrategy,NumSubAreas,StartingYear,MinClosureTime,MaxClosureTime,ClosureLength
Integer :: NumStepsPerYear,FinalTCount
Integer :: RandomNumberSeed,GrowthMethod
Integer :: ChangeYear
Integer :: InitialManagement
Integer :: Count, Year, YearCount
Real*8 :: CullSize1,Selecta1,Selectb1,CullSize2,Selecta2,Selectb2
Character*64 :: MV !character to be assigned missing value
Character*64 :: InitFile !Initial values file
Character*64 :: OutputFileName
Logical :: NewYear
Logical :: Comma !true if comma delimited output is desired
Logical :: SFOutput !true if full-size freq is desired
Logical, Dimension(:,:), Allocatable :: HighFlag
Real*8 :: DredgeFootprint !Dredgefootprint = = 0.001152074 (area of a tow), DredgeEff = 1/estimated dredge efficiency 
Real*8 :: CullSize,MinSize,FullSize,TargetF
Real*8 :: CritGrowth,CritGrowth2
Real*8 :: StartFirstBin = 80.0 !beginning of first bin in two bin model
Real*8 :: ClassWidth, TotalHarvest, GlobalHarvest, Time, TimeStep, Temp
Real*8 :: Globalr, DiscHarvest, MeanHarvest
Real*8 :: HarSum,DiscSum,HarSS,DiscSS,HarMean,DiscMean,HarStddev,DiscStddev
Real*8 :: LastHarvest, Drops, DropsSS, LowF, HighF, OldGRecruit, TotalRecruits
Real*8 :: EqHarvest, TotEqHarvest, EqSS,EqStddev, EqMean
Real*8 :: BinSum, BinSumProduct, SubareaF
Real*8 :: SurveyCV, LandingsCV
Type(PopSummaryRecord), Dimension(:,:), Allocatable :: SubAreaSummary
Type(PopSummaryRecord), Dimension(:), Allocatable :: RegionSummary
Type(PopSummaryRecord) :: OverallSummary, OverallSummarySS, RunSummary
Type(PopSummaryRecord) :: RunSummarySS, GBop, GBcl
Type(DataNode), Pointer :: ListHeadMA,ListHeadGB
Real*8, Dimension (:,:,:,:), Allocatable :: Survive,Transition,Fish,NoFish ! 3rd subscript is region; 4th is open/closed 
Real*8, Dimension (:,:,:), Allocatable :: PopVector,InitialPop
Real*8, Dimension (:,:,:), Allocatable :: F, CatchVector, chol, Boot4080
Real*8, Dimension(:,:,:), Allocatable :: Boot80plus, bycatchrate, AccessF
Real*8, Dimension(:,:,:), Allocatable :: SelectivityVector, MeatWeight
Real*8, Dimension(:,:,:), Allocatable :: MeatWeight0, ExploitMW, ExploitMW0
Real*8, Dimension(:,:,:), Allocatable :: SurveyLengths
Real*8, Dimension (:,:), Allocatable :: ThisHarvest, Discards, Counts, Biomass
Real*8, Dimension(:,:), Allocatable :: Area, ExploitableBiomass, AvWt
Real*8, Dimension(:,:), Allocatable :: Recruits, OldLRecruit, Landings, Survey
Real*8, Dimension(:,:), Allocatable :: ESurvey,LPUE
Real*8, Dimension(:,:), Allocatable :: mu, TotalF, oldrecruits, a, b, LenFreq
Real*8, Dimension(:,:), Allocatable :: M, IncidentalM, DiscardM, Linf, K
Real*8, Dimension(:,:), Allocatable :: DredgeEff, RegionalF, FirstBin
Real*8, Dimension(:,:), Allocatable :: SecondBin, TwoBinF, StartingAge
Real*8, Dimension(:,:),Allocatable :: selecta,selectb  !selectivity parametrs
Real*8, Dimension (:), Allocatable :: FecundityVector,Har,oldgrecruits
Real*8, Dimension(:),Allocatable :: HarVec, DiscVec, EggVec, EqVec, theta
Real*8, Dimension(:), Allocatable :: DASTarget, Adjust(:)
! for GB Linf= 152.46, K = 0.3374 ,for MA = 151.84 0.2997
Real*8, Dimension(:), Allocatable :: MeanM, MeanDiscardM, MeanIncidentalM
Real*8, Dimension(:), Allocatable :: MeanLinf, MeanK, MeanDredgeEff, Mean_a
Real*8, Dimension(:), Allocatable :: Mean_b
Logical,Dimension(:,:,:), Allocatable :: Access  !true means the area is a special access/closed area
Integer, Dimension (:,:), Allocatable :: LastFished,YearsClosed
Integer, Dimension(0:LongTime) :: Bin



!BEGIN PROGRAM EXECUTION **********************************************************************************************************

!!!Initialize 
CALL RNOPT(5)  !set random number generator option
CALL ReadParameters(NumRuns, NumClasses, ClassWidth, NumRegions, NumSubareas,  &
                    NumStepsPerYear, NumYears, StartingYear, MinSize,          &
                    FullSize, CullSize, Ringsize, outputcode, SFOutput,        &
                    NumBoots, RandomNumberSeed, GrowthMethod, StartFirstBin,   &
                    OutputFileName, HSat, MaxIndv)   
CALL RNSET(RandomNumberSeed) !set random number seed
TimeStep = 1/Dble(NumStepsPerYear)
Allocate (SubareaSummary(1:NumRegions,1:NumSubAreas),                          &
          RegionSummary(1:NumRegions))
Allocate (Transition(1:NumClasses,1:NumClasses,1: NumRegions, 1:NumSubareas),  &
          Survive(1:NumClasses,1:NumClasses,1:NumRegions,1:NumSubareas))
Allocate (PopVector(1:NumClasses, 0:NumRegions, -2:NumSubareas),               &
          Fish(1:NumClasses,1:NumClasses,1:NumRegions,1:NumSubareas))
Allocate (F(1:NumRegions,1:NumSubareas,1:(NumYears+1)),                        &
          OldLRecruit(1:NumRegions,1:NumSubareas),                             &
          a(1:NumRegions,1:Numsubareas),                                       &
          b(1:NumRegions,1:Numsubareas),                                       &
          AvWt(1:NumRegions,1:NumSubareas),                                    &
          InitialPop(1:NumClasses,0:NumRegions,-2:NumSubareas),                &
          Recruits(1:NumRegions,1:NumSubareas))
Allocate (MeatWeight(1:NumClasses,1:NumRegions,1:NumSubareas),                 &
          MeatWeight0(1:NumClasses,1:NumRegions,1:NumSubareas),                &
          FecundityVector(1:NumClasses))
Allocate (ThisHarvest(1:NumRegions,1:NumSubareas),                             &
          Discards(1:NumRegions,1:NumSubareas),                                &
          Counts(1:NumRegions,1:NumSubareas),                                  &
          Biomass(1:NumRegions,1:NumSubareas),                                 &
          ExploitableBiomass(1:NumRegions,1:NumSubAreas),                      &
          Area(1:NumRegions,1:NumSubareas))
Allocate (HighFlag(1:NumRegions,1:NumSubareas),                                &
          LastFished(1:NumRegions,1:NumSubareas),                              &
          Har(0:NumYears),                                                     &
          NoFish(1:NumClasses,1:NumClasses,1:NumRegions,1:NumSubareas),        &
          YearsClosed(1:NumRegions,1:NumSubareas))
Allocate (StartingAge(1:NumRegions,1:NumSubareas),                             &
          DredgeEff(1:NumRegions,1:NumSubareas),                               &
          Linf(1:NumRegions,1:NumSubareas),                                    &
          K(1:NumRegions,1:NumSubareas),                                       &
          oldgrecruits(1:NumRegions))
Allocate (ExploitMW(1:NumClasses,1:NumRegions,1:NumSubareas),                  &
          ExploitMW0(1:NumClasses,1:NumRegions,1:NumSubareas),                 &
          SelectivityVector(1:NumClasses,1:NumRegions,1:NumSubareas))
Allocate (HarVec(1:NumRuns),                                                   &
          DiscVec(1:NumRuns),                                                  &
          EggVec(1:NumRuns),                                                   &
          EqVec(1:NumRuns),                                                    &
          M(1:NumRegions,1:NumSubareas),                                       &
          IncidentalM(1:NumRegions,1:NumSubareas),                             &
          DiscardM(1:NumRegions,1:NumSubareas))
Allocate (mu(1:Numregions,1:NumSubareas),                                      &
          theta(1:NumRegions),                                                 &
          chol(1:NumRegions,1:NumSubareas,1:NumSubareas),                      &
          oldrecruits(1:NumRegions,1:NumSubareas))
Allocate (Bycatchrate(1:NumBycatch,1:Numregions,1:NumSubareas),                &
          TotalF(1:NumRegions,1:NumSubareas))
Allocate (RegionalF(1:NumRegions,1:NumYears),                                  &
          selecta(1:NumRegions,1:NumSubareas),                                 &
          selectb(1:NumRegions,1:NumSubareas))
Allocate (Boot4080(1:NumRegions, 1:NumSubareas,1:NumBoots),                    &
          Boot80plus(1:NumRegions, 1:NumSubareas,1:NumBoots))
Allocate (DasTarget(1:NumYears))
Allocate (ListHeadMA,ListHeadGB)
Allocate (Landings(1:NumRegions,1:NumYears),                                   &
          LPUE(1:NumRegions,1:NumYears),                                       &
          Survey(1:NumRegions,0:NumYears),                                     &
          CatchVector(1:NumClasses,1:NumRegions,1:NumSubAreas))
Allocate (MeanM(1:NumRegions), MeanDiscardM(1:NumRegions),                     &
          MeanIncidentalM(1:NumRegions), MeanLinf(1:NumRegions),               &
          MeanK(1:NumRegions), MeanDredgeEff(1:NumRegions),                    &
          Mean_a(1:NumRegions), Mean_b(1:NumRegions))
Allocate (FirstBin(1:NumRegions,0:NumYears),                                   &
          SecondBin(1:NumRegions,0:NumYears),                                  &
          TwoBinF(1:NumRegions,1:NumYears),                                    &
          ESurvey(1:NumRegions,0:NumYears))
Allocate (Adjust(0:NumYears))
Allocate(Access(1:NumRegions,1:NumSubareas,1:NumYears),                        &
         AccessF(1:NumRegions,1:NumSubareas,1:NumYears))
CALL ReadArrayParameters(NumRegions, NumSubareas, Area, a, b, Linf, K,         &
                         DredgeFootPrint, DredgeEff)
Print *,"MADredge Eff = ",DredgeEff(1,:)
CALL ReadAccessAreaManagement(Access,AccessF)
CALL ReadMortality(NumRegions, NumYears, M, DiscardM, IncidentalM, F, LowF,    &
                   HighF, RotStrategy, CritGrowth, CritGrowth2, rotperiod,     &
                   dastarget)
continue
CALL ReadRecruitmentParams(NumRegions,NumSubareas,mu,theta,chol)


CALL ReadInitConditions(NumClasses,NumRegions,InitialPop)
if (NumBoots > 1) then
  CALL ReadBootData(NumBoots,Boot4080,Boot80plus)
endif
CALL CalcMeatWeight(NumRegions, NumSubareas, TimeStep, a, b, ringsize,         &
                    MeatWeight, MeatWeight0, ExploitMW, ExploitMW0, 0)
CALL CalcStartingAge(NumRegions,NumSubAreas,StartingAge)
CALL CalcFecundityVector(NumClasses,ClassWidth,FecundityVector,TimeStep)
CALL CalcRegionalAverages(NumRegions, NumSubareas, M, DiscardM,IncidentalM,    &
                          Linf, K,DredgeEff, a, b, MeanM, MeanDiscardM,        &
                          MeanIncidentalM, MeanLinf, MeanK, MeanDredgeEff,     &
                          Mean_a,Mean_b)
CALL InitToZero(HarSum,DiscSum,Drops,DropsSS,TotEqHarvest)
Bin = 0
GrowthPeriod = 1  !Default
FirstExploit = 0 !default
CALL OpenOutputFiles(OutputFileName)
if (GrowthMethod == 0) then  !create matrix from growth curves
     CALL CreateTransitionMatrix(NumClasses,NumRegions,ClassWidth,Transition,TimeStep)
else  !read in growth matrix
     CALL ReadTransitionMatrix(NumRegions,NumSubareas,NumClasses,Transition)
endif
Close (3, Status = 'KEEP') !close input file 
print *,"first closure = ",FirstClsdReg,FirstClsdSub
print *,"second closure = ",SecondClsdReg,SecondClsdSub
Do RunCount = 1,NumRuns
  Year = 0
  CALL InitRecruits(NumRegions,NumSubareas,mu,chol,oldrecruits)
  CALL InitializeSummaryRecords(Area,SubAreaSummary,RegionSummary,OverallSummary)
  if (RunCount > 1) then
    LastHarvest = TotalHarvest
  else
    LastHarvest = 0.0
  endif
  modyear = 1
  TotalHarvest = 0.0
  EqHarvest = 0.0
  DiscHarvest = 0.0
  TotalRecruits = 0.0
  Do Region = 1,NumRegions
    Do SubArea = 1,NumSubAreas
      HighFlag(Region,SubArea) = .FALSE.
      LastFished(Region,SubArea)  = 0
      YearsClosed(Region,SubArea) = 0
      TotalF(Region,Subarea) = 0
    EndDo
  EndDo
  PopVector = 0
  if (NumBoots > 1) then
    CALL Boot(NumBoots,InitialPop,PopVector)
  else
    PopVector = InitialPop
  endif
  Continue
  CALL CalcSummaryPopVector (PopVector,Area) 
  CALL CalcBins(FirstBin,SecondBin,PopVector,-1)
  CALL CalcAvWt(PopVector,ExploitMW0,AvWt)
  Do Region = 1,NumRegions
    Do Subarea = 1,NumSubareas
       CALL CalcSubareaSummary(PopVector(:,Region,Subarea),                    &
                               CatchVector(:,Region,Subarea),                  &
                               Region,                                         &
                               Subarea,                                        &
                               Transition(:,:,Region,Subarea),                 &
                               Recruits(Region,Subarea),                       &
                               SubareaSummary(Region,Subarea),                 &
                               Year,                                           &
                               F(region,subarea,1))
       RunSummary = OverallSummary
       RunSummarySS = Square(OverallSummary)
       CALL WriteOutput(RunCount, Year, NumRegions, NumSubareas, Region,       &
                        Subarea, SubareaSummary, RegionSummary, OverallSummary,&
                        outputcode,SFOutput)
    EndDo
  EndDo
  CALL CreateLinkedList
  CALL ComputeCumBms(1)
  CALL ComputeCumBms(2)
  !CALL PrintLinkedList(ListHeadMA)
  !CALL PrintLinkedList(ListHeadGB)
  CALL CalcF(F,NumRegions,modyear,HighF,LowF,HighFlag,LastFished,YearsClosed,PopVector,RotStrategy,0)
  if (Access1 > 0) then
    F(1,Access1,1) = AccessF1  
    F(1,Access2,1) = Access2F1  !hack ma access areas
    F(1,Access3,1) = Access3F1
  endif
 
  if (FirstClsdReg > 0) then
        F(FirstClsdReg,FirstClsdSub,year+1) = 0
  endif
  !if (SecondClsdReg > 0) then
  !  F(SecondClsdReg,SecondClsdSub,year+1) = 0
  !endif
  !F(FirstClsdReg,FirstClsdSub,1) = dble(0)
  CALL CreateSurvivalMatrix(NumClasses, NumRegions, ClassWidth, M, IncidentalM,&
                            DiscardM,F(:,:,1),Survive,Fish,TimeStep)
  CALL Initialize(NumRegions, NumClasses, ThisHarvest, Discards, GlobalHarvest,&
                  Biomass,ExploitableBiomass,Counts,CatchVector)
  OldGRecruit = -9999
  CALL CalcRecruitment(NumRegions,NumSubareas,mu,theta,chol,oldrecruits,recruits)
  !!! START MAIN LOOP ****************************
  Do Year = 0,NumYears -1
    if ((Year == 0) .AND. (EndFirstYear > 0)) then
       FinalTCount = EndFirstYear - 1
    else
       FinalTCount = NumStepsPerYear - 1
    endif
    Do TCount = 0,FinalTCount
      Time = Year*NumStepsPerYear + TCount + 1
      Do Region = 1,NumRegions
        Do SubArea = 1,NumSubareas
          if (Tcount > 0) then
            CALL CalcHarvest(MeatWeight(:,Region,Subarea),                     &
                             TotalHarvest,                                     &
                             ThisHarvest(Region,SubArea),                      &
                             GlobalHarvest,                                    &
                             Discards(Region,SubArea),                         &
                             PopVector, Fish, SubArea, Region, CatchVector,    &
                             Area,                                             &
                             DiscardM(Region,Subarea))
          endif

          ! Calculate Egg Production
          Temp = Dot_Product(FecundityVector,PopVector(:,Region,SubArea)) 
          CALL CalcCounts(NumClasses, PopVector, MeatWeight0(:,Region,Subarea),&
                          Region, Subarea, Counts, Biomass, ExploitableBiomass,&
                          ClassWidth, TimeStep, year)
          PopVector(:,Region,Subarea) = MatMul(Transition(:,:,Region,Subarea),    &
                                               MatMul(Survive(:,:,Region,Subarea),&
                                                      PopVector(:,Region,Subarea)))
          CALL AddRecruits(NumClasses,                                         &
                           PopVector(:,Region,Subarea),                        &
                           Recruits(Region,Subarea),                           &
                           TimeStep)                        


          !if end of year, write output etc
          if (TCount == FinalTCount) then
             CALL CalcSummaryPopVector(PopVector,Area)
             CALL CalcBins(FirstBin,SecondBin,PopVector,Year)
             CALL CalcHarvest(MeatWeight(:,Region,Subarea),                    &
                              TotalHarvest,                                    &
                              ThisHarvest(Region,SubArea),                     &
                              GlobalHarvest,                                   &
                              Discards(Region,SubArea),                        &
                              PopVector,                                       &
                              Fish,                                            &
                              SubArea,                                         &
                              Region,                                          &
                              CatchVector,                                     &
                              Area,                                            &
                              DiscardM(Region,Subarea))
             modyear = mod(year+1,rotperiod)
             SubareaF = F(region,subarea,year+1)
             CALL CalcSubareaSummary(PopVector(:,Region,Subarea),              &
                                     CatchVector(:,Region,Subarea),            &
                                     Region, Subarea,                          &
                                     Transition(:,:,Region,Subarea),           &
                                     Recruits(Region,Subarea),                 &
                                     SubareaSummary(Region,Subarea),           &
                                     Year+1,                                   &
                                     SubareaF)
             CALL WriteOutput(RunCount, Year+1, NumRegions, NumSubareas,       &
                              Region, Subarea, SubareaSummary, RegionSummary,  &
                              OverallSummary, outputcode, SFoutput)
             if ((Subarea == NumSubareas) .AND. (Region == NumRegions)) then
                CALL SumRec(RunSummary,OverallSummary)
                CALL SSRec(RunSummarySS,OverallSummary)
             endif
             DiscHarvest = DiscHarvest + GlobalHarvest*exp(-Discount*Time)
             if (Year > 9) then
                EqHarvest = EqHarvest + GlobalHarvest
             endif
          endif !TCount == FinalTCount
        End Do !Subarea
      End Do !Region F
      if (TCount == FinalTCount) then !if endo of year, then change Fs, recruit
        CALL CreateLinkedList
        CALL ComputeCumBms(1)
        CALL ComputeCumBms(2)
        !CALL PrintLinkedList(ListHeadMA)
        !CALL PrintLinkedList(ListHeadGB)
          Har(Year) = GlobalHarvest
             CALL Initialize(NumRegions, NumClasses, ThisHarvest, Discards,    &
                             GlobalHarvest, Biomass, ExploitableBiomass,       &
                             Counts,CatchVector) 
          CALL CalcRecruitment(NumRegions, NumSubareas, mu, theta, chol,       &
                               oldrecruits, recruits)

          !Calc avg size of exploitable scallops in each region
          CALL CalcAvWt(PopVector,ExploitMW0,AvWt)

          !print *,year,region,subarea
          !if (year < NumYears) then
          CALL CalcF(F(:,:,:), NumRegions, modyear, HighF, LowF, HighFlag,     &
                     LastFished, YearsClosed, PopVector, RotStrategy, year+1)
          CALL ModifySurvivalMatrix(NumClasses, NumRegions, ClassWidth, M,     &
                                    IncidentalM, DiscardM,                     &
                                    F(:,:,year+2),                             &
                                    Survive, Fish, TimeStep, year)
          CALL DestroyList(ListHeadMA)
          CALL DestroyList(ListHeadGB)
          !endif
       endif !TCount
       continue
    End Do !TCount

    CALL CalcBins(FirstBin,SecondBin,PopVector,Year)
  End Do !Year

  HarSS = 0.0
  MeanHarvest = TotalHarvest/NumYears
  Do YearCount = 1,NumYears
      HarSS = HarSS + (Har(YearCount) - MeanHarvest)**2
  EndDo
  if (NumYears > 1) then
      HarStddev = safesq(HarSS/(NumYears - 1))
  else
      HarStddev = 0.0
  endif
  ! Write (19,100) RunCount,MeanHarvest,DiscHarvest,EqHarvest, TotalRecruits,HarStddev
  HarSum = HarSum + TotalHarvest
  DiscSum = DiscSum + DiscHarvest
  TotEqHarvest = TotEqHarvest + EqHarvest
  HarVec(RunCount) = TotalHarvest
  DiscVec(RunCount) = DiscHarvest
  EqVec(RunCount) = EqHarvest
  if (TotalHarvest < LastHarvest) then
     Drops = Drops + LastHarvest - TotalHarvest
     DropsSS = DropsSS + (LastHarvest - TotalHarvest)**2
  endif
  !CALL WriteRunSummary(RunCount,Year,RunSummary,RunSummarySS)
  Print *, RunCount
  CALL WriteRegionalSummary(RunCount,NumRegions,NumYears)
  !CALL CalcTwoBinF(FirstBin,SecondBin,TwoBinF)
  !CALL WriteTwoBinF(FirstBin,SecondBin,TwoBinF)
EndDo !RunCount (main outer loop)

!!!
!!! Calculate and output final statistics
HarMean = HarSum/NumRuns
DiscMean = DiscSum/NumRuns
EqMean = TotEqHarvest/NumRuns
HarSS = 0.0
DiscSS = 0.0
EqSS = 0.0
BinSum = 0.0
BinSumProduct = 0.0
Do Count = 1,LongTime
  BinSum = BinSum + Bin(Count)
  BinSumProduct = BinSumProduct + Bin(Count)*Count
EndDo
Do RunCount = 1,NumRuns
   HarSS = HarSS + (HarVec(RunCount) - HarMean)**2
   DiscSS = DiscSS + (DiscVec(RunCount) - DiscMean)**2
   EqSS = EqSS + (EqVec(RunCount) - EqMean)**2
EndDo
if (NumRuns > 1) then
   HarStddev = safesq(HarSS/(NumRuns-1))
   DiscStddev = safesq(DiscSS/(NumRuns-1))
   EqStddev = safesq(EqSS/(NumRuns-1))
else
   HarStddev = 0.0
   DiscStddev = 0.0
   EqStddev = 0.0
endif
!Write (15,103) 'Total Recruits = ',TotalRecruits
!Write (19,101) 'Mean:   ',HarMean,DiscMean,EqMean
! Write (19,101) 'SD:     ',HarStddev,DiscStddev, EqStddev
! Write (19,102) 'Declines',Drops/NumRuns,safesq(DropsSS)/NumRuns
!Print *,BinSumProduct/32000,BinSumProduct/BinSum
!Close (9,Status = 'SAVE') 
Call EchoParams(RunCount, NumClasses, ClassWidth, NumRegions, NumSubareas,     &
                Numboots, MaxTCount, TimeStep, RingSize, DredgeFootprint, Linf,&
                K, a, b, DredgeEff, M, DiscardM, IncidentalM, Rotstrategy,     &
                HighF, LongTime, Bin)

Close (15,Status = 'KEEP')
if (SFOutput) then    
    Close (16,Status = 'KEEP')
    Close (11,Status = 'KEEP')
endif
Close(12,STATUS = "KEEP") 
Close (18,Status = 'KEEP')
Close(19,Status='KEEP')
Close(13,Status = 'KEEP')
DeAllocate(Transition,Survive,SubAreaSummary,RegionSummary)
DeAllocate (PopVector,Fish,CatchVector,InitialPop,M,IncidentalM,DiscardM)
DeAllocate (F,OldLRecruit,Oldgrecruits,Recruits,a,b)
DeAllocate (MeatWeight,MeatWeight0,FecundityVector,chol)
DeAllocate (mu,ThisHarvest,Discards,Counts,Biomass,Area,theta,bycatchrate)
DeAllocate (ListHeadMA,ListHeadGB)
!Read *
100 Format (I6,3X,E12.5,1X,E12.5,1X,E12.5,1X,E12.5,1X,E12.5,1X,E12.5)
101 Format (A8,2X,E13.5,2X,E13.5,2X,E13.5,2X,E13.5)
102 Format (A8,2X,E15.5,2X,E15.5)
103 Format (A17,1X,F20.0)



!!!! SUBROUTINES AND FUNCTIONS

CONTAINS

!!!! FUNCTIONS
Real*8 FUNCTION SH(age,Linf,K,StartingAge)
Real*8,Intent(In) :: age,Linf,K,StartingAge
  SH = Linf*(1-exp(-K*(age+StartingAge)))
END FUNCTION SH

Real*8 FUNCTION Age(SH,Linf,K,StartingAge)
Real*8,Intent(In) :: SH,Linf,K,StartingAge
  if (SH >= Linf) then
     Age = 99
  else
     Age = -log (1-SH/Linf)/K - StartingAge
  end if
END FUNCTION Age

SUBROUTINE CalcStartingAge(NumRegions,NumSubareas,StartingAge)
Integer,Intent(In) :: NumRegions,NumSubareas
Real*8, Dimension(1:NumRegions,1:NumSubareas),Intent(Out) :: StartingAge
Integer :: Region,Subarea

Do Region = 1,NumRegions
  Do Subarea = 1,NumSubareas
       StartingAge(Region,Subarea) = -log(1-StartingSH/Linf(Region,Subarea))/K(Region,Subarea)
  EndDo
EndDo

ENDSUBROUTINE CalcStartingAge


Real*8 FUNCTION Grow(ShellHeight,TimePeriod,Linf,K) 
!starting with shell height ShellHeight, calculates new shell height will be after a period TimePeriod 

Real*8, Intent(In) :: ShellHeight, TimePeriod,Linf,K
  Grow = ShellHeight + (Linf-ShellHeight)*(1-exp(-K*TimePeriod)) 
END FUNCTION !Grow


SUBROUTINE CalcLPUE(ESize,EBiomass,CPUE,DredgeTime,DredgeArea) !computes catch per unit effort, dredgetime and area
 
  Real*8, Intent(In) :: ESize,EBiomass
  Real*8, Intent(Out) :: CPUE,DredgeTime,DredgeArea !dredge bottom time and area swept per day
  Real*8 :: MaxShuck, ECount,ENum,Slope0
  Real*8,Parameter :: GramsPerPound = 454.0

  
  ECount = GramsPerPound/ESize
  if (ESize <= 1e-3) then
    MaxShuck = MaxIndv
  elseif (ESize < BreakPoint) then
    MaxShuck = MaxIndv
  else
    MaxShuck = MaxIndv/safesq(ESize/BreakPoint)
  endif
  ENum = EBiomass/ESize
  CPUE = MaxShuck*EBiomass/safesq(hsat**2 + ENum**2)/GramsPerPound
  !Print *,EBiomass, " ESize = ",ESize," ENum = ",ENum," CPUE = ",CPUE
  Slope0 = MaxIndv/Hsat/GramsPerPound
  DredgeTime = MaxBottomTime*CPUE/(Slope0*EBiomass)/Dble(24)
  DredgeArea = DredgeTime*TowingSpeed*Dble(24)*DredgeWidth
ENDSUBROUTINE !Calc LPUE

SUBROUTINE AddRecruits(NumClasses,PVector,Recruit,TimeStep)
   Integer, Intent(In) :: NumClasses
   Real*8, Intent(InOut) :: PVector(1:NumClasses)
   Real*8, Intent(In) :: Recruit,TimeStep
   Integer :: SizeClass
   if (TimeStep >= 0.99) then
      Do SizeClass = 1,5
             PVector(SizeClass) = PVector(SizeClass)+Recruit*TimeStep/Dble(7)
      EndDo
      Do SizeClass = 6,9
          PVector(SizeClass) = PVector(SizeClass) + Recruit*(10-SizeClass)*TimeStep/Dble(35)
      Enddo
   else
      PVector(1) = PVector(1)+ Recruit*TimeStep
   endif
ENDSUBROUTINE !AddRecruits
 

Real*8 FUNCTION LnPrice(Landings,ExMW)
Real*8, Intent(In) :: Landings, ExMW
Real*8, Parameter :: Intercept = 1.3327, Clandings = 1.43e-8, CDPI = 1.27e-05
Real*8, Parameter :: DPI = 26157, CPIM = 0.1119, PIM = 3.63, CAMC = -0.0028 
Real*8 :: Landingslbs

Landingslbs = 2204*Landings
if (Landings > eps) then
  LnPrice = Intercept - Clandings*Landingslbs + CDPI*DPI + CPIM*PIM + CAMC*ExMW
else
  LnPrice = 3
endif
ENDFUNCTION !Lnprice


Real*8 FUNCTION Revenue(Landings,ExMW)

Real*8, Intent(In) :: Landings,ExMW
Real*8 :: Landingslbs 

Landingslbs = Landings*2204
if (Landings > eps) then
   Revenue = exp(LnPrice(Landings,ExMW))*Landingslbs
else
   Revenue = Dble(0)
endif

ENDFUNCTION !Revenue

Real*8 FUNCTION Select(Class,ClassWidth,ringsize,year,Linf,K,region,subarea)
Integer, Intent(In) :: Class,ringsize,year,region,subarea
Real*8, Intent(In) :: ClassWidth,Linf,K
Real*8 :: Size
Integer :: Ring

if ((Year < 1) .AND. (Ringsize == 5)) then
  Ring = 3
elseif (Ringsize == 3) then
  Ring = 3
elseif (Ringsize < 0) then
  Ring = -1
else
  Ring = ringsize
endif
!Print *,"Ring = ",Ring
Size = StartingSH + ClassWidth*(Class - 0.5)
if (Size < Linf) then
  Size = Size + (Linf-Size)*(1-exp(-K*TimeStep/2))
endif
if (Ring == 3) then
   Select = 1/(1+exp(15.16-0.2021*Size))
elseif (Ring == 4) then
   Select = 1/(1+exp(9.692-0.1016*Size))
elseif (Ring == 1) then !area specific selectivity
   Select = 1/(1+exp(selecta(region,subarea)-selectb(region,subarea)*Size))
elseif (Ring < 0) then
  if (Year < ChangeYear) then
     Select = 1/(1+exp(Selecta1-Selectb1*Size))
     CullSize = CullSize1
  else
     Select = 1/ (1+exp(Selecta2-Selectb2*log(Size)))
     CullSize = CullSize2
  endif
endif

!if (Size < MinSize) then
!  Select = 0
!else if (Size < FullSize) then
!  Select = (Size - MinSize)/(FullSize - MinSize)
!else
!  Select = 1
!endif !3.5" ring selectivity
!if (ring == 4) then
!  if (Size < 99) then
!     Select = Select*0.59
!  elseif (Size < 104) then
!     Select = Select*0.77
!  elseif (Size < 109) then
!     Select = Select*0.85
!  elseif (Size < 124) then
!     Select = Select*0.9
!  endif !Size
!endif !ring
     
END FUNCTION !Select

SUBROUTINE CalcSelectivityVector(ringsize,SelectivityVector,year,Linf,K,NumRegions,NumSubareas)
Integer, Intent(In) :: ringsize,year,NumRegions,NumSubareas
Real*8, Intent(In) :: Linf(1:NumRegions,1:NumSubareas),K(1:NumRegions,1:NumSubareas)
Real*8, Intent(Out) :: SelectivityVector(1:NumClasses,1:NumRegions,1:NumSubareas)
Real*8 :: ShellHeight,Linf1,K1
Integer :: Class,Region,Subarea

Do Region = 1,NumRegions
Do Subarea = 1,NumSubareas
Linf1 = Linf(Region,Subarea)
K1 = K(Region,Subarea)
Do Class = 1,NumClasses
  ShellHeight = StartingSH + ClassWidth*(Class - 0.5) !StartingSH
  if (Shellheight < Linf1) then
     ShellHeight = ShellHeight + (Linf1-Shellheight)*(1-exp(-K1*TimeStep/2))
  endif
  SelectivityVector(Class,region,subarea) = Select(Class,ClassWidth,ringsize,year,Linf1,K1,region,subarea)
EndDo
EndDo
EndDo
ENDSUBROUTINE !CalcSelectivityVector


Real*8 FUNCTION ConvertToMT(Survey,Area,DredgeFootprint,DredgeEff)
!converts survey units (in grams) to abs units (in metric tons)
Real*8, Intent(In) :: Survey,Area,DredgeEff,DredgeFootprint

!Print *,Area,DredgeEff
ConvertToMT = Area*Survey/DredgeFootprint/1e6/DredgeEff

END FUNCTION !ConvertToMT

Real*8 FUNCTION MTRegion(Survey,Area,Region,DredgeFootprint,DredgeEff)
Real*8, Dimension (1:NumSubareas),Intent(In) :: Survey,Area,DredgeEff
Real*8,Intent(In) :: DredgeFootprint
Integer,Intent(In) :: Region 
Integer :: Subarea
Real*8 :: Sum

Sum = 0
Do Subarea = 1,NumSubareas
    Sum = Sum + ConvertToMT(Survey(Subarea),Area(Subarea),DredgeFootprint,DredgeEff(SubArea))
Enddo
MTRegion = Sum

END FUNCTION !MTConvert


Real*8 FUNCTION MTTotal(Survey,Area,DredgeFootprint,DredgeEff)
Real*8, Dimension(1:NumRegions,1:NumSubareas),Intent(In) :: Survey,Area,DredgeEff
Real*8,Intent(In) :: DredgeFootprint
Integer :: Region
Real*8 :: Sum

Sum = 0
Do Region = 1,NumRegions
     Sum = Sum + MTRegion(Survey(Region,:),Area(Region,:),Region,DredgeFootprint,DredgeEff(Region,:))
Enddo
MTTotal = Sum
END FUNCTION !MTTOTAL


Real*8 FUNCTION RegionalSummaryVector(SurveyVector,Area)
!Summarizes survey bms,catch, etc. over a region
Real*8, Dimension(1:NumSubareas),Intent(In) :: SurveyVector, Area
Real*8 :: Product,TotalArea

Product = Dot_Product(SurveyVector,Area)
TotalArea = Sum(Area)
RegionalSummaryVector = Product/TotalArea

END FUNCTION !RegionalSummaryVector


Real*8 FUNCTION TotalSummary(SurveyArray,Area)
!Summarizes survey bms,catch, etc. over all regions
Real*8, Dimension(1:NumRegions,1:NumSubareas), Intent(In) :: SurveyArray,Area
Real*8, Dimension(1:NumRegions) :: RegionSummary,RegionalArea
Real*8 :: TotalArea,Product
Integer :: Region
!combines numbers from individual areas into a composite index

Do Region = 1,NumRegions
  RegionSummary(Region) = RegionalSummaryVector(SurveyArray(Region,:),Area(Region,:))
  RegionalArea(Region) = Sum(Area(Region,:))
EndDo 
Product = Dot_Product(RegionSummary,RegionalArea)
TotalArea = Sum(RegionalArea)
TotalSummary = Product/TotalArea

ENDFUNCTION !Summarize

SUBROUTINE CalcMeatCounts(ScallopPop,MWVector,MCVector,Region,Subarea,Area)
Integer, Intent(In) :: Region,Subarea
Real*8, Intent(In) :: Area
Real*8, Dimension(1:NumClasses),Intent(In) :: ScallopPop,MWVector
Real*8, Intent(Out) :: MCVector(1:NumMeatCounts)
Real*8 :: ClassWeight,Total1,Total2
Integer :: Class,MCClass
MCVector = 0
Do Class = 1,NumClasses
  ClassWeight = MWVector(Class)*ScallopPop(Class)
  if (MWVector(Class) > 45.4) then  !U10s
     MCVector(1) = MCVector(1) + ClassWeight
  elseif (MWVector(Class) >= 22.7) then  !10-20s
     MCVector(2) = MCVector(2) + ClassWeight
  elseif (MWVector(Class) > 15.13333) then  !20-30s
     MCVector(3) = MCVector(3) + ClassWeight
  elseif (MWVector(Class) >= 11.35) then !30-40s
     MCVector(4) = MCVector(4) + ClassWeight
  else !40+
     MCVector(5) = MCVector(5) + ClassWeight
  endif
EndDo  !Class
Total1 = sum(MCVector)
Total2 = Dot_Product(MWVector,ScallopPop)
!print *,"totals = ",Total1,Total2
Do MCClass = 1,5
  MCVector(MCClass) = ConvertToMT(MCVector(MCClass),Area,DredgeFootPrint,DredgeEff(Region,Subarea))
EndDo
!print *,"summcmt = ",sum(MCVector)
ENDSUBROUTINE !CalcMeatCounts

SUBROUTINE CalcSummaryPopVector (PopVector,Area)
Real*8, Intent(InOut) :: PopVector(1:NumClasses,0:NumRegions,-2:NumSubareas)
Real*8,Intent(In) :: Area(1:NumRegions,1:NumSubareas)
Integer :: Region,Class
Real*8 :: RegArea,TotalArea
  PopVector(:,0,-2:0) = Dble(0)
  TotalArea = Dble(0)
  Do Region = 1,NumRegions
    RegArea = sum(area(Region,:))
    TotalArea = TotalArea + RegArea
    Do Class = 1,NumClasses
     PopVector(Class,Region,0) = Dot_Product(PopVector(Class,Region,1:NumSubAreas),Area(Region,:))/RegArea
     if (Region == GBRegion) then  !Calc Open and Closed PopVectors
         PopVector(Class,Region,-1) = Dot_Product(PopVector(Class,Region,OpNum:NumSubAreas),  &
                                                  Area(Region,OpNum:NumSubareas))             &
                                    / Sum(Area(Region,OpNum:NumSubareas))
         PopVector(Class,Region,-2) = Dot_Product(PopVector(Class,Region,1:(OpNum-1)),        &
                                                  Area(Region,1:(Opnum-1)))                   &
                                    / Sum(Area(Region,1:(OpNum-1)))
     endif
     PopVector(Class,0,0) = PopVector(Class,Region,0)*RegArea
    Enddo
  EndDo
  PopVector(:,0,0) = PopVector(:,0,0)/TotalArea
ENDSUBROUTINE

!!! I/O Routines

SUBROUTINE ReadParameters(RunCount, NumClasses, ClassWidth, NumRegions,        &
                          NumSubareas, NumStepsPerYear, NumYears, StartingYear,&
                          MinSize, FullSize, CullSize, RingSize, outputcode,   &
                          SFOutput, NumBoots, RandomNumberSeed, GrowthMethod,  &
                          StartFirstBin, OutputFileName, HSat, Maxindv)      
  Integer, Intent(Out) :: RunCount, NumClasses, NumRegions, NumSubAreas
  Integer, Intent(Out) :: StartingYear, NumStepsPerYear, RingSize, outputcode
  Integer, Intent(Out) :: NumBoots,NumYears,RandomNumberSeed,GrowthMethod
  Real*8, Intent(Out) :: ClassWidth,MinSize,FullSize,CullSize,StartFirstBin,HSat,Maxindv
  Logical, Intent(Out) :: SFOutput
  Character*64, Intent(Out) :: OutputFileName
  Character*64 :: InputFileName,BootFileName
  Integer :: Tempoutputcode
  Logical :: Iexist
  Logical,Parameter :: InputFlag = .False.  !true means prompt for file names, otherwise get from command line

 
  if (InputFlag .eqv. .TRUE.) then
     Print *,"Enter input file name"
     Read *,InputFileName
     Print *, "Enter output file name"
     Read *, OutputFileName
  else
    CALL Getarg(1,InputFileName)
    ! Check if File Exists
    Inquire (File=InputFileName,Exist=Iexist)
    IF (Iexist .EQV. .False.) THEN
        Write(*,*) 'Input File is Not Available !!'
        Call Exit(1)
    ELSE
      CALL Getarg(2,OutputFileName)
    ENDIF
  endif
  Open (3, File = InputFileName)
  Print *,"Reading parameters..."
  Read (3,*) RunCount,NumClasses, ClassWidth,GrowthMethod  !GrowthMethod = 0 - use Linf and K, GrowthMethod = 1 - use specified transition matrix
  Print *,"Runs = ",RunCount," NumClasses = ",NumClasses," ClassWidth = ",ClassWidth," GrowthMethod = ",GrowthMethod
  Read (3,*) NumRegions, NumSubareas, MASubareas,Tempoutputcode,NumBoots,RandomNumberSeed
  Print *,"Regions = ",Region," Subareas = ",NumSubareas," ",MASubareas
  Print *," OutputCode = ",Tempoutputcode," Boots = ",NumBoots," Seed = ",RandomNumberSeed
  OutputCode = mod(Tempoutputcode,10)
  SFOutput = (Tempoutputcode >= 100) !if Tempoutputcode >= 100, turn on size-freq output
  if (SFOutput) then
    Print *,"SFOutput"
  else
    Print *,"No SFOutput",TempOutputCode
  endif
  !format output for reading in various statistics programs
  if ((Tempoutputcode < 10) .OR. (Tempoutputcode >= 100)) then !statistica output
    MV = "-9999"
    Comma = .TRUE.
  elseif (Tempoutputcode < 20) then !SAS or systat output
    MV = "."
    Comma = .TRUE.
  else
    MV = 'NA'
    Comma = .FALSE.
  endif
  Read (3,*) NumYears, NumStepsPerYear,StartingYear,HSat,MaxIndv
  Print *,"Years = ",NumYears," Steps/Year = ",NumStepsPerYear," StartYear = ",StartingYear,'HSat = ',HSat,'MaxI = ',MaxIndv
  EndFirstYear = -1
  Read (3,*) MinSize,FullSize,CullSize,RingSize,StartFirstBin,InitialManagement
  !if (RingSize < 0)  then !user defined selectivity with a change
    Read (3,*) ChangeYear,CullSize1,Selecta1,Selectb1,CullSize2,Selecta2,Selectb2
  if (NumBoots > 1) then  !bootstraps are turned off when NumBoots = 1 (or less)
     Bootfilename = 'boot' // InputFileName
     Open(19, File = Bootfilename)
  endif
  continue
ENDSUBROUTINE !ReadParamters

SUBROUTINE ReadArrayParameters(NumRegions,NumSubareas,Area,a,b,Linf,K,DredgeFootPrint,DredgeEff)
  Integer, Intent(In) :: NumRegions,NumSubareas
  Real*8, Dimension(1:NumRegions,1:NumSubareas),Intent(Out) :: Area !area of each subregion
  Real*8, Dimension(1:NumRegions,1:NumSubareas), Intent(Out) :: K,Linf,DredgeEff
  Real*8, Dimension(1:NumRegions,1:Numsubareas),Intent(Out) :: a,b  !shell height/meat weight params
  Real*8,Intent(Out) :: DredgeFootprint
  Integer :: Region,subarea

  Print *,"Reading Array parameters... "
  Read (3,*) DredgeFootprint
  Do Region = 1,NumRegions
    Do Subarea = 1,NumSubareas
      Read (3,*) Linf(Region,Subarea),K(Region,Subarea),DredgeEff(Region,Subarea),selecta(Region,Subarea),selectb(Region,Subarea)  !DredgeEff = 1/q
      Print *,'Linf= ',floor(Linf(Region,Subarea)),K(Region,Subarea),DredgeEff(Region,Subarea)
    Enddo
    Do Subarea = 1,NumSubareas
       Read (3,*) a(Region,Subarea),b(Region,Subarea)
    EndDo
  EndDo
  Print *, 'Reading areas...'
  Do Region = 1,NumRegions
      Read (3,*) Area(Region,:)
      Print *, floor(Area(Region,:))
  EndDo
ENDSUBROUTINE !ReadArrayParameters


SUBROUTINE ReadMortality(NumRegions, NumYears, M, DiscardM, IncidentalM, F,    &
                         LowF, HighF, RotStrategy, CritGrowth, CritGrowth2,    &
                         rotperiod, DASTarget)
  Integer, Intent(In) :: NumRegions,NumYears
  Integer, Intent(Out) :: Rotperiod
  Real*8, Intent(Out) :: F(1:NumRegions,1:NumSubareas,1:NumYears+1),DasTarget(1:NumYears)
  Real*8, Dimension(1:NumRegions,1:NumSubareas),Intent(Out) :: M,IncidentalM,DiscardM
  Real*8, Intent(Out) :: LowF,HighF,CritGrowth,CritGrowth2
  Integer, Intent(Out) :: RotStrategy
  Integer :: Region,Year,Subarea,numinityears

  Print *,"Reading mortality parameters..."
  Do Region = 1,NumRegions
    Do Subarea = 1,NumSubareas
     Read (3,*) M(Region,Subarea),  DiscardM(Region,Subarea),IncidentalM(Region,Subarea)
     print *, M(Region,Subarea), DiscardM(Region,Subarea),IncidentalM(Region,Subarea)
    EndDo
  EndDo
  Read (3,*) RotStrategy,gfoption  !gfoption = 0 - all areas closed, = 1, access areas fished at F = 0.2, = 2 all areas fished at F = 0.2
  !1 = mech rot, 2 = constant uniform F, 3 adaptive rot, 0 = constant nonuniform F, -2 = user specified Fs, -1 user spec for 2 years, then constant F
  Print *,'RotStrategy = ',RotStrategy,'gfoption = ',gfoption
  Rotperiod = 1
  if (RotStrategy > 1) then
     Read (3,*) LowF, HighF, MaxPercentageClosed, FirstExploit
     Fudge = 1
     if (RotStrategy == 3) then
        Read (3,*) CritGrowth,CritGrowth2
        F(:,:,:) = LowF
     elseif (RotStrategy == 4) then
           Read (3,*) CritGrowth,CritGrowth2,MinClosureTime,MaxClosureTime
        F(:,:,:) = LowF
     elseif ((RotStrategy == 13)) then  !adaptive closure,  fixed time reopening
        Read (3,*) CritGrowth,ClosureLength
        Read (3,*) TargetF
        Print *,"CritGrowth = ",CritGrowth
        Print *,"Target F = ",TargetF
        F(:,:,:) = LowF
        Do Year = 0,NumYears
          Read (3,*) Adjust(Year)
          Print *,"Year = ",Year," Adjust = ",Adjust(Year)
        EndDo
     elseif (RotStrategy == 23) then
           Read (3,*) CritGrowth,ClosureLength
        Read (3,*) TargetF
        F(:,:,:) = LowF
        Do Year = 1,2
         Do Region = 1,NumRegions
           Read (3,*) F(Region,:,Year)
         Enddo
        Enddo
      elseif (RotStrategy == 25) then
           Read (3,*) CritGrowth,ClosureLength
        Read (3,*) TargetF
        F(:,:,:) = LowF
        Do Year = 1,2
         Do Region = 1,NumRegions
           Read (3,*) F(Region,:,Year)
         Enddo
        Enddo                
     elseif (RotStrategy == 14) then
        Read (3,*) CritGrowth,CritGrowth2,MinClosureTime,MaxClosureTime
        Read (3,*) TargetF
        F(:,:,:) = LowF
     elseif (RotStrategy == 24) then
        Read (3,*) CritGrowth,CritGrowth2,MinClosureTime,MaxClosureTime
        Read (3,*) TargetF
        F(:,:,:) = LowF
        Do Year = 1,2
         Do Region = 1,NumRegions
           Read (3,*) F(Region,:,Year)
         Enddo
       Enddo     
     endif
  elseif (RotStrategy == 1) then !mech rotation
     Read (3,*) rotperiod
    Do Year = 1,2
      Do Subarea = 1,NumSubareas
         Read (3,*) F(:,Subarea,Year)
      Enddo
    Enddo
    Do Year = 3,rotperiod+2
       Do Subarea = 1,NumSubareas
         Read(3,*) F(:,Subarea,Year)
       Enddo
       Do Region = 1,NumRegions
         Do Subarea = 1,NumSubareas
                F(Region,Subarea,Year) = F(Region,Subarea,Year) /(1+IncidentalM(Region,Subarea))
         EndDo
       EndDo
     EndDo
  elseif (RotStrategy == 0) then !specified first n years, constant fixed F after that
  read (3,*) NumInitYears
  Do Year = 1,NumInitYears
    Do Region = 1,Numregions
     Read (3,*) F(Region,:,Year)
     Do Subarea = 1,NumSubareas
       F(Region,Subarea,Year) = F(Region,Subarea,Year)/(1+IncidentalM(Region,Subarea))
     EndDo
    ENDDO
   ENDDO
   Do Region = 1,NumRegions
     Do Year = NumInitYears+1,NumYears+1
       F(Region,:,Year) = F(Region,:,NumInitYears)
     EndDo
    EndDo
  elseif (RotStrategy == -10) then !specified for first 2 years, then fixed non-uniform
    Do Region = 1,NumRegions
       Read (3,*) F(Region,:,3)
       Do Subarea = 1,NumSubareas
          F(Region,Subarea,3) = F(Region,Subarea,3)/(1+IncidentalM(Region,Subarea))
       EndDo
       Do Year = 4,NumYears
           F(Region,:,Year) = F(Region,:,3)
       Enddo
    Enddo
    Do Year = 1,2
      Do Region = 1,NumRegions
         Read (3,*) F(Region,:,Year)
      Enddo
    Enddo
  elseif (RotStrategy == -1) then !specified for first 2 years, then fixed uniform
    Read (3,*) LowF
    Do Region = 1,NumRegions
      Do Subarea = 1,NumSubareas
         F(Region,Subarea,:) = LowF/(1+IncidentalM(Region,Subarea))
      Enddo
    Enddo
    Do Year = 1,2
     Do Region = 1,NumRegions
       Read (3,*) F(Region,:,Year)
     Enddo
    Enddo
  elseif (RotStrategy == -2) then !determine F to fit DAS after first two years
    F(:,:,:) = 0
    Read (3,*) DasTarget(1)
    DasTarget = DasTarget(1)
    Do Region = 1,NumRegions  !read F for first two years
       Read (3,*) F(Region,:,1)
    EndDo
    Do Region = 1,NumRegions
       Read (3,*) F(Region,:,2)
    EndDo
  elseif (RotStrategy == -22) then !like -2 but with variable DAS
    F(:,:,:) = 0
    Do Region = 1,NumRegions  !read F for first two years
       Read (3,*) F(Region,:,1)
    EndDo
    Do Region = 1,NumRegions
       Read (3,*) F(Region,:,2)
    EndDo
    Do Year = 1,NumYears
      if (Year <= 2) then
         DasTarget(Year) = 0
      elseif (Year <= 30) then
         Read (3,*) DasTarget(Year)
      else
         DasTarget(Year) = DasTarget(15)
      endif
    Enddo
  elseif (RotStrategy == -12) then !specified for first 2 years, then set to target F with fleet dynamics
    F(:,:,:) = 0
    Read (3,*) TargetF
    Do Region = 1,NumRegions  !read F for first two years
       Read (3,*) F(Region,:,1)
    EndDo
    Do Region = 1,NumRegions
       Read (3,*) F(Region,:,2)
    EndDo
    Do Year = 2,NumYears
       Read (3,*) Adjust(Year)
    EndDo
  elseif (RotStrategy == -13) then
    F(:,:,:) = 0
    Read (3,*) TargetF
    Do Year = 0,NumYears
       Read (3,*) Adjust(Year)
    EndDo
  elseif (RotStrategy == -3) then
    Read (3,*) DasTarget
    Read (3,*) LowF, HighF
    F(:,:,:) = LowF
  elseif (RotStrategy == -99) then !specified for first 7 years, then fixed
    Do Year = 1,NumYears
        Do Region = 1,NumRegions
          if (Year <= 7) then
             Read(3,*) F(Region,:,Year)
          else
             F(Region,:,Year) = F(Region,:,7)
          endif
        EndDo
    EndDo
   elseif (RotStrategy == -9) then
     Do Year = 1,NumYears !user specified F
       Do Region = 1,NumRegions
          Read (3,*) F(Region,:,Year)
       Enddo
     Enddo
  endif
  if (GFOption == 99) then
     print *, "reading gf",NumYears
     Do Year = 1,NumYears
         Read(3,*) F(2,1:(Opnum-1),Year)
         print *, Year,F(2,:,Year)
      Enddo
   Endif
  Print *,"END READ MORT"
ENDSUBROUTINE !ReadMortality


SUBROUTINE ReadInitConditions(NumClasses,NumRegions,PopVector) 
  Integer, Intent(In) :: NumClasses, NumRegions
  Real*8, Intent(Out) :: PopVector(1:NumClasses,0:NumRegions,-2:NumSubareas)   
  Real*8 :: Temp(1:NumClasses)
  Integer :: SHClass,Region,SubArea
  
  Print *,"Reading Initial Conditions..."
  PopVector = 0
  Do Region = 1,NumRegions
    Do SubArea = 1,NumSubAreas
      Read (3,*) PopVector(:,Region,SubArea)
      Print *,Region,Subarea,PopVector(:,Region,Subarea) 
    End Do !Region
  End Do !SubArea
  continue
EndSubroutine !ReadInitConditions

SUBROUTINE ReadBootData(NumBoots,Boot4080,Boot80plus)
Integer, Intent(In) :: NumBoots
Real*8, Dimension(1:NumRegions, 1:NumSubareas,1:NumBoots), Intent(Out) :: Boot4080,Boot80plus
Integer :: Region,Subarea,Boot

Print *,"Reading bootstrap data"
Do Region = 1,NumRegions
  Do Boot = 1,NumBoots
    Read(19,*) Boot4080(Region,:,Boot),Boot80plus(Region,:,Boot)
  EndDo
EndDo
Close(19,Status = "KEEP")
ENDSUBROUTINE !ReadBootData


Subroutine ReadAccessAreaManagement(Access,AccessF) 
Logical,Intent(Out) :: Access(1:NumRegions,1:NumSubareas,1:NumYears)
Real*8,Intent(Out) :: AccessF(1:NumRegions,1:NumSubareas,1:NumYears)
Integer :: Region,Year,AccessAreas(1:NumSubareas),SubIndex,NAccess
Real*8 :: FMort(1:NumSubareas)


Access = .FALSE.
AccessF = 0
Print *,"Reading Access Area info..."
Do Region = 1,NumRegions
  Print *,"Region",Region
  Do Year = 1,NumYears
   Read (3,*)   NAccess,AccessAreas(1:NAccess),FMort(1:NAccess)
   Print *,Region,Year,NAccess,AccessAreas(1:NAccess),FMort(1:NAccess)
   !Read *
   Do SubIndex = 1,NAccess
      Access(Region,AccessAreas(SubIndex),Year) = .TRUE.
      AccessF(Region,AccessAreas(SubIndex),Year) = Fmort(SubIndex)
   enddo
  enddo
enddo
Print *,"End ReadAccessAreaManagement"
!Read *
EndSubroutine !ReadAccessAreaManagement

   

SUBROUTINE ReadTransitionMatrix(NumRegions,NumSubareas,NumClasses,Transition)
Integer, Intent(In) :: NumRegions,NumSubareas,NumClasses
Real*8, Intent(Out) :: Transition(1:NumClasses,1:NumClasses,1:NumRegions,1:NumSubareas)
Integer :: row,region,subarea

Print *,"Reading transition matrices..."
if (GrowthMethod == 1) then
  Do Region = 1,NumRegions
    Read (3,*)  !Skip label for each region
    Do row = 1,NumClasses
      Read(3,*) Transition(row,:,Region,1)
      !Print *,Transition(row,:,Region,1)
       Do Subarea = 2,NumSubareas
        Transition(row,:,region,Subarea) = Transition(row,:,region,1)
      Enddo
    EndDo
  EndDo
else
  Do Region = 1,NumRegions
    Read (3,*)  !Skip label for each region
     Do Subarea = 1,NumSubareas
      Read (3,*) !skip label
      Do row = 1,NumClasses
        !Print *,Region,Subarea,row
        Read(3,*) Transition(row,:,Region,Subarea)
        !Print *,Transition(row,:,Region,Subarea)
      Enddo
    EndDo
  EndDo
endif
Close(3, Status = 'KEEP')  
ENDSUBROUTINE !ReadTransitionMatrix
     

SUBROUTINE EchoParams(RunCount, NumClasses, ClassWidth, NumRegions,            &
                      NumSubareas, Numboots, MaxTCount, TimeStep, RingSize,    &
                      DredgeFootprint, Linf, K, a, b, DredgeEff, M, DiscardM,  &
                      IncidentalM, Rotstrategy, HighF, LongTime, Bin)
                 
    implicit none    
    Integer, Intent(In) :: RunCount, NumClasses, NumRegions, NumSubareas
    Integer, Intent(In) :: Numboots, MaxTCount, RingSize
    Real*8, Intent(In) :: ClassWidth, TimeStep, DredgeFootprint
    Real*8, Intent(In) :: Linf(1:NumRegions, 1:NumSubareas)
    Real*8, Intent(In) :: K(1:NumRegions, 1:NumSubareas)
    Real*8, Intent(In) :: a(1:NumRegions, 1:NumSubareas)
    Real*8, Intent(In) :: b(1:NumRegions, 1:NumSubareas)
    Real*8, Intent(In) :: DredgeEff(1:NumRegions, 1:NumSubareas)
    Real*8, Intent(In) :: M(1:NumRegions, 1:NumSubareas)
    Real*8, Intent(In) :: DiscardM(1:NumRegions, 1:NumSubareas)
    Real*8, Intent(In) :: IncidentalM(1:NumRegions, 1:NumSubareas)
    Integer, Intent(In) :: Rotstrategy
    Real*8, Intent(In) :: HighF
    Integer, Intent(In) :: LongTime
    Integer, Dimension(0:LongTime), Intent(In) :: Bin

Integer :: Region,Subarea

Write (18,93) "#Runs = ",RunCount," #Classes = ",NumClasses," ClassWidth = ",ClassWidth
Write (18,94) "NumRegions = ",NumRegions, " NumSubareas = ",NumSubareas," NumBoots = ", Numboots
Write (18,95) "NumTimeSteps = ", MaxTCount," Time Step = ",TimeStep," Ring size = ",RingSize
Write (18,96) "Dredge footprint = ",Dredgefootprint
Do Region = 1,NumRegions
  Do Subarea = 1,NumSubareas
   Write (18,9000,advance='no') Region, " Linf = ",Linf(Region,Subarea)
   Write (18,9001,advance='no') " K = ",K(Region,Subarea)
   Write (18,9002,advance='no') " a = ",a(Region,Subarea)
   Write (18,9002,advance='no') " b = ",b(Region,Subarea)
   Write (18,9003)              " DredgeEff = ",DredgeEff(Region,Subarea)

   Write (18,9100,advance='no') Region, " M = ",M(Region,Subarea)
   Write (18,9101,advance='no') " DiscardM = ",DiscardM(Region,Subarea)
   Write (18,9102)              " IncidentalM = ",IncidentalM(Region,Subarea)
  EndDo
Enddo
Write (18,97) "Rotational policy = ",Rotstrategy," F = ",HighF
Do Year = 1,LongTime
    Write (18,92) Bin(Year)
    if (Year == 10) then
      Write(18,*)
    endif
 enddo
Write (18,*) '% Closed',BinSumProduct/(32000)
Write (18,*) 'Avg Duration',BinSumProduct/BinSum
!
90 Format (I3,A8,F8.2,A5,F8.4,A5,F9.4,A5,F9.4,A13,F6.1)
91 Format (I3,A5,F6.2,A12,F6.2,A15,F7.3)
92 Format (I7,1X,$)
93 Format (A8,I6,A12,I5,A13,F6.1)
94 Format (A13,I5,A14,I5,A12,I8)
95 Format (A15,I7,A13,F7.3,A13,I4)
96 Format (A18,F12.6)
97 Format (A20,I5,A5,F7.3)
9000 Format (I3, A8, F8.2)
9001 Format (A5, F8.4)
9002 Format (A5, F9.4)
9003 Format (A13, F6.1)
9100 Format (I3,A5,F6.2)
9101 Format (A12, F6.2)
9102 Format (A15, F7.3)

ENDSUBROUTINE



SUBROUTINE Boot(NumBoots,ObsPopVector,AltPopVector)
Integer, Intent(In) :: NumBoots
Real*8, Intent(In) :: ObsPopVector(1:NumClasses, 0:NumRegions, -2:NumSubareas)
Real*8,Intent(Out) :: AltPopVector(1:NumClasses, 0:NumRegions, -2:NumSubareas)
Integer :: SizeClass,Draw4080,Draw80plus,Region,Subarea
Real*8 :: Rnd

Do Region = 1,NumRegions
  Do Subarea = 1,NumSubareas
    CALL Random_Number(Rnd)
    Draw4080 = Int(NumBoots*Rnd)+1
    if (Draw4080 > NumBoots) then
      Draw4080 = NumBoots
    endif
    Draw80plus = Int(NumBoots*Rnd) + 1
    if (Draw80plus > NumBoots) then
      Draw80plus = NumBoots
    endif
    Do SizeClass = 1,NumClasses
      if (StartingSH + ClassWidth*(SizeClass-0.5) < Dble(80)) then
         AltPopVector(SizeClass,Region,Subarea) = Boot4080(Region,Subarea,Draw4080)*ObsPopVector(SizeClass,Region,Subarea)
      else
         AltPopVector(SizeClass,Region,Subarea) = Boot80plus(Region,Subarea,Draw80plus)*ObsPopVector(SizeClass,Region,Subarea)
      endif
     EndDo
  EndDo
EndDo

ENDSUBROUTINE !Boot  

!SUBROUTINE ReadNameFile(Subarea,Regional,Overall)
!Type(PopSummaryRecord),Intent(InOut) :: Subarea(1:NumRegions,1:Numareas),Regional(1:NumRegions),Overall
!Character*64 :: Namefile

!ENDSUBROUTINE

SUBROUTINE ReadRecruitmentParams(NumRegions,NumSubareas,mu,theta,chol)
Integer, Intent(In) :: NumRegions,NumSubareas
Real*8, Dimension(1:NumRegions),Intent(Out) :: theta
Real*8, Dimension(1:NumRegions,1:NumSubareas), Intent(Out) :: mu
Real*8, Dimension(1:NumRegions,1:NumSubareas,1:NumSubareas),Intent(Out) ::chol 
Real*8,Dimension(1:NumSubareas,1:NumSubareas) :: CovMatrix,TempChol
Integer :: Region,Row,Col

Print *,"Reading recruitment parameters..."
Do Region = 1,NumRegions
  Read(3,*) mu(Region,:)  !mean logtransformed recruitment
  Print *, Region,"  ",mu(Region,:)
Enddo
Read(3,*) theta  !autocorrolation paramaters
print *,"reading cov matrices..."
Do Region = 1,NumRegions
  Do Row = 1,NumSubareas
    Read(3,*) CovMatrix(Row,:)
    print *,region," Cov Matrix Row ",Row," :",CovMatrix(Row,:)
  Enddo !row
  CALL DLFTDS(Numsubareas,CovMatrix,NumSubareas,TempChol,NumSubareas)  !compute cholosky factorization of covariance matrix
  Print *,region,row
 ! Print *, TempChol
  chol(region,:,:) = TempChol
Enddo !Region
continue
ENDSUBROUTINE !ReadRecruitmentParams   




SUBROUTINE OpenOutputFiles(OutputFileName)
Character*64,Intent(In) :: OutputFileName
Character*64 :: SummaryFileName, CFFileName, RunFileName, StdFileName,         &
        RecruitFileName, RunSumFileName, CasaFileName, SFFileName,             &
        TrueSummaryFileName
Integer :: SizeClass

 Print *,"Opening output files..."
 SummaryFileName = 's' // OutputFileName
 Open (15,File = SummaryFileName, Action = "WRITE") !summary file
 TrueSummaryFileName = 'tru'//OutputFileName
 Open(12,File = TrueSummaryFileName,Action = "WRITE")
 if (SFOutput) then
     SFFileName = 'f' // OutputFileName
     CFFileName = 'cf' // OutputFileName
     Open (16, File = SFFileName,Action = "WRITE") !Population size-freq file
     Write (16,520) 'Run, Year, Reg, Sreg,'
     Open (11, File = CFFileName,Action = "WRITE") !Catch at length file
      Write (11,520) 'Run, Year, Reg, Sreg,'
     Do Sizeclass = 1,NumClasses
        Write(16,521) Int(StartingSH + (SizeClass-1)*ClassWidth+0.5),","
           Write(11,521) Int(StartingSH + (SizeClass-1)*ClassWidth+0.5),","
     Enddo
     Write(16,*)
     Write(11,*)
 endif
 if (Comma) then 
   Write (15,'(A)',advance='no') ' Run#,Year,Reg,Sreg,Fbm,Fn,   Bms, ExplBms,  '
   Write (15,'(A)',advance='no') 'Numb,  ENum,Recrt,AvWt, AvgEWt,AvMC, Eggs, '
   Write (15,'(A)',advance='no') 'AbsEggs,  BmsMT,  EBmsMT,Catch, CatchN,'
   Write (15,'(A)',advance='no') ' CatchMT, CU10,  C1020, C2030,  C3040, C40pl,'
   Write (15,'(A)') 'Growth,  LPUE,  DAS, BotTime,BotArea, PctCl, Price,  Rev'
 else
   Write (15,'(A)',advance='no') ' Run# Year Reg Sreg F(bm) F(n)    Bms  ExplBms   '
   Write (15,'(A)',advance='no') 'Numb   ENum Recrt AvWt  AvgEWt AvMC  Eggs  '
   Write (15,'(A)',advance='no') 'AbsEggs   Bms(MT)   EBms(MT) Catch  CatchN  '
   Write (15,'(A)',advance='no') 'Catch(MT)  C-U10   C-1020  C-2030   C-3040  '
   Write (15,'(A)',advance='no') 'C-40+ Growth   LPUE   DAS  BotTime BotArea '
   Write (15,'(A)')              'PctCl  Price   Rev'
 endif
 RunSumFileName = 'rr' // OutputFileName
 Open (18,File=RunSumFileName,Action="Write")
 520 Format (A27,$)
 521 Format (I4,A1,1X,$)
 ENDSUBROUTINE !OpenOutputFiles


!!! Calculational Subroutines

Real*8 FUNCTION Y(Length,Linf) !y from Ssentongo and Larkin

Real*8,Intent(In) :: Length,Linf

if (Length > Linf) then
  if (Length < Linf + 5) then
     Y = -log(1-(Linf-1)/Linf)
  else
    Print *,"Error, Length > Linf"
    Read *
    Y = 0
  endif
else
  Y = -log(1-Length/Linf)
endif

ENDFUNCTION !Y


Real*8 FUNCTION EstZ(NumClasses,Linf,K,PopVec)

Real*8, Parameter :: KnifeEdge = 80.0
Integer, Intent(In) :: NumClasses
Real*8, Intent(In) :: PopVec(1:NumClasses),Linf,K
Real*8 :: Length,Ysum,Ytemp,Yc,Number,Ybar
Integer :: SizeClass

Yc = Y(KnifeEdge,Linf)
Ysum = Dble(0)
Number = Dble(0)
Do SizeClass = 1,NumClasses
   Length = 5*SizeClass + 37.5
   if (Length >= KnifeEdge) then
      Ytemp = Y(Length,Linf)
      Ysum = Ysum + Ytemp*PopVec(SizeClass)
      Number = Number + PopVec(SizeClass)
   endif
ENDDO
if (Number > 0) then
  Ybar = Ysum/Number
else
  Ybar = 0
endif
EstZ = K*Number/(Number+1)/(Ybar-Yc)

ENDFUNCTION EstZ


SUBROUTINE CalcRegionalAverages(NumRegions, NumSubareas, M, DiscardM,          &
                                IncidentalM, Linf, K, DredgeEff, a, b, MeanM,  &
                                MeanDiscardM, MeanIncidentalM, MeanLinf, MeanK,&
                                MeanDredgeEff, Mean_a, Mean_b)
Integer, Intent(In) :: NumRegions,NumSubareas
Real*8, Dimension(1:NumRegions,1:NumSubAreas),Intent(In) :: M,DiscardM,IncidentalM,Linf,K,DredgeEff,a,b
Real*8, Dimension(1:NumRegions), Intent(Out) :: MeanM,MeanDiscardM,MeanIncidentalM,MeanLinf,MeanK,MeanDredgeEff,Mean_a,Mean_b
Integer :: Region,Subarea
Real*8 :: RegionArea

Do Region = 1,NumRegions
  RegionArea = Sum(Area(Region,:))
  Do Subarea = 1,NumSubareas
    MeanM(Region) = Dot_Product(Area(Region,:),M(Region,:))/Sum(Area(Region,:))
    MeanDiscardM(Region) = Dot_Product(Area(Region,:),DiscardM(Region,:))/Sum(Area(Region,:))
    MeanIncidentalM(Region) = Dot_Product(Area(Region,:),IncidentalM(Region,:))/Sum(Area(Region,:))
    MeanLinf(Region) = Dot_Product(Area(Region,:),Linf(Region,:))/Sum(Area(Region,:))
    MeanK(Region) = Dot_Product(Area(Region,:),K(Region,:))/Sum(Area(Region,:))
    MeanDredgeEff = Dot_Product(Area(Region,:),DredgeEff(Region,:))/Sum(Area(Region,:))
    Mean_a = Dot_Product(Area(Region,:),a(Region,:))/Sum(Area(Region,:))
    Mean_b = Dot_Product(Area(Region,:),b(Region,:))/Sum(Area(Region,:))
  Enddo
Enddo

ENDSUBROUTINE !CalcRegionalAverages



SUBROUTINE InitRecruits(NumRegions,NumSubareas,mu,chol,oldrecruits)

Integer, Intent(In) :: NumRegions,NumSubareas
Real*8, Intent(In) :: mu(1:NumRegions,1:NumSubareas),chol(1:NumRegions,1:NumSubareas,1:NumSubareas)
Real*8, Intent(Out) :: OldRecruits(1:NumRegions,1:NumSubareas)
Real*8 :: Ranvector(1:NumSubareas)
Integer :: Region,Subarea

Do Region = 1,NumRegions
  CALL DRNMVN(1,Numsubareas,chol(Region,:,:),Numsubareas,ranvector,1)
  OldRecruits(Region,:) = exp(ranvector +mu(Region,:))
EndDo
ENDSUBROUTINE !InitRecruits

SUBROUTINE CalcRecruitment(NumRegions,NumSubareas,mu,theta,chol,oldrecruits,recruits)

Integer, Intent(In) :: NumRegions,NumSubareas
Real*8, Dimension(1:NumRegions,1:NumSubareas),Intent(In):: mu
Real*8, Intent(In) :: theta(1:NumRegions),chol(1:NumRegions,1:NumSubareas,1:NumSubareas)
Real*8, Intent(InOut) :: Oldrecruits(1:NumRegions,1:NumSubareas)
Real*8, Intent(Out) :: Recruits(1:NumRegions,1:NumSubareas)
Real*8, Dimension(1:NumSubAreas) :: ranvector
Real*8, Dimension(1:NumRegions,1:NumSubareas) :: newrecruits

Integer :: Region,Subarea

Do Region = 1,NumRegions
  CALL DRNMVN(1,Numsubareas,chol(Region,:,:),Numsubareas,ranvector,1)
  NewRecruits(Region,:) = exp(ranvector+mu(Region,:))
  Recruits(Region,:) = (NewRecruits(Region,:) + theta(Region)*Oldrecruits(Region,:))/(1+theta(Region))
EndDo
Write (9,*) int(recruits(1,:))
Oldrecruits = NewRecruits
900 Format(F10.3,2X,F10.3,2X,F10.3,2X,F10.3,2X,F12.4)
END SUBROUTINE CalcRecruitment

SUBROUTINE InitToZero(HarSum,DiscSum,Drops,DropsSS,TotEqHarvest)

Real*8, Intent(Out) :: HarSum,DiscSum,Drops,DropsSS,TotEqHarvest

HarSum = 0.0
DiscSum = 0.0
Drops = 0.0
DropsSS = 0.0
TotEqHarvest = 0.0

END SUBROUTINE

SUBROUTINE Initialize(NumRegions,NumClasses,ThisHarvest,Discards,GlobalHarvest,Biomass,ExploitableBiomass,Counts,CatchVector)
Integer, Intent(In) :: NumRegions,NumClasses
Real*8, Intent(Out) :: ThisHarvest(1:NumRegions,1:NumSubareas)
Real*8, Intent(out) :: Discards(1:NumRegions,1:NumSubareas)
Real*8, Intent(Out) :: GlobalHarvest,Biomass(1:NumRegions,1:NumSubareas)
Real*8, Intent(out) :: Counts(1:NumRegions,1:NumSubareas)
Real*8, Intent(Out) :: CatchVector(1:NumClasses,1:NumRegions,1:NumSubareas)
Real*8, Intent(Out) :: ExploitableBiomass(1:NumRegions,1:NumSubareas)
Integer :: Region,Subarea,Class

Do Region = 1,NumRegions
  Do Subarea = 1,NumSubareas
    ThisHarvest(Region,Subarea) = 0.0
    Discards(Region,Subarea) = 0.0
    Counts(Region,Subarea) = 0.0
    Biomass(Region,Subarea) = 0.0
    ExploitableBiomass(Region,Subarea) = 0.0
    ExploitableBiomass = 0.0
    Do Class = 1,NumClasses
      CatchVector(Class,Region,Subarea) = 0.0
    EndDo
  EndDo
EndDo
GlobalHarvest = 0.0
ENDSUBROUTINE !Initialize


SUBROUTINE CreateSurvivalMatrix(NumClasses,NumRegions,ClassWidth,M,IncidentalM,DiscardM,F,Survive,Fish,TimeStep)

Integer, Intent(In) :: NumRegions, NumClasses
Integer :: SubArea, Region,Col,Row
Real*8, Intent(In) :: ClassWidth,F(1:NumRegions,1:NumSubareas),TimeStep
Real*8, Dimension(1:NumRegions,1:NumSubareas), Intent(In) :: M,IncidentalM,DiscardM
Real*8 :: ShellHeight,NewShellHeight,NewAge,Z
Real*8, Intent(Out), Dimension(1:NumClasses,1:NumClasses,1:NumRegions,1:NumSubareas) :: Survive
Real*8, Intent(Out), Dimension(1:NumClasses,1:NumClasses,1:NumRegions,1:NumSubareas) :: Fish

   Survive = 0
   Fish = 0
   CALL ModifySurvivalMatrix(NumClasses,NumRegions,ClassWidth,M,IncidentalM,DiscardM,F,Survive,Fish,TimeStep,year)

END SUBROUTINE !CreateSurvivalMatrix


SUBROUTINE ModifySurvivalMatrix(NumClasses,NumRegions,ClassWidth,M,IncidentalM,DiscardM,F,Survive,Fish,TimeStep,year)
Integer, Intent(In) :: NumRegions, NumClasses,year
Integer :: Subarea, Region,Row
Real*8, Intent(In) :: ClassWidth,TimeStep
Real*8,Dimension(1:NumRegions,1:NumSubareas) :: M,F,IncidentalM,DiscardM
Real*8 :: ShellHeight,NewShellHeight,NewAge,Z
Real*8, Intent(InOut), Dimension(1:NumClasses,1:NumClasses,1:NumRegions,1:NumSubareas) :: Survive,Fish

!Print *,'F matrix'
!Print *, F
!Read *

Do Region = 1,NumRegions
  Do SubArea = 1,NumSubAreas
    Do Row = 1,NumClasses
          ShellHeight = StartingSH + Row*ClassWidth - ClassWidth/2 !average ShellHeight at beginning of period
       NewAge = Age(ShellHeight,Linf(Region,Subarea),K(Region,Subarea),StartingAge(Region,Subarea)) + TimeStep/2
       NewShellHeight = SH(NewAge,Linf(Region,Subarea),K(Region,Subarea),StartingAge(Region,Subarea))
       if (NewShellHeight < CullSize) then
          Survive(Row, Row, Region, Subarea) = exp((-M(Region,Subarea)                  &
                                                   - F(Region,Subarea)                  &
                                                     *(IncidentalM(Region,Subarea)      &
                                                       + DiscardM(Region,Subarea)       &
                                                         * F(Region,Subarea)            &
                                                         * Select(Row,                  &
                                                                  ClassWidth,           &
                                                                  ringsize,             &
                                                                  year,                 &
                                                                  Linf(Region,Subarea), &
                                                                  K(Region,Subarea),    &
                                                                  Region,Subarea)))     &
                                                   * TimeStep)
          Fish(Row,Row,Region,Subarea) = 0
        else
             Z = M(Region,Subarea)                                             &
               + F(Region,Subarea)*(IncidentalM(Region,Subarea)                &
               + Select(Row,ClassWidth,ringsize,year,Linf(Region,Subarea),K(Region,Subarea),Region,Subarea))
          Survive(Row,Row,Region,Subarea) = exp(-Z*TimeStep)
          Fish(Row, Row, Region, Subarea) = F(Region,Subarea)                  &
              * Select(Row,ClassWidth,ringsize,0,Linf(Region,Subarea),K(Region,Subarea),Region,Subarea) &
              * (1-exp(-Z*TimeStep))/Z
       endif !if NewShellHeight
!          Print *,Survive(Row,Row,Region,Subarea)
    enddo !row
  enddo !Subarea
  !Read *
enddo !Region
Continue

END SUBROUTINE !ModifySurvivalMatrix



SUBROUTINE CreateTransitionMatrix(NumClasses,NumRegions,ClassWidth,Transition,TimeStep)
!creates matrix which gives growth in one time step
Integer, Intent(In) :: NumRegions, NumClasses
Integer :: Subarea, Region,Col,Row,small
Real*8, Intent(In) :: ClassWidth,TimeStep
Real*8 :: Survive, ShellHeight,MinSH,MaxSH,NewMinSH, NewMaxSH,Smallest,Largest,Length
Real*8, Intent(Out), Dimension(1:NumClasses,1:NumClasses,1:NumRegions,1:NumSubareas) :: Transition


Do Region = 1,NumRegions
  Do SubArea = 1,NumSubareas
    Print *,"Trans Matrix for ",Region,Subarea
    Do Col = 1,NumClasses
      Transition(:,Col,Region,Subarea) = 0
      MinSH = (Col-1)*ClassWidth+StartingSH  !lower bound of starting sh bin
      MaxSH = Col*ClassWidth+StartingSH      !upper bound of starting sh bin
      if (MinSH < Linf(Region,Subarea)) then
          NewMinSH = MinSH + (Linf(Region,Subarea)-MinSH)*(1-exp(-K(Region,Subarea)*TimeStep))  !lower bound after growth
      else
          NewMinSH = MinSH  !if MinSH > Linf, no growth
      endif
      if (MaxSH < Linf(Region,Subarea)) then
          NewMaxSH = MaxSH + (Linf(Region,Subarea)-MaxSH)*(1-exp(-K(Region,Subarea)*TimeStep))  !upper bound after growth
      else
          NewMaxSH = MaxSH
      endif
      Smallest = 1+(NewMinSH-StartingSH)/ClassWidth   !smallest non-zero bin after growth
      Largest = 1+(NewMaxSH-StartingSH)/ClassWidth    !largest non-zero bin after growth
      Length = Largest - Smallest
      !Print *, TimeStep,NewMinSH,NewMaxSH,Smallest, Largest, Length
      !Read *
      small = Ceiling(Smallest+eps)    !1 bin larger than Smallest
      if (Largest <= small) then   !if all in one bin
         Transition(small - 1,Col,Region,Subarea) = 1
      else   !split into two bins
         Transition(small-1,Col,Region,Subarea) = (small - Smallest)/Length
         Transition(Floor(Largest),Col,Region,Subarea) = (Largest - Floor(Largest))/Length
      endif
      Do Row = small, Floor(Largest) - 1
         Transition(Row,Col,Region,Subarea) = 1/Length
      end do !row
        !Print Transition(:,Col,Region,Subarea)
        !Print * 
    end do !col
     Continue !Read *
  end do !Subarea
end do !Region
END Subroutine CreateTransitionMatrix
 

SUBROUTINE CalcMeatWeight(NumRegions,NumSubareas,TimeStep,a,b,ringsize,MeatWeight,MeatWeight0,ExploitMW,ExploitMW0,year)
 Real*8, Intent(In) :: TimeStep
 Integer, Intent(In) :: NumRegions,year,NumSubareas
 Real*8, Dimension(1:NumRegions,1:NumSubareas),Intent(In) :: a,b
 Real*8, Dimension(1:NumClasses,1:NumRegions,1:NumSubareas),Intent(Out) :: MeatWeight,MeatWeight0,ExploitMW,ExploitMW0
 Integer :: Class,Region,ringsize,Subarea
 Real*8 :: ShellHeight,SelectivityVector(1:NumClasses,1:NumRegions,1:NumSubareas)


CALL CalcSelectivityVector(ringsize,SelectivityVector,year,Linf,K,NumRegions,NumSubareas)
Do Region = 1,NumRegions
  Do Subarea = 1,NumSubareas
     Do Class = 1,NumClasses
       ShellHeight = (Class-0.5)*ClassWidth+StartingSH
       MeatWeight0(Class,Region,Subarea) = exp(a(Region,subarea)+b(Region,subarea)*log(ShellHeight))
       ShellHeight = Grow((Class-0.5)*ClassWidth+StartingSH,TimeStep/2,Linf(Region,Subarea),K(Region,Subarea))
       MeatWeight(Class,Region,Subarea) = exp(a(Region,subarea)+b(Region,subarea)*log(ShellHeight))
       ExploitMW(Class,Region,Subarea) = MeatWeight(Class,Region,subarea)*SelectivityVector(Class,Region,Subarea)
       ExploitMW0(Class,Region,Subarea) = MeatWeight0(Class,Region,subarea)*SelectivityVector(Class,Region,Subarea)
     !Print *,Class,ShellHeight,MeatWeight(Class)
     EndDo !Class
   Enddo !subarea
 EndDo  !Region
ENDSUBROUTINE CalcMeatWeight



Real*8 FUNCTION Fecundity(ShellHeight)
 Real*8, Intent(In) :: ShellHeight
 Real*8 :: Eggs(1:24)
 

 Eggs = (/2.5, 4.5, 6.5, 8.25, 10.75, 13.5, 17.25, 21.75, 27.25, 34.25, 42.5, 52.25, 63.5, 76.44, 91.24, 108.4, &
 127.24,  148.72,  172.72,   199.76,  229.76,  263.24, 300.76,  342.76 /)
 
 if (ShellHeight <= 45) then
   Fecundity = 0.0
 else if (ShellHeight >= 165) then
   Fecundity = 343.0/2
 else
   Fecundity = Eggs(Ceiling((ShellHeight-45)/5))/2
 endif
ENDFUNCTION Fecundity

SUBROUTINE CalcFecundityVector(NumClasses, ClassWidth, FecundityVector,TimeStep)
 Real*8, Intent(In) :: ClassWidth,TimeStep
 Integer, Intent(In) :: NumClasses
 Real*8, Intent(Out) :: FecundityVector(1:NumClasses)
 Integer :: ClassCount
 Real*8 :: AvgShellHeight

 Do ClassCount = 1,NumClasses
   AvgShellHeight = (ClassCount - 0.50)*ClassWidth + StartingSH
   FecundityVector(ClassCount) = Fecundity(AvgShellHeight)*TimeStep
 Enddo !ClassCount

ENDSUBROUTINE CalcFecundityVector

SUBROUTINE CalcHarvest(MeatWeight, TotalHarvest, ThisHarvest, GlobalHarvest,   &
                       Discard, PopVector, Fish, SubArea,Region, CatchVector,  &
                       Area, DiscardM)
 Real*8, Intent(In) :: PopVector(1:NumClasses, 0:NumRegions, -2:NumSubareas)
 Real*8, Intent(In) :: Fish(1:NumClasses,1:NumClasses,1:NumRegions,1:NumSubareas)
 Real*8, Intent(In) :: MeatWeight(1:NumClasses), Area(1:NumRegions,1:NumSubareas), DiscardM
 Real*8, Intent(InOut) :: TotalHarvest, GlobalHarvest, ThisHarvest, Discard
 Real*8, Intent(InOut) :: CatchVector(1:NumClasses,1:NumRegions,1:NumSubareas)
 Integer, Intent(In) :: SubArea,Region
 Integer :: Class
 Real*8 :: ThisDiscard,FishMort,Fished(1:NumClasses),ShellHeight,Temp
 
 

 Fished = MatMul(Fish(:,:,Region,Subarea),PopVector(:,Region,Subarea))
 FishMort = Dot_Product(MeatWeight,Fished)
 Temp = 0.0
 Do Class = FirstClass,NumClasses
   ShellHeight = (Class-0.5)*ClassWidth + StartingSH
   if (ShellHeight > CullSize) then 
      Temp = Temp + Fished(Class)*MeatWeight(Class)
      CatchVector(Class,Region,Subarea) = CatchVector(Class,Region,Subarea) + Fished(Class)
   endif
 EndDo !Class 
 ThisHarvest = ThisHarvest + Temp
 ThisDiscard = FishMort - Temp 
 GlobalHarvest = Area(Region,Subarea)*Temp + GlobalHarvest !- ThisDiscard Global Harvest by biomass, not including discards
 Discard = Discard + ThisDiscard/DiscardM
 TotalHarvest = TotalHarvest+ Area(Region,Subarea)*Temp
 !Print *,Fished
 !Read *
ENDSUBROUTINE !CalcHarvest

SUBROUTINE CalcAvWt(PopVector,HarvestMW,AvWt)
Real*8, Intent(In) :: PopVector(1:NumClasses,0:NumRegions,0:NumSubareas),HarvestMW(1:NumClasses,1:NumRegions,1:NumSubareas)
Real*8, Intent(Out) :: AvWt(1:NumRegions,1:NumSubareas)
Integer :: SizeClass,Region,SubArea
Real*8 :: ShellHeight, CountVector(1:NumClasses), ExplInd, ExplBiomass

Do SizeClass = 1,NumClasses !Calc CountVector, used to count effective number of vulnerable individuals
   ShellHeight = (SizeClass-0.5)*ClassWidth+StartingSH
   if (ShellHeight < MinSize) then
     CountVector(SizeClass) = 0.0
   elseif (ShellHeight < FullSize) then
     CountVector(SizeClass) = (ShellHeight-MinSize)/(FullSize-MinSize)
   else
     CountVector(SizeClass) = 1.0
   endif
EndDo
Do Region = 1,NumRegions
  Do SubArea = 1,NumSubAreas
    ExplInd = Dot_Product(CountVector,PopVector(:,Region,Subarea)) !Number of individuals vul to exploitation
    ExplBiomass = Dot_Product(HarvestMW(:,Region,Subarea),PopVector(:,Region,Subarea)) !Biomass vul to exploitation
    if (ExplInd < 1e-3) then
      AvWt(Region,Subarea) = 0.0
    else
      AvWt(Region,Subarea) = ExplBiomass/ExplInd
    endif
    !Print *,ExplInd,ExplBiomass,AvWt(Region,Subarea)
  EndDo !Subarea
EndDo !Region

END SUBROUTINE !CalcAvWt
      

Real*8 FUNCTION GrowthRate(NumClasses,M,TimeStep,Period,Transition,PopVector0,MeatWeight0)

Integer, Intent(In) :: NumClasses
Real*8, Intent(In) :: M,TimeStep,Period,Transition(1:NumClasses,1:NumClasses) !,Recruits
Real*8, Intent(In), Dimension(1:NumClasses) :: PopVector0,MeatWeight0
Real*8 :: BeginningMW, FinalMW, PopVector(1:NumClasses),InitPopVector(1:NumClasses) 
!PopVector0 = population at beginning of time period, PopVector = Population at end of time period
Integer :: Time,NumTimeSteps,SH

NumTimeSteps = Int(Period/TimeStep + 0.5) !Calc number time steps in one Period of time
InitPopVector = PopVector0
Do SH = 1,FirstExploit
  InitPopVector(SH) = 0
EndDo
PopVector = InitPopVector
Do Time = 1,NumTimeSteps  
   PopVector = MatMul(Transition,PopVector)
!  PopVector(1) = PopVector(1) + Recruits*TimeStep
EndDo
BeginningMW = Dot_Product(PopVector0,MeatWeight0)  !MW at beginning of period
FinalMW = exp(-TimeStep*NumTimeSteps*M)*Dot_Product(PopVector,MeatWeight0) !MW at end of period
if (BeginningMW > 1e-9) then
   GrowthRate = (FinalMW - BeginningMW)/(TimeStep*NumTimeSteps*BeginningMW)
else
   GrowthRate = DBLE(-1)
endif
!Print *, (FinalMW - BeginningMW)/(TimeStep*NumTimeSteps*BeginningMW)
ENDFUNCTION GrowthRate 



SUBROUTINE CreateLinkedList
Type(DataNode), Pointer :: Head,ListPointer,NewPointer,OldPointer
Integer :: Region,Subarea

Do Region = 1,NumRegions
  if (Region == 1) then
    Head => ListHeadMA
  else
    Head => ListHeadGB
  endif
  Do Subarea = 1,NumSubareas
     if (Subarea == 1) then
        Head%Growth = GrowthRate(NumClasses,MeanM(Region),TimeStep,Dble(GrowthPeriod),Transition,PopVector(:,Region,1),MeatWeight0)
        Head%EBmsMT = SubareaSummary(Region,Subarea)%EBmsMT
        Head%Region = Region
        Head%Subarea = 1
        Nullify(Head%Link)
     elseif (((Region ==1) .AND. (Subarea >= 10)) .OR. ((Region == 2) .AND. (Subarea >= 9))) then
        Continue
     else
        ListPointer => Head
        Allocate(NewPointer)
        NewPointer%Growth = GrowthRate(NumClasses,M(Region,Subarea), &
                                       TimeStep,                     &
                                       Dble(GrowthPeriod),           &
                                       Transition,                   &
                                       PopVector(:,Region,Subarea),  &
                                       MeatWeight0)
        NewPointer%EBmsMT = SubareaSummary(Region,Subarea)%EBmsMT
        NewPointer%Region = Region
        NewPointer%Subarea = Subarea
        if (Head%Growth < NewPointer%Growth) then 
            NewPointer%Link => Head
            Head => NewPointer
        else
           Do
             if (.NOT. Associated(ListPointer%Link)) then !if end of list
                ListPointer%Link => NewPointer
                Nullify(NewPointer%Link)
                Exit
             else
                OldPointer => ListPointer
                ListPointer => ListPointer%Link  !move one down on list
                if (ListPointer%Growth < NewPointer%Growth) then
                   NewPointer%Link => ListPointer
                   OldPointer%Link => NewPointer
                   Exit
                endif !listpointer%growth...
             endif !not associated...
           EndDo
         endif !Head%Growth...
     endif !Subarea == 1...
  Enddo !Subarea
  if (Region == 1) then
    ListHeadMA => Head
  else
    ListHeadGB => Head
  endif
Enddo !Region
ENDSUBROUTINE !CreateLinkedList

SUBROUTINE PrintLinkedList(Head)
Type(DataNode), Pointer :: ListPointer,Head
Integer :: Region

  ListPointer => Head
  Do
   if (.NOT. Associated(ListPointer)) then !if end of list
      EXIT
   else
     Print *,ListPointer%Region,ListPointer%Subarea,ListPointer%Growth,ListPointer%EBmsMT,ListPointer%PctEBms,ListPointer%cdf
     ListPointer => ListPointer%Link
   endif
  EndDo
Read *
ENDSUBROUTINE

SUBROUTINE ComputeCumBms(Region)
Integer, Intent(In) :: Region
Type(DataNode), Pointer :: Listpointer, Head
Real*8 :: TotalBms, CumPctBms,PctBms

if (Region == 1) then
  Head => ListheadMA
else
  Head => ListheadGB
endif
TotalBms = Dble(0)
PctBms = Dble(0)
CumPctBms = Dble(0)
Listpointer => Head
Do 
  if (.NOT. Associated(ListPointer)) then !if end of list
    EXIT
  else
    TotalBms = TotalBms + ListPointer%EBmsMT
    ListPointer => ListPointer%Link
  endif
EndDo
ListPointer => Head
Do   
  if (.NOT. Associated(ListPointer)) then !if end of list
    EXIT
  else
    ListPointer%PctEBms = ListPointer%EBmsMT/TotalBms
    SubareaSummary(ListPointer%Region,ListPointer%Subarea)%PctEBms = ListPointer%PctEBms
    CumPctBms = CumPctBms + ListPointer%PctEBms
    ListPointer%cdf = CumPctBms
    ListPointer => ListPointer%Link
  endif
EndDo
ENDSUBROUTINE !ComputeCumBms

SUBROUTINE DestroyList(Head)
Type(DataNode), Pointer :: Listpointer, OldListPointer,Head

Do
  if (.NOT. Associated(Head%Link)) then
    EXIT
  else
    ListPointer => Head
    Do   
      if (.NOT. Associated(ListPointer%Link)) then !if at next to last node on list
        Deallocate(ListPointer)
        Nullify(OldListPointer%Link)
        EXIT
      else
        OldListPointer => ListPointer
        ListPointer => ListPointer%Link
      endif
    EndDo
  endif
EndDo

ENDSUBROUTINE ! DestroyList
 

Real*8 FUNCTION EstimatedF(Region,Subarea,EBms)
Real*8, Parameter :: Del = 1
Integer, Parameter :: Delmarva = 1
Real*8, Parameter :: NYB = 1
Real*8, Parameter :: Slope = 0.000010
Real*8, Parameter :: MaxF = 1.5
Integer,Intent(In) :: Subarea,Region
Real*8, Intent(In) :: EBms
Real*8 :: TempF
if (Region == 2) then
  TempF = Slope*EBms
elseif (Subarea <= Delmarva) then
  TempF = Slope*Del*EBms
else
  TempF = Slope*NYB*Ebms
endif
!slope
!if ((Region == 2) .AND. (Subarea == 7) .AND. (Year == 2)) then
!  TempF = Slope*EBms*0.8
!endif
if (TempF <= MaxF) then
  EstimatedF = TempF
else
  EstimatedF = MaxF
endif
!print *,EstimatedF

ENDFUNCTION !estimated F






SUBROUTINE CalcFreeF(F,Closed,Harvest,DasTarget)
Integer, Parameter :: tol = 200
Integer, Parameter :: maxcounter = 99
Real*8, Intent(In) :: DasTarget
Logical, Dimension(1:NumRegions,1:NumSubareas),Intent(InOut) :: Closed,Harvest
Real*8, Intent(InOut) :: F(1:NumRegions,1:NumSubAreas)
Real*8 :: TempF(1:NumRegions,1:NumSubareas)
Real*8 :: TempOpDAS
Real*8 :: TempClDAS,TempDAS(1:NumRegions,1:NumSubareas)
Real*8 :: NewLPUE(1:NumRegions,1:NumSubareas)
Real*8 :: DredgeTime
Real*8 :: DredgeArea
Real*8 :: ProjBmsMT(1:NumRegions,1:NumSubareas),ProjBms,Fudge,AvDAS,AvLPUE,LPUE(1:NumRegions,1:NumSubareas)
Integer :: Region,Subarea,Counter
Logical :: OpenArea(1:NumRegions,1:NumSubareas)
Type(PopSummaryRecord) :: PopRec


!if (FixedStart .AND. (year < 5)) then
!  Closed(1,4) = .TRUE.
!  Closed(2,2) = .TRUE.
!endif
Do Region = 1,NumRegions
 Do Subarea = 1,NumSubareas
   PopRec = SubareaSummary(Region,Subarea)
   if (Closed(Region,Subarea)) then
     TempF(Region,Subarea) = 0
     OpenArea(Region,Subarea) = .False.
   elseif (Harvest(Region,Subarea)) then
     TempF(Region,Subarea) = F(Region,Subarea)
     OpenArea(Region,Subarea) = .False.
   else
     CALL CalcLPUE(PopRec%AvEWt,ProjBms,LPUE(Region,Subarea),DredgeTime,DredgeArea)
     TempF(Region,Subarea) = DasTarget*EstimatedF(Region,Subarea,PopRec%LPUE)/Dble(26000)
     !ProjBms = 0.5*(PopRec%Bms+PopRec%EBms)*exp(0.5*(PopRec%Growth-TempF(Region,Subarea)))
     !TempF(Region,Subarea) = DasTarget*EstimatedF(Region,Subarea,ProjBms)/Dble(26000)
     OpenArea(Region,Subarea) = .True.
   endif
  EndDo
EndDo
Counter = 1
Do
 TempOpDAS = Dble(0)
 TempClDas = Dble(0)
  Do Region = 1, NumRegions
    Do Subarea = 1,NumSubareas
     PopRec = SubareaSummary(Region,Subarea)
     if (PopRec%EBms > 1) then
       ProjBms = PopRec%EBms*exp(0.5*(PopRec%Growth-TempF(Region,Subarea)))
       ProjBmsMT(Region,Subarea) = PopRec%EBmsMT*exp(0.5*(PopRec%Growth-TempF(Region,Subarea)))
       TempDAS(Region,Subarea) = Dble(2)*ProjBmsMT(Region,Subarea)*TempF(Region,Subarea)*LPUE(Region,Subarea)/PopRec%ebms
       if (OpenArea(Region,Subarea)) then
         TempOpDAS = TempOpDAS + TempDAS(Region,Subarea)
       else
         TempClDAS = TempClDAS + TempDAS(Region,Subarea)
       endif
     endif
    EndDo
 EndDo
 if ((Abs(TempOpDAS + TempClDAS - DASTARGET) < tol) .OR. (Counter >= MaxCounter)) then
   if (Counter > 10) then
      Print *,"Counter=",Counter
   endif
   EXIT
 else
   Counter = Counter+1
   Do Region = 1,NumRegions
     Do SubArea = 1,NumSubareas
       if (OpenArea(Region,Subarea)) then
         TempF(Region,Subarea) = TempF(Region,Subarea)*(DASTarget - TempClDAS)/TempOpDAS
       endif
      EndDo
    EndDo
 endif
EndDo
Do Region = 1,NumRegions
  Do Subarea = 1,NumSubareas
    if (TempDAS(Region,Subarea) > 10) then
        TempF(Region,Subarea) = TempF(Region,Subarea)*(1.0557 +  0.0*SubareaSummary(Region,Subarea)%Growth)
    endif
  Enddo
Enddo
F = TempF


       !ProjBms = PopRec%EBms*exp(PopRec%Growth-TempF(Region,Subarea))
       !CALL CalcLPUE(PopRec%AvEWt,ProjBms,TempLPUE,DredgeTime,DredgeArea)
       !NewLPUE(Region,Subarea) = TempLPUE
       !ProjBms = PopRec%EBmsMT*exp(PopRec%Growth-TempF(Region,Subarea)+0.1)
       !AvDAS = safesq(PopRec%EBmsMT/PopRec%LPUE*ProjBms/NewLPUE(REgion,Subarea))*Dble(2204) !compute DAS required to exert unit F
       !Temp = TempF(Region,Subarea)*AvDAS

ENDSUBROUTINE !calcfreef


SUBROUTINE CalcFreeFF(F,Closed,Harvest,TargetF)
Integer, Parameter :: tol = 0.01
Integer, Parameter :: maxcounter = 99
Real*8, Parameter :: MAFweight = 2  !factor to weight MAB in fleet dynamics model
Real*8, Intent(In) :: TargetF
Logical, Dimension(1:NumRegions,1:NumSubareas),Intent(InOut) :: Closed,Harvest
Real*8, Intent(InOut) :: F(1:NumRegions,1:NumSubAreas)
Real*8 :: TempF(1:NumRegions,1:NumSubareas),FSum,BmsSum,OpenF
Real*8 :: ProjBmsMT(1:NumRegions,1:NumSubareas),ProjBms,Fudge,AvDAS,AvLPUE,LPUE(1:NumRegions,1:NumSubareas)
Integer :: Region,Subarea,Counter
Logical :: OpenArea(1:NumRegions,1:NumSubareas)
Type(PopSummaryRecord) :: PopRec

!print *,'*',year
Do Region = 1,NumRegions
  Do Subarea = 1,Numsubareas
    if ((Region == 1) .AND. (Subarea > MASubareas)) then
      F(Region,Subarea) = LowF
    elseif (Region == 1) then
        !if ((year == 1) .AND. (SubArea == Access1) .AND. (AccessF2 >= 0)) then
        !   Harvest(Region,Subarea) = .TRUE.
         !  F(Region,Subarea) = AccessF2
        !elseif ((year==1)  .AND. (Subarea == Access2) .AND. (Access2F2 >= 0)) then
         !  Harvest(Region,Subarea) = .TRUE.
          ! F(Region,Subarea) = Access2F2
        !if ((year == 2) .AND. (SubArea == Access1) .AND. (AccessF3 >= 0)) then
         !  Harvest(Region,Subarea) = .TRUE.
          ! F(Region,Subarea) = AccessF3
        !elseif ((year==2)  .AND. (Subarea == Access2) .AND. (Access2F3 >= 0)) then
         !  Harvest(Region,Subarea) = .TRUE.
          ! F(Region,Subarea) = Access2F3
        !elseif ((year == 3) .AND. (SubArea == Access2) .AND. (Access2F4 >= 0)) then
         !  Harvest(Region,Subarea) = .TRUE.
          ! F(Region,Subarea) = Access2F4
        !elseif ((year == 3) .AND. (SubArea == Access1) .AND. (AccessF4 >= 0)) then
         !  Harvest(Region,Subarea) = .TRUE.
          ! F(Region,Subarea) = AccessF4
       ! elseif ((year == 4) .AND. (SubArea == Access2) .AND. (Access2F5 >= 0)) then
        !   Harvest(Region,Subarea) = .TRUE.
         !  F(Region,Subarea) = Access2F5
        !elseif ((year == 5) .AND. (F(Region,Subarea) <= 0) .AND. ((Subarea == Access1) .OR. (Subarea == Access2))) then
         !  Harvest(Region,Subarea) = .FALSE.
        !endif
    endif
  Enddo
Enddo
Do Region = 1,NumRegions
 Do Subarea = 1,NumSubareas
   PopRec = SubareaSummary(Region,Subarea)
   if (Closed(Region,Subarea)) then
     TempF(Region,Subarea) = LowF
     OpenArea(Region,Subarea) = .False.
   elseif (Harvest(Region,Subarea)) then
     TempF(Region,Subarea) = F(Region,Subarea)
     OpenArea(Region,Subarea) = .False.
   else
     TempF(Region,Subarea) = TargetF*EstimatedF(Region,Subarea,PopRec%LPUE)*4
     OpenArea(Region,Subarea) = .True.
   endif
  EndDo
EndDo
  FSum = Dble(0)
  BmsSum = Dble(0)
  Do Region = 1, NumRegions
    Do Subarea = 1,NumSubareas
     PopRec = SubareaSummary(Region,Subarea)
        if (OpenArea(Region,Subarea)) then
          FSum = FSum + TempF(Region,Subarea)*PopRec%EBmsMT*(1+IncidentalM(Region,Subarea))
          BmsSum = BmsSum + PopRec%EBmsMT
        endif
    EndDo
 EndDo
 if (BmsSum > eps) then
   OpenF = FSum/BmsSum
 else
   OpenF = TargetF
 endif 
  FSum = Dble(0)
  BmsSum = Dble(0)
   Counter = Counter+1
   Do Region = 1,NumRegions
     Do SubArea = 1,NumSubareas
       if (OpenArea(Region,Subarea)) then
         TempF(Region,Subarea) = TempF(Region,Subarea)*TargetF*(1+IncidentalM(Region,Subarea))/OpenF
         FSum = FSum + TempF(Region,Subarea)*PopRec%EBmsMT
         BmsSum = BmsSum + PopRec%EBmsMT
       endif
      EndDo
    EndDo
F = TempF
if (runcount == 1) then
  print *, year,1,F(1,:)
  print *, year, 2,F(2,:)
endif
!Print *,Year,'f = ',TempF
!Read *
ENDSUBROUTINE !calcfreef


SUBROUTINE CalcFreeFST(F)
Real*8, Intent(Out) :: F(1:NumRegions,1:NumSubAreas)
Real*8 :: TempF(1:NumRegions,1:NumSubareas), TempOpDAS, TempClDAS, Temp,       &
        NewLPUE(1:NumRegions,1:NumSubareas), DredgeTime, DredgeArea, ProjBms,  &
        Fudge,AvDAS,AvLPUE,TempLPUE
Integer :: Region,Subarea
Logical :: OpenArea(1:NumRegions,1:NumSubareas)
Type(PopSummaryRecord) :: PopRec

TempOpDAS = Dble(0)
TempClDas = Dble(0)
Do Region = 1,NumRegions
 Do Subarea = 1,NumSubareas
   PopRec = SubareaSummary(Region,Subarea)
   if ((Region == 2) .AND. (Subarea > 8)) then
     TempF(Region,Subarea) = 0
     OpenArea(Region,Subarea) = .False.
   elseif ((Region == 1) .AND. (Subarea >= 5) .AND. (Subarea <= 6)) then
     TempF(Region,Subarea) = 0.2
     OpenArea(Region,Subarea) = .False.
   elseif ((Region == 1) .AND. (Subarea == 1)) then
     TempF(Region,Subarea) = 0.2
     OpenArea(Region,Subarea) = .False.
   else
     TempF(Region,Subarea) = EstimatedF(Region,Subarea,PopRec%EBms) 
     OpenArea(Region,Subarea) = .True.   
   endif
   if (PopRec%EBms > 1) then
      ProjBms = PopRec%EBms*exp(PopRec%Growth-TempF(Region,Subarea))
      CALL CalcLPUE(PopRec%AvEWt,ProjBms,TempLPUE,DredgeTime,DredgeArea)
      NewLPUE(Region,Subarea) = TempLPUE
      ProjBms = PopRec%EBmsMT*exp(PopRec%Growth-TempF(Region,Subarea)+0.1)
      AvDAS = safesq(PopRec%EBmsMT/PopRec%LPUE*ProjBms/NewLPUE(REgion,Subarea))
      Temp = TempF(Region,Subarea)*Dble(2204)*AvDAS
      if (OpenArea(Region,Subarea)) then
        TempOpDAS = TempOpDAS + Temp
      else
        TempClDAS = TempClDAS + Temp
      endif
   endif
  EndDo
EndDo
Do Region = 1,NumRegions
  Do Subarea = 1,NumSubareas
      PopRec = SubareaSummary(Region,Subarea)
     if (OpenArea(Region,Subarea)) then
       AvLPUE = safesq(PopRec%LPUE*NewLPUE(Region,Subarea))
       if (AvLPUE > 1800) then
         Fudge = 1
       elseif (AvLPUE < 800) then
         Fudge = 1
       else
         Fudge = 1
       endif
       Fudge = Fudge*0.93*(NewLPUE(Region,Subarea)/PopRec%LPUE)
       F(Region,Subarea) = Fudge*TempF(Region,Subarea)*(DASTARGET(Year)-TempClDAS)/TempOpDAS
     else
       F(Region,Subarea) = TempF(Region,Subarea)
     endif
  EndDo
EndDO
ENDSUBROUTINE 

SUBROUTINE InitializeAreaStatus(Harvest,Closed,ShortTerm)

Logical, Intent(In) :: ShortTerm
Logical, Dimension(1:NumRegions,1:NumSubareas),Intent(Out) :: Harvest,Closed
Integer :: Region,Subarea

Harvest = .False.
Closed = .False.
if (Closures) then
 Do Region = 1,NumRegions
  Do Subarea = 1,NumSubareas
    if ((Region == 1) .AND. (Subarea > MASubareas)) then 
      Closed(Region,Subarea) = .True.
    elseif ((Region == FirstClsdReg) .AND. (SubArea == FirstClsdSub) .AND. Year <= 1 .AND. Year >= 0) then
      Closed(Region,Subarea) = .TRUE.
    elseif ((Region == 2) .AND. (Subarea <= Opnum-1)) then
      Harvest(Region,Subarea) = .True.
    elseif ((Year <= 4) .AND. ((Region == 1) .AND. ((Subarea == Access2) .OR. (Subarea == Access3)))) then 
      Harvest(Region,Subarea) = .True.
    elseif ((Year >= 2) .AND. (Year <= 3) .AND. (Region == FirstClsdReg) .AND. (Subarea == FirstClsdSub)) then
      Harvest(Region,Subarea) = .True.
    endif
  enddo
 EndDo
endif


ENDSUBROUTINE

SUBROUTINE CalcF(F,NumRegions,modyear,HighF,LowF,HighFlag,LastFished,YearsClosed,PopVector,RotStrategy,year)
Integer, Intent(In) :: NumRegions,modyear,RotStrategy,year
Integer, Intent(InOut) :: YearsClosed(1:NumRegions,1:NumSubareas),LastFished(1:NumRegions,1:NumSubareas)
Real*8, Intent(InOut) :: F(1:NumRegions,1:NumSubareas,1:NumYears+1)
Logical, Intent(In) :: HighFlag(1:NumRegions,1:NumSubareas)
Real*8, Intent(In) :: PopVector(1:NumClasses,0:NumRegions,0:NumSubareas),HighF,LowF
Logical, Dimension(1:NumRegions,1:NumSubareas) :: Harvest,Closed
Real*8 :: ModDASTarget,ModFTarget
Integer :: Region,Subarea,YearP1
!!!!  Calls approprate strategy for calculating F next year
! 1 = constant f, 2 = fixed rotation, 3 = adaptive rotation

if (RotStrategy == 2) then
  CALL CalcFixedF(NumRegions,LowF,F(:,:,year+1))
elseif (RotStrategy == 1) then
  CALL FixedRotation(F,NumRegions,year)
elseif (RotStrategy == 3) then
  CALL CalcRotF3(F(:,:,year+1),NumRegions,YearsClosed,PopVector,HighF,LowF) !adaptive rotation a la Trevor
elseif (RotStrategy == 4) then  
  CALL CalcRotF4(F(:,:,year+1),NumRegions,YearsClosed,PopVector,HighF,LowF) !adaptive rotation
elseif (((RotStrategy == -2) .OR. (RotStrategy == -22)).AND. (Year >= 2)) then !DAS target strategies
   CALL InitializeAreaStatus(Harvest,Closed,.FALSE.)
  if (Year == 3) then
    ModDASTarget = DASTarget(Year+1)
  else
    ModDASTarget = DASTarget(Year+1)
  endif
  if (Year == 3) then
     F(1,NewClosure,year+1) = NewClF1
  elseif (Year == 4) then
     F(1,NewClosure,year+1) = NewClF2
  elseif (Year == 5) then
     F(1,NewClosure,year+ 1) = NewClF3
  endif
  if (Access1 > 0) then !if controlled access area 
    if (Year == 1) then
      F(1,Access1,year+1) =AccessF1
      if (Access2 > 0) then
         F(1,Access2,year+1) = AccessF1
      endif
    elseif ((Year == 2) .and. (AccessF2 >=0)) then
      F(1,Access1,year+1) = AccessF2
      if (Access2 > 0) then
         F(1,Access2,year+1) = AccessF2
      endif
    elseif ((Year == 3) .AND. (AccessF3 >= 0)) then
      F(1,Access1,year+1) = AccessF3
      if (Access2 > 0) then
         F(1,Access2,year+1) = AccessF3
      endif
    elseif ((Year == 4) .AND. (AccessF4 >= 0)) then
           F(1,Access1,year+1) = AccessF4
      if (Access2 > 0) then
         F(1,Access2,year+1) = AccessF4
      endif
    elseif ((Year == 5) .AND. (AccessF5 >= 0)) then
      F(1,Access2,year+1) = AccessF5
    endif
  endif
  CALL CalcFreeF(F(:,:,year+1),Closed,Harvest,ModDasTarget)
  CALL CalcGFClosedF(F(2,:,year+1),Year)
  elseif ((RotStrategy == -12) .AND. (Year >= 2)) then
    CALL InitializeAreaStatus(Harvest,Closed,.FALSE.)
    ModFTarget = TargetF*Adjust(year)
   if (Year == 2) then
      if (Access1 > 0) then 
        F(1,Access1,year+1) = AccessF1/(1+IncidentalM(1,Access1))
      endif
      if (FirstClsdReg > 0) then
        F(FirstClsdReg,FirstClsdSub,year+1) = 0
      endif
   elseif (Year == 3) then
      if (Access1 > 0) then
        F(1,Access1,year+1) = AccessF2/(1+IncidentalM(1,Access1))
      endif
         if (FirstClsdReg > 0) then
        F(FirstClsdReg,FirstClsdSub,year+1) = 0
      endif
   elseif (Year == 4) then
      if (AccessF3 >= 0) then
         F(1,Access1,year+1) = AccessF3/(1+IncidentalM(1,Access1))
      endif
      if (FirstClsdReg >0) then
         F(FirstClsdReg,FirstClsdSub,year+1) = NewClF1/(1+IncidentalM(FirstClsdReg,FirstClsdSub))
      endif
   elseif (Year == 5) then
      if (AccessF4 >= 0) then
        F(1,Access1,year+1) = AccessF4/(1+IncidentalM(1,Access1))
      endif
      if (FirstClsdReg > 0) then
        F(FirstClsdReg,FirstClsdSub,year+1) = NewClF2/(1+IncidentalM(FirstClsdReg,FirstClsdSub))
      endif
  elseif ((Year == 6) .AND. (FirstClsdReg > 0)) then
      F(FirstClsdReg,FirstClsdSub,year+1) = NewClF3/(1+IncidentalM(FirstClsdReg,FirstClsdSub))
  endif
  CALL CalcFreeFF(F(:,:,year+1),Closed,Harvest,ModFTarget)
  CALL CalcGFClosedF(F(2,:,year+1),Year)
  elseif (RotStrategy == -3) then
  CALL InitializeAreaStatus(Harvest,Closed,.FALSE.)
  if (Year == 3) then
    ModDASTarget = 16*DASTarget(Year+1)/Dble(16)
  else
    ModDASTarget = DASTarget(Year+1)
  endif
    CALL CalcFreeF(F(:,:,year+1),Closed,Harvest,ModDasTarget)
  elseif (RotStrategy == -13) then
    CALL InitializeAreaStatus(Harvest,Closed,.FALSE.)
    CALL CalcFreeFF(F(:,:,year+1),Closed,Harvest,TargetF)
  elseif (RotStrategy == 13) then
     Yearp1 = min(year+1,NumYears)
     Harvest = Access(:,:,Yearp1)
     Do Region = 1,NumRegions    
        Do Subarea = 1,NumSubareas
          if (Access(Region,Subarea,Yearp1)) then
              F(Region,Subarea,Yearp1) = AccessF(Region,Subarea,Yearp1)/(1+IncidentalM(Region,Subarea))
          endif
        EndDo
     EndDo 
    CALL CalcRotFreeF3 (F(:,:,year+1),NumRegions,YearsClosed,PopVector,HighF,LowF,Closed,Harvest,TargetF,Year)
elseif ((RotStrategy == 23) .AND. (Year >= 2)) then
  CALL CalcRotFreeF3(F(:,:,year+1),NumRegions,YearsClosed,PopVector,HighF,LowF,Closed,Harvest,TargetF,Year)
elseif ((RotStrategy == 25) .AND. (Year >= 2)) then
  CALL CalcRotFreeF3(F(:,:,year+1),NumRegions,YearsClosed,PopVector,HighF,LowF,Closed,Harvest,TargetF,Year)
elseif (RotStrategy == 14) then
  CALL CalcRotFreeF4(F(:,:,year+1),NumRegions,YearsClosed,PopVector,HighF,LowF,Closed,Harvest,TargetF)
elseif ((RotStrategy == 24) .AND. (Year >= 2)) then
  CALL InitializeAreaStatus(Harvest,Closed,.FALSE.)
  Continue
  CALL CalcRotFreeF4(F(:,:,year+1),NumRegions,YearsClosed,PopVector,HighF,LowF,Closed,Harvest,TargetF)
endif
Do Region = 1,NumRegions
  Do Subarea = 1,NumSubareas
    if (Closed(Region,Subarea)) then
       SubareaSummary(Region,Subarea)%PctClosed = Dble(1)
    else
       SubareaSummary(Region,Subarea)%PctClosed = Dble(0)
    endif
  enddo
Enddo
Continue
!Print *,year+1,EstimatedCatch(F(:,:,year+1))
!Read *
!if ((gfoption == 9) .AND. (Year >=2)) then  !adaptive reopening of closed areas
!  CALL CalcGFF(F(:,:,year+1),year+1)
!endif
ENDSUBROUTINE !CalcF


Real*8 FUNCTION EstimatedCatch(F)
Real*8, Intent(In) :: F(1:NumRegions,1:NumSubareas)
Real*8 :: Catch
Integer :: Region,Subarea

Catch = 0
Do Region = 1,NumRegions
  Do Subarea = 1,NumSubareas
    Catch = Catch + F(Region,Subarea)*SubareaSummary(Region,Subarea)%EBmsMT
  Enddo
Enddo
EstimatedCatch = Catch

ENDFUNCTION

 
SUBROUTINE CalcGFF(F,newyear)
Real*8, Parameter :: TargetCatch = 22600
Real*8, Parameter :: MaxF = 0.2
Real*8, Intent(InOut) :: F(1:NumRegions,1:NumSubareas)
Integer :: Region,Subarea,newyear
Real*8 :: OpenCatch,TotalCatch,Catch1,TempF
Logical :: Reopened(9:14)

Do Subarea = 9,14
  F(2,Subarea) = Dble(0)
  Reopened(Subarea) = .FALSE.
Enddo
if (mod(newyear,4) == 3) then
  Reopened(11) = .TRUE.
  Reopened(13) = .TRUE.
else
  Reopened(9) = .TRUE.
endif
OpenCatch = EstimatedCatch(F)
Catch1 = 0
Do Subarea = 9,14
  if (Reopened(Subarea)) then
    Catch1 = Catch1 + 0.1*SubareaSummary(2,Subarea)%EBmsMT
  endif
Enddo
if ((OpenCatch >= TargetCatch) .OR. (Catch1 == 0)) then
  TempF = Dble(0)
else
  TempF = 0.1*(TargetCatch - OpenCatch)/Catch1
  if (mod(newyear,4) == 3) then
     TempF = TempF*1.35
     if (TempF > 4*MaxF) then
       TempF = 4*MaxF
     endif
  elseif (TempF > 1.33*MaxF) then
       TempF = 1.33*MaxF
  endif
endif
Do Subarea = 9,14
  if (Reopened(Subarea)) then
    F(2,Subarea) = TempF
!    Print *,Year+1,Subarea,TempF
  endif
Enddo
ENDSUBROUTINE

SUBROUTINE CalcFixedF(NumRegions,LowF,F)
Integer, Intent(In) :: NumRegions
Real*8, Intent(In) :: LowF
Real*8, Intent(Out) :: F(1:NumRegions,1:NumSubareas)
Integer :: Region,SubArea

Do Region = 1,NumRegions
  Do Subarea = 1,NumSubareas
    F(Region,Subarea) = LowF/(1+IncidentalM(Region,Subarea))
  Enddo
Enddo

ENDSUBROUTINE

SUBROUTINE CalcGFClosedF(F,Year)
Real*8, Intent(InOut) :: F(1:NumSubareas)
Integer, Intent(In) :: Year
Integer :: Subarea

Do Subarea = 1,Opnum-1
   CALL CalcGroundFishClosedAreaF(F(Subarea),Subarea,Year)
Enddo

ENDSUBROUTINE





SUBROUTINE CalcGroundFishClosedAreaF(F,Subarea,SimYear)
Real*8, Parameter :: GFTargetF = 0.2
Real*8, Parameter :: FirstYearTarget = 0.32
Real*8, Intent(InOut) :: F
Integer, Intent(In) :: Subarea,SimYear
Integer :: CalendarYear,Year


CalendarYear = SimYear + StartingYear
year = min(simyear,NumYears-1)
if (gfoption == 9) then  !Fs are specified in input file
  if (Access(2,Subarea,year+1)) then
     F = AccessF(2,Subarea,year+1)/(1+IncidentalM(2,Subarea))
  else
     F = 0
  endif
elseif (gfoption == 0) then
    F = Dble(0)
elseif (gfoption == 1) then !access areas only
  if (mod(Subarea,2) == 0) then     
     F = GFTargetF/(1+IncidentalM(2,Subarea))
  else
     F = Dble(0)
  endif
elseif (gfoption == 2) then !closed area IIs only
  if (Subarea == 3) then
     F = 0.2/(1+IncidentalM(2,Subarea))
  else
     F = Dble(0)
  endif
elseif (gfoption == 3) then  !4 year rotation
  if (mod(CalendarYear,4) == 1) then
     if (Subarea == 4) then
        F = 0.2/(1+IncidentalM(2,Subarea))
     elseif (Subarea == 6) then
        F = 0.24/(1+IncidentalM(2,Subarea))
     else
        F = Dble(0)
     endif
  else
    if (Subarea == 4) then
       F = 0.2/(1+IncidentalM(2,Subarea))
    else
       F = Dble(0)
    endif
  endif
elseif (gfoption == 4)  then !constant F all areas
   F = 0.24/ (1+IncidentalM(2,Subarea))
elseif ((gfoption == 5) .OR. (gfoption == 6)) then !2/3 rotation
 
  if (CalendarYear == 2012) then
     if (Subarea == 4) then
       F = 0.191
     elseif (Subarea == 2) then
       F = 0.31
     elseif (Subarea == 6) then
       F = 0.15
     endif
  elseif (CalendarYear == 2013) then
     if (Subarea == 2) then
       F = 0.55
     elseif (Subarea == 4) then
       F = 0.55
     elseif (Subarea == 6) then
       F = 0.0
     else
       F = 0
     endif
  elseif (CalendarYear == 2014) then
     if (Subarea == 4) then
       F = 0.55
     elseif (Subarea == 6) then
       F = 0.69
     else
       F = 0
    endif
  elseif (mod(CalendarYear,3) == 1) then
     if (Subarea == 4) then  !closed area IIS
        F = 0.2/(1+IncidentalM(2,Subarea))
     elseif (Subarea == 6) then  !NLS access
        F = 0.2/(1+IncidentalM(2,Subarea))
     else
        F = Dble(0)
     endif
  elseif (mod(CalendarYear,3) == 0) then  !cl1 and nls
    if ((Subarea == 2)) then 

          F = 0.2/(1+IncidentalM(2,Subarea))

    elseif (Subarea == 6) then

         F = 0.2/(1+IncidentalM(2,Subarea))
    else
       F = Dble(0)
    endif
  else
    if (Subarea == 2) then
        F = 0.2/(1+IncidentalM(2,Subarea))
    elseif (Subarea == 4) then !cl1 and cl2
       F = 0.2/(1+IncidentalM(2,Subarea))
    else
       F = Dble(0)
    endif
  endif 
else
  Print *,"Error in groundfish option!!!"
  F = 0.2/(1+IncidentalM(2,Subarea))
endif
!if (gfoption == 6 .AND. simyear == 1) then
!  if (subarea == 1) then
!     F = 0.32/(1+IncidentalM(2,Subarea))
!  elseif (subarea == 5) then
!     F = 0.25/(1+IncidentalM(2,Subarea))
!  elseif (subarea == 3) then
!     F = 0.2/(1+IncidentalM(2,Subarea))
!  endif
!endif
  

ENDSUBROUTINE !CalcGroundfishclosedareaF
       
SUBROUTINE CalcRotFreeF3(F,NumRegions,YearsClosed,PopVector,HighF,LowF,Closed,HarvestArea,TargetF,Year)
!fishing mortality subroutine for adaptive closures with fixed closure length
Integer, Intent(In) :: NumRegions,Year
!Real*8, Intent(In) :: AvWt(1:NumRegions,1:NumSubareas)
Real*8, Intent(In) :: PopVector(1:NumClasses,0:NumRegions,0:NumSubareas),HighF,LowF,TargetF
Integer, Intent(InOut) :: YearsClosed(1:NumRegions,1:NumSubareas)
Real*8, Intent(Out) :: F(1:NumRegions,1:NumSubareas)
Logical, Dimension(1:NumRegions,1:NumSubareas),Intent(InOut) :: HarvestArea,Closed
Real*8 :: PctBmsClsd,SecondGrowth,MinGrowth,Growth,Period,SumF,TotalGrowth,AvgGrowth,OpenBiomass,&
          Biomass(1:NumRegions,1:NumSubareas),AvgBiomass,ClosedArea
Integer :: MinRegion, MinSubArea,SecondRegion,SecondSubarea,Region,SubArea,OpenRegions,ClosedRegions
Type(DataNode), Pointer :: Head, ListPointer


Do Region = 1,NumRegions !Determine closure and harvest areas
  PctBmsClsd = Dble(0)
  Do SubArea = 1,NumSubAreas
    !HarvestArea(Region,Subarea) = .FALSE.
    Closed(Region,Subarea) = .FALSE.    
    Growth = GrowthRate(NumClasses,M(Region,Subarea),TimeStep,Dble(GrowthPeriod),Transition,PopVector(:,Region,Subarea),MeatWeight0)
  enddo
 Continue
  Do Subarea = 1,NumSubareas
    if ((Region == 2) .AND. (Subarea < Opnum)) then !if groundfish closed area
       CALL CalcGroundFishClosedAreaF(F(2,Subarea),Subarea,Year)
       HarvestArea(Region,Subarea) = .TRUE.
       YearsClosed(Region,Subarea) = -2
    !elseif ((Access1 > 0) .AND. (Region == 1) .AND. (Subarea == Access1) .AND. (Year == StartYear)) then
    !     HarvestArea(Region,Subarea) = .TRUE.
    !     YearsClosed(Region,Subarea) = -InitialHarvestYears
    !     TotalF(Region,Subarea) = 6*HighF      
    !     F(Region,Subarea) = AccessF1
    elseif (HarvestArea(Region,Subarea)) then
       !Print *,F(Region,Subarea)
       Continue
    elseif ((Region == 1) .AND. (Subarea == Access1) .AND. (InitialManagement >=0) .AND. (Year <= 5)) then
       if ((Year ==4) .AND. (AccessF5 >= 0)) then
         F(Region,Subarea) =AccessF5
         HarvestArea(Region,Subarea) = .TRUE.
         YearsClosed(Region,Subarea) = -1  
         !print *,Year,subarea,F(Region,Subarea)  
       elseif ((Year == 3) .AND. (AccessF4 >= 0)) then
         F(Region,Subarea) = AccessF4
           HarvestArea(Region,Subarea) = .TRUE.
         YearsClosed(Region,Subarea) = -1  
         !print *,Year,subarea,F(Region,Subarea)
       elseif ((Year == 2) .AND. (AccessF3 >= 0)) then
         F(Region,Subarea) = AccessF3
         HarvestArea(Region,Subarea) = .TRUE.
         YearsClosed(Region,Subarea) = -1
          elseif (Year == 1) then
         F(Region,Subarea) = AccessF2
         HarvestArea(Region,Subarea) = .TRUE.
         YearsClosed(Region,Subarea) = -1
       elseif (Year ==2) then
         F(Region,Subarea) = AccessF1
         HarvestArea(Region,Subarea) = .TRUE.
         YearsClosed(Region,Subarea) = -1
       endif
    elseif ((Region == 1) .AND. (Subarea == Access2) .AND. (InitialManagement >=0) .AND. (Year <= 5)) then
       if ((Year == 4) .AND. (Access2F5 >= 0)) then
         F(Region,Subarea) = Access2F5
          HarvestArea(Region,Subarea) = .TRUE.
         YearsClosed(Region,Subarea) = -1
         !print *,Year,subarea,F(Region,Subarea)
       elseif ((Year == 3) .AND. (Access2F4 >= 0)) then
        F(Region,Subarea) = Access2F4
            HarvestArea(Region,Subarea) = .TRUE.
         YearsClosed(Region,Subarea) = -1
         !print *,Year,subarea,F(Region,Subarea)
       elseif ((Year == 2) .AND. (Access2F3 > 0)) then
         F(Region,Subarea) = Access2F3
         HarvestArea(Region,Subarea) = .TRUE.
         YearsClosed(Region,Subarea) = -1
          elseif (Year == 1) then
         F(Region,Subarea) = Access2F2
         HarvestArea(Region,Subarea) = .TRUE.
         YearsClosed(Region,Subarea) = -2
          elseif (Year == 0) then
         F(Region,Subarea) = Access2F1
         HarvestArea(Region,Subarea) = .TRUE.
         YearsClosed(Region,Subarea) = -3
       endif
    elseif ((Year == 0) .AND. (Subarea == FirstClsdSub)) then
       if (InitialManagement == 1) then
           F(Region,Subarea) = 0
           Closed(region,subarea) = .FALSE.
           YearsClosed(Region,Subarea) = -2
           HarvestArea(Region,Subarea) = .TRUE.
        else
           F(Region,Subarea) = 0
           Closed(region,subarea) = .TRUE.
           YearsClosed(Region,Subarea) = 1
           HarvestArea(Region,Subarea) = .FALSE.
        endif
    elseif ((Year == 1) .AND. (Region==SecondClsdReg) .AND. (Subarea == SecondClsdSub) .AND. InitialManagement >= 1) then
           F(Region,Subarea) = LowF 
           Closed(Region,Subarea) = .TRUE.
           YearsClosed(Region,Subarea) = 1
           HarvestArea(Region,Subarea) = .FALSE.
    elseif (YearsClosed(Region,Subarea) >= ClosureLength) then !if region closed for specified amount of time, then reopen it
           if (Region == FirstClsdReg .AND. Subarea == FirstClsdSub) then
              YearsClosed(Region,Subarea) = 2
           endif
           TotalF(Region,Subarea) = Fudge*Dble(YearsClosed(Region,Subarea)+3)*HighF/(1+IncidentalM(Region,Subarea))
           Bin(YearsClosed(Region,Subarea)) = Bin(YearsClosed(Region,Subarea)) + 1
           YearsClosed(Region,Subarea) = -2
           F(Region,Subarea) = 0.8*TotalF(Region,Subarea)/Dble(3)
           HarvestArea(Region,Subarea) = .TRUE.
              !Print *,Region,Subarea,F(Region,Subarea)
           if (Region == FirstClsdReg .AND. Subarea == FirstClsdSub) then
              F(Region,Subarea) = NewClF1
           endif
    elseif (YearsClosed(Region,Subarea) > 0)  then
         YearsClosed(Region,Subarea) = YearsClosed(Region,Subarea)+1
         F(Region,Subarea) = LowF
         PctBmsClsd = PctBmsClsd + SubareaSummary(Region,Subarea)%PctEBms
         Closed(Region,Subarea) = .TRUE.
    elseif (YearsClosed(Region,Subarea) < 0) then !if area presently harvest area
       HarvestArea(Region,Subarea) = .TRUE.
       YearsClosed(Region,Subarea) = YearsClosed(Region,Subarea) + 1
       if (Year > 4) then !(.NOT. ((Year <= 4) .AND. (Region == 1) .AND. ((SubArea == Access1) .OR. (Subarea == Access2) .OR. (Subarea == Access3)))) then   
          F(Region,Subarea) = (1.2+0.2*YearsClosed(Region,Subarea))*TotalF(Region,Subarea)/Dble(3)
       endif
    endif
  EndDo !subarea
  if ( .NOT. (FixedStart .AND. (Year < 5))) then
  if (Region == 1) then  
     Head => ListHeadMA
  else
     Head => ListHeadGB
  endif
  ListPointer => Head
  Do !go down list of subareas by order of growth rates and decide if an area should be closed
    Subarea = ListPointer%Subarea
    if ((YearsClosed(Region,Subarea) == 0) .AND. (.NOT. HarvestArea(Region,Subarea))) then !if area is currently open/unrestricted
         if (ListPointer%Growth < CritGrowth) then
            Closed(Region,Subarea) = .False.
         elseif ((PctBmsClsd + ListPointer%PctEBms > MaxPercentageClosed) .AND. (PctBmsClsd > eps)) then
            Closed(Region,Subarea) = .False.
         else
            F(Region,Subarea) = LowF
            Closed(Region,Subarea) = .True.
            YearsClosed(Region,Subarea) = 1
            PctBmsClsd = PctBmsClsd + ListPointer%PctEBms
            !Print *,Year,Region,Subarea,ListPointer%PctEBms,PctBmsClsd
         endif
    endif
    if (.NOT. Associated(ListPointer%Link)) then
       EXIT
    else
       ListPointer => ListPointer%Link
    endif
  EndDo 
  endif
EndDo !Region
CALL CalcFreeFF(F,Closed,HarvestArea,TargetF*Adjust(year))


ENDSUBROUTINE !CalcRotF




SUBROUTINE CalcRotFreeF4(F,NumRegions,YearsClosed,PopVector,HighF,LowF,Closed,HarvestArea,TargetF)

Real*8, Parameter :: FinalHarvestF = 0.48 !F in the last year of a harvest cycle
Real*8, Parameter :: HarvestMultiplier = 0.2
Integer, Intent(In) :: NumRegions
!Real*8, Intent(In) :: AvWt(1:NumRegions,1:NumSubareas)
Real*8, Intent(In) :: PopVector(1:NumClasses,0:NumRegions,0:NumSubareas),HighF,LowF,TargetF
Integer, Intent(InOut) :: YearsClosed(1:NumRegions,1:NumSubareas)
Real*8, Intent(Out) :: F(1:NumRegions,1:NumSubareas)
Logical, Dimension(1:NumRegions,1:NumSubareas),Intent(InOut) :: HarvestArea,Closed
Real*8 :: PctBmsClsd,SecondGrowth,MinGrowth,Growth,Period,SumF,TotalGrowth,AvgGrowth,OpenBiomass,&
          Biomass(1:NumRegions,1:NumSubareas),AvgBiomass,ClosedArea
Integer :: Region,SubArea
Type(DataNode), Pointer :: Head, ListPointer

Do Region = 1,NumRegions !Determine regions with minimum and 2nd min growth rates 
  PctBmsClsd = Dble(0)
  !Do SubArea = 1,NumSubAreas
  !  HarvestArea(Region,Subarea) = .FALSE.    
  !Enddo
  Continue
  Do Subarea = 1,NumSubareas
      Growth = GrowthRate(NumClasses,                  &
                          M(Region,Subarea),           &
                          TimeStep,                    &
                          Dble(GrowthPeriod),          &
                          Transition,                  &
                          PopVector(:,Region,Subarea), &
                          MeatWeight0)
    if ((Region == 2) .AND. (Subarea < OpNum)) then !if groundfish closed area
       CALL CalcGroundFishClosedAreaF(F(2,Subarea),Subarea,Year)
       HarvestArea(Region,Subarea) = .TRUE.
       Closed(Region,Subarea) = .FALSE.
       YearsClosed(Region,Subarea) = -2
    elseif (Region == 1 .AND. Subarea == Access1 .AND. AccessF1 > 1e-5 .AND. Year <= 2) then !HCspecial
       if (Year == 2) then
          F(1,Access1) = AccessF1   !    *1.2
          HarvestArea(Region,Subarea) = .TRUE.
          YearsClosed(Region,Subarea) = 0
          Closed(Region,Subarea) = .False.
       else
          HarvestArea(Region,Subarea) = .False.
          YearsClosed(Region,Subarea) = 0
          Closed(Region,Subarea) = .False.
       endif
    elseif (YearsClosed(Region,Subarea) < 0) then !if area presently harvest area
       HarvestArea(Region,Subarea) = .TRUE.
       Closed(Region,Subarea) = .FALSE.
       YearsClosed(Region,Subarea) = YearsClosed(Region,Subarea) + 1
       F(Region,Subarea) = (3*FinalHarvestF &
                         + 0.2*YearsClosed(Region,Subarea)*TotalF(Region,Subarea))/Dble(3)/(1+IncidentalM(Region,Subarea))
       if (etlong .and. year == 2 .and. subarea == 3) then
           YearsClosed(Region,Subarea) = -1
       elseif (etlong .and. year == 3 .and. subarea == 3) then
           F(Region,Subarea) = 0.47
       endif
     elseif (YearsClosed(Region,Subarea) > 0) then !if region is currently closed
       if (YearsClosed(Region,Subarea) < MinClosureTime) then
          YearsClosed(Region,Subarea) = YearsClosed(Region,Subarea)+1
          F(Region,Subarea) = LowF
          PctBmsClsd = PctBmsClsd + SubareaSummary(Region,Subarea)%PctEBms
          Closed(Region,Subarea) = .TRUE.
       else if ((Growth < CritGrowth2)                              &
               .OR. (YearsClosed(Region,Subarea) >= MaxClosureTime) &
               .OR. ((Year == 3) .AND. (Subarea == FirstClsdSub))   &
               .OR. ((Year == 6) .AND. (Subarea == SecondClsdSub))) then !if area ready for reopening
           TotalF(Region,Subarea) = Fudge*Dble(YearsClosed(Region,Subarea)+3)*HighF
           Bin(YearsClosed(Region,Subarea)) = Bin(YearsClosed(Region,Subarea)) + 1
           YearsClosed(Region,Subarea) = -2
           F(Region,Subarea) = 0.8*TotalF(Region,Subarea)/Dble(3)/(1+IncidentalM(Region,Subarea))
           HarvestArea(Region,Subarea) = .TRUE.
           Closed(Region,Subarea) = .FALSE.
           !ET special
           if (ETlong) then
           if (Year == 3 .AND. Subarea == FirstClsdSub) then
               F(1,Subarea) = 0.16/(1+IncidentalM(1,Subarea))
           endif
           endif
       else !if growth is still too fast for opening
            YearsClosed(Region,Subarea) = YearsClosed(Region,Subarea)+1
         F(Region,Subarea) = LowF
         PctBmsClsd = PctBmsClsd + SubareaSummary(Region,Subarea)%PctEBms
         Closed(Region,Subarea) = .TRUE.
       endif !growth < CritGrowth2
    endif
  !if (Closed(Region,Subarea) .AND. Subarea  <  7 .AND. Subarea > 4) then
  !   Continue
  !endif
  EndDo !subarea
  !!! Now calc F for currently open areas
 if (Region == 1) then  
     Head => ListHeadMA
  else
     Head => ListHeadGB
  endif
  ListPointer => Head
  Do !go down list of subareas by order of growth rates and decide if an area should be closed
    Subarea = ListPointer%Subarea
    if ((YearsClosed(Region,Subarea) == 0) .AND. (.NOT. HarvestArea(Region,Subarea))) then !if area is currently open/unrestricted
         if (ListPointer%Growth < CritGrowth) then
            Closed(Region,Subarea) = .False.
         elseif ((PctBmsClsd + ListPointer%PctEBms > MaxPercentageClosed) .AND. (PctBmsClsd > eps)) then
            Closed(Region,Subarea) = .False.
         else
            F(Region,Subarea) = LowF
            Closed(Region,Subarea) = .True.
            YearsClosed(Region,Subarea) = 1
            PctBmsClsd = PctBmsClsd + ListPointer%PctEBms
         endif
    endif
    if (.NOT. Associated(ListPointer%Link)) then
       EXIT
    else
       ListPointer => ListPointer%Link
    endif
  EndDo 
EndDo !Region
Continue
!SearchHere
ENDSUBROUTINE !CalcRotF


SUBROUTINE CalcRotF3(F,NumRegions,YearsClosed,PopVector,HighF,LowF)
Real*8, Parameter :: CritSize = 99999.0
Integer, Intent(In) :: NumRegions
!Real*8, Intent(In) :: AvWt(1:NumRegions,1:NumSubareas)
Real*8, Intent(In) :: PopVector(1:NumClasses,0:NumRegions,0:NumSubareas),HighF,LowF
Integer, Intent(InOut) :: YearsClosed(1:NumRegions,1:NumSubareas)
Real*8, Intent(Out) :: F(1:NumRegions,1:NumSubareas)
Logical,Dimension(1:NumRegions,1:NumSubareas) :: HarvestArea,Closed
Real*8 :: PctBmsClsd,SecondGrowth,MinGrowth,Growth,Period,SumF,TotalGrowth,AvgGrowth,OpenBiomass,&
          Biomass(1:NumRegions,1:NumSubareas),AvgBiomass,ClosedArea
Integer :: MinRegion, MinSubArea,SecondRegion,SecondSubarea,Region,SubArea,OpenRegions,ClosedRegions
Type(DataNode), Pointer :: Head, ListPointer

Do Region = 1,NumRegions !Determine regions with minimum and 2nd min growth rates 
  PctBmsClsd = Dble(0)
  Do SubArea = 1,NumSubAreas
    !HarvestArea(Region,Subarea) = .FALSE.    
    Growth = GrowthRate(NumClasses,M(Region,Subarea),TimeStep,Dble(GrowthPeriod),Transition,PopVector(:,Region,Subarea),MeatWeight0)
    if ((Region == 2) .AND. (Subarea < Opnum)) then !if groundfish closed area
       CALL CalcGroundFishClosedAreaF(F(2,Subarea),Subarea,Year)
    elseif (YearsClosed(Region,Subarea) > 0) then !if region is currently closed
      if (Growth < CritGrowth2) then !if area ready for reopening
         HarvestArea(Region,Subarea) = .TRUE.
         if (YearsClosed(Region,Subarea) >= 3) then
           TotalF(Region,Subarea) = Fudge*Dble(YearsClosed(Region,Subarea)+3)*HighF
           YearsClosed(Region,Subarea) = -2
           F(Region,Subarea) = 0.8*TotalF(Region,Subarea)/Dble(3)
         else
           TotalF(Region,Subarea) = Fudge*Dble(YearsClosed(Region,Subarea)*2)*HighF
           YearsClosed(Region,Subarea) = -YearsClosed(Region,Subarea)+1
           if (YearsClosed(Region,Subarea) == 0) then
             F(Region,Subarea) = TotalF(Region,Subarea)
           else
             F(Region,Subarea) = TotalF(Region,Subarea)/Dble(2)
             TotalF(Region,Subarea) = 1.5*TotalF(Region,Subarea)
           endif
         endif
       else !if growth is still too fast for opening
         YearsClosed(Region,Subarea) = YearsClosed(Region,Subarea)+1
         F(Region,Subarea) = LowF
         PctBmsClsd = PctBmsClsd + SubareaSummary(Region,Subarea)%PctEBms
       endif !growth < CritGrowth2
    elseif (YearsClosed(Region,Subarea) < 0) then !if area presently harvest area
       HarvestArea(Region,Subarea) = .TRUE.
       YearsClosed(Region,Subarea) = YearsClosed(Region,Subarea) + 1
       F(Region,Subarea) = (1.2+0.2*YearsClosed(Region,Subarea))*TotalF(Region,Subarea)/Dble(3)    
    endif
  EndDo !subarea
   if (Region == 1) then
     Head => ListHeadMA
  else
     Head => ListHeadGB
  endif
  ListPointer => Head
  Do !go down list of subareas by order of growth rates
    Subarea = ListPointer%Subarea
    if ((YearsClosed(Region,Subarea) == 0) .AND. (.NOT. HarvestArea(Region,Subarea))) then !if area is currently open/unrestricted
         if (ListPointer%Growth < CritGrowth) then
            F(Region,Subarea) = HighF
         elseif (PctBmsClsd + ListPointer%PctEBms > MaxPercentageClosed) then
            F(Region,Subarea) = HighF
         else
            F(Region,Subarea) = LowF
            YearsClosed(Region,Subarea) = 1
            Closed(Region,Subarea) = .TRUE.
            PctBmsClsd = PctBmsClsd + ListPointer%PctEBms
         endif
    endif
    if (.NOT. Associated(ListPointer%Link)) then
       EXIT
    else
       ListPointer => ListPointer%Link
    endif
  EndDo 
EndDo !Region

ENDSUBROUTINE !CalcRotF

SUBROUTINE CalcRotF4(F,NumRegions,YearsClosed,PopVector,HighF,LowF)
Real*8, Parameter :: CritSize = 99999.0
Integer, Intent(In) :: NumRegions
!Real*8, Intent(In) :: AvWt(1:NumRegions,1:NumSubareas)
Real*8, Intent(In) :: PopVector(1:NumClasses,0:NumRegions,0:NumSubareas),HighF,LowF
Integer, Intent(InOut) :: YearsClosed(1:NumRegions,1:NumSubareas)
Real*8, Intent(Out) :: F(1:NumRegions,1:NumSubareas)
Logical, Dimension(1:NumRegions,1:NumSubareas) :: HarvestArea,Closed
Real*8 :: PctBmsClsd,SecondGrowth,MinGrowth,Growth,Period,SumF,TotalGrowth,AvgGrowth,OpenBiomass,&
          Biomass(1:NumRegions,1:NumSubareas),AvgBiomass,ClosedArea
Integer :: MinRegion, MinSubArea,SecondRegion,SecondSubarea,Region,SubArea,OpenRegions,ClosedRegions
Type(DataNode), Pointer :: Head, ListPointer

Do Region = 1,NumRegions !Determine regions with minimum and 2nd min growth rates 
  PctBmsClsd = Dble(0)
  Do SubArea = 1,NumSubAreas
    HarvestArea(Region,Subarea) = .FALSE.    
    Growth = GrowthRate(NumClasses,M(Region,Subarea),TimeStep,Dble(GrowthPeriod),Transition,PopVector(:,Region,Subarea),MeatWeight0)
    if ((Region == 2) .AND. (Subarea < 9)) then !if groundfish closed area
       CALL CalcGroundFishClosedAreaF(F(2,Subarea),Subarea,Year)
    elseif (YearsClosed(Region,Subarea) > 0) then !if region is currently closed
       if (YearsClosed(Region,Subarea) < MinClosureTime) then
          YearsClosed(Region,Subarea) = YearsClosed(Region,Subarea)+1
          F(Region,Subarea) = LowF
          PctBmsClsd = PctBmsClsd + SubareaSummary(Region,Subarea)%PctEBms
       elseif ((Growth < CritGrowth2) .OR. (YearsClosed(Region,Subarea) >= MaxClosureTime)) then !if area ready for reopening
         HarvestArea(Region,Subarea) = .TRUE.
         TotalF(Region,Subarea) = Fudge*Dble(YearsClosed(Region,Subarea)+3)*HighF
         YearsClosed(Region,Subarea) = -2
         F(Region,Subarea) = 0.8*TotalF(Region,Subarea)/Dble(3)
       else !if growth is still too fast for opening
         YearsClosed(Region,Subarea) = YearsClosed(Region,Subarea)+1
         F(Region,Subarea) = LowF
         PctBmsClsd = PctBmsClsd + SubareaSummary(Region,Subarea)%PctEBms
       endif !growth < CritGrowth2
    elseif (YearsClosed(Region,Subarea) < 0) then !if area presently harvest area
       HarvestArea(Region,Subarea) = .TRUE.
       YearsClosed(Region,Subarea) = YearsClosed(Region,Subarea) + 1
       F(Region,Subarea) = (1.2+0.2*YearsClosed(Region,Subarea))*TotalF(Region,Subarea)/Dble(3)    
    endif
  EndDo !subarea
  !!! Now decide if area should be closed
  if (Region == 1) then  
     Head => ListHeadMA
  else
     Head => ListHeadGB
  endif
  ListPointer => Head
  Do !go down list of subareas by order of growth rates and decide if an area should be closed
    Subarea = ListPointer%Subarea
    if ((YearsClosed(Region,Subarea) == 0) .AND. (.NOT. HarvestArea(Region,Subarea))) then !if area is currently open/unrestricted
         if (ListPointer%Growth < CritGrowth) then
            Closed(Region,Subarea) = .False.
         elseif (PctBmsClsd + ListPointer%PctEBms > MaxPercentageClosed) then
            Closed(Region,Subarea) = .False.
         else
            F(Region,Subarea) = LowF
            Closed(Region,Subarea) = .True.
            YearsClosed(Region,Subarea) = 1
            PctBmsClsd = PctBmsClsd + ListPointer%PctEBms
         endif
    endif
    if (.NOT. Associated(ListPointer%Link)) then
       EXIT
    else
       ListPointer => ListPointer%Link
    endif
  EndDo 
EndDo !Region
ENDSUBROUTINE !CalcRotF


SUBROUTINE FixedRotation(F,NumRegions,year)
!subroutine for fixed rotations
!MidAtlantic rotation
Integer, Intent(In) :: NumRegions,year
Real*8, Intent(InOut) :: F(1:NumRegions,1:NumSubareas,1:NumYears)
Integer :: newmodyear,Region


if (year > (rotperiod + 1)) then 
  newmodyear = mod(year-2,rotperiod) + 3
  Do Region = 1,NumRegions
    F(Region,:,Year+1) = F(Region,:,newmodyear)
  EndDo
endif
END SUBROUTINE !Calcfixedrot

SUBROUTINE CalcCounts(NumClasses,PopVector,MeatWeight,Region,Subarea,Counts,Biomass,ExploitableBiomass,ClassWidth,TimeStep,year)
Integer, Intent(In) :: NumClasses,Region,SubArea,year
Real*8, Intent(In) :: PopVector(1:NumClasses,0:NumRegions,0:NumSubareas),MeatWeight(1:NumClasses),TimeStep,ClassWidth
Real*8, Intent(InOut) :: Counts(1:NumRegions,1:NumSubareas)
Real*8, Intent(InOut) :: Biomass(1:NumRegions,1:NumSubareas)
Real*8, Intent(InOut) :: ExploitableBiomass(1:NumRegions,1:NumSubareas)
Integer :: Class

Do Class = 1,NumClasses
  Counts(Region,Subarea) = Counts(Region,Subarea) &
                         + PopVector(Class,Region,Subarea) &
                           * Select(Class,ClassWidth,ringsize,year,Linf(Region,Subarea),K(Region,Subarea),Region,Subarea) &
                           * TimeStep
  Biomass(Region,Subarea) = Biomass(Region,Subarea) + MeatWeight(Class)*PopVector(Class,Region,Subarea)*TimeStep
  ExploitableBiomass(Region,Subarea) = ExploitableBiomass(Region,Subarea)                           &
                                     + Select(Class, ClassWidth, ringsize, year,                    &
                                              Linf(Region,Subarea),                                 &
                                              K(Region,Subarea),                                    &
                                              Region,Subarea)                                       &
                                       * MeatWeight(Class)*PopVector(Class,Region,Subarea)*TimeStep
 ! Print *,Region,Subarea,Biomass(Region,Subarea),ExploitableBiomass(Region,Subarea)
  !Read *
EndDo

END SUBROUTINE !CalcCounts




SUBROUTINE Recruit(Region,Subarea,Pop,RecruitVector,TimeStep)
Integer, Intent(In) :: Region, SubArea
Real*8, Intent(In) :: RecruitVector(1:NumRegions,1:NumSubareas),TimeStep
Real*8, Intent(InOut) :: Pop(1:NumClasses)
Real*8, Parameter :: NewRecruits = 10

Pop(1) = Pop(1) + RecruitVector(Region,Subarea)*TimeStep

END SUBROUTINE !Recruit

!!!! Structure subroutines

SUBROUTINE CalcSubareaSummary(PopVector,CatchVector,Region,Subarea,Transition,Recruits,SubareaSummary,Year,F)
Real*8, Dimension(1:NumClasses), Intent(In) :: PopVector,CatchVector
Real*8, Intent(In) :: Recruits,F,Transition(1:NumClasses,1:NumClasses)
Integer, Intent(In) :: Region,Year,Subarea
Type(PopSummaryRecord), Intent(InOut) :: SubareaSummary
Integer :: Ones(1:NumClasses),MeatCountClass,Species,Class
Real*8 :: DredgeTimePUE,DredgeAreaPUE,OldEBms,GAvgEBms,OldENum,GAvgENum !Time and area swept by dredge per day
Real*8 :: SelectivityVector(1:NumClasses),ShellHeight,SelectivityVector4(1:NumClasses)

Do Class = 1,NumClasses
  ShellHeight = StartingSH + ClassWidth*(Class - 0.5) !StartingSH
  SelectivityVector(Class) = Select(Class,ClassWidth,1,year,Linf(Region,Subarea),K(Region,Subarea),Region,Subarea)
  SelectivityVector4(Class) = Select(Class,ClassWidth,4,year,Linf(Region,Subarea),K(Region,Subarea),Region,Subarea)
EndDo
Ones = 1 !create vector of ones
SubareaSummary%Bms = Dot_Product(PopVector,MeatWeight0(:,Region,Subarea))
OldEBms = SubareaSummary%EBms
SubareaSummary%EBms = Dot_Product(PopVector,ExploitMW0(:,Region,Subarea))
SubareaSummary%EBms4 = Dot_Product(PopVector,SelectivityVector*MeatWeight0(:,Region,Subarea))
GAvgEBms = safesq(OldEBms*SubareaSummary%EBms+eps)
SubareaSummary%BmsMT = ConvertToMT(SubareaSummary%Bms,SubareaSummary%Area,DredgeFootPrint,DredgeEff(Region,Subarea))
SubareaSummary%EBmsMT = ConvertToMT(SubareaSummary%EBms,SubareaSummary%Area,DredgeFootPrint,DredgeEff(Region,Subarea))
SubareaSummary%Num = Dot_Product(PopVector,Ones)
OldENum = SubareaSummary%ENum
SubareaSummary%ENum = Dot_Product(PopVector,SelectivityVector)
SubareaSummary%ENum4 = Dot_Product(PopVector,SelectivityVector4)
GAvgENum = safesq(OldENum*SubareaSummary%ENum+eps)
if (SubareaSummary%Num > eps) then
  SubareaSummary%AvWt = SubareaSummary%Bms/SubareaSummary%Num
else
  SubareaSummary%AvWt = Dble(0)
endif
if (SubareaSummary%ENum > eps) then
  SubareaSummary%AvEWt = SubareaSummary%EBms/SubareaSummary%ENum
  SubareaSummary%AvEWt4 = SubareaSummary%EBms4/SubareaSummary%ENum4
  SubareaSummary%AvMC = Dble(454)/SubareaSummary%AvEWt
else
  SubareaSummary%AvEWt = Dble(0)
  SubareaSummary%AvEWt4 = Dble(0)
  SubareaSummary%AvMC = 99
endif 
SubareaSummary%Eggs = Dot_Product(PopVector,FecundityVector)
SubareaSummary%Abseggs = ConvertToMT(SubareaSummary%Eggs,SubareaSummary%Area,DredgeFootprint,DredgeEff(Region,Subarea))
SubareaSummary%Growth = GrowthRate(NumClasses,                    &
                                   M(Region,Subarea),             &
                                   TimeStep,                      &
                                   Dble(GrowthPeriod),            &
                                   Transition,                    &
                                   PopVector,                     &
                                   MeatWeight0(:,Region,Subarea))
SubareaSummary%Recruits = Recruits
SubareaSummary%MeatCounts = 0
CALL CalcLPUE(SubareaSummary%AvEWt,SubareaSummary%EBms,SubareaSummary%LPUE,DredgeTimePUE,DredgeAreaPUE) 
if (Year > 0) then !compute catch stats for all but initial pass
  !print *,int(catchvector(11:26)+ 0.5)
  CALL CalcMeatCounts(CatchVector,MeatWeight(:,Region,Subarea),SubareaSummary%MeatCounts,Region,Subarea,SubareaSummary%Area)
  SubareaSummary%Catch = Dot_Product(CatchVector,MeatWeight(:,Region,Subarea))
  !print *,"catch = ",subareasummary%catch
  SubareaSummary%CatchMT = ConvertToMT(SubareaSummary%Catch,SubareaSummary%Area,DredgeFootPrint,DredgeEff(Region,Subarea))
  !print *,SubareaSummary%CatchMT,sum(SubareaSummary%MeatCounts)
  SubareaSummary%Fbm = F*(1+IncidentalM(Region,Subarea))
  SubareaSummary%CatchNum = Sum(CatchVector)
  SubareaSummary%Fn = F*(1+IncidentalM(Region,Subarea))
  if (SubareaSummary%LPUE > eps) then
    SubareaSummary%DAS = Dble(2204)*SubareaSummary%CatchMT/SubareaSummary%LPUE
  else
    SubareaSummary%DAS = Dble(0)
  endif
  SubareaSummary%DredgeTime = SubareaSummary%DAS*DredgeTimePUE
  SubareaSummary%DredgeArea = SubareaSummary%DAS*DredgeAreaPUE
endif

ENDSUBROUTINE !CalcSubareaSummary

SUBROUTINE SumRec(Rec1,Rec2)
Type(PopSummaryRecord), Intent(In) :: Rec2
Type(PopSummaryRecord),Intent(InOut) :: Rec1
Type(PopSummaryRecord) :: SRec
Integer :: MC
!SumRec adds fields in Rec1 and Rec2

  SRec%Bms = Rec1%Bms + Rec2%Bms
  SRec%EBms = Rec1%EBms + Rec2%EBms
  SRec%BmsMT = Rec1%BmsMT + Rec2%BmsMT
  SRec%EBmsMT = Rec1%EBmsMT + Rec2%EBmsMT
  SRec%Num = Rec1%Num + Rec2%Num
  SRec%ENum = Rec1%ENum + Rec2%ENum
  SRec%AvWt = Rec1%AvWt + Rec2%AvWt
  SRec%AvEWt = Rec1%AvEWt + Rec2%AvEWt
  SRec%Catch = Rec1%Catch + Rec2%Catch
  SRec%CatchMT = Rec1%CatchMT + Rec2%CatchMT
  SRec%CatchNum = Rec1%CatchNum + Rec2%CatchNum 
  SRec%Fbm = Rec1%Fbm + Rec2%Fbm
  SRec%Fn = Rec1%Fn + Rec2%Fn
  SRec%Eggs = Rec1%Eggs + Rec2%Eggs
  SRec%AbsEggs = Rec1%AbsEggs + Rec2%AbsEggs
  SRec%Growth = Rec1%Growth + Rec2%Growth
  SRec%Recruits = Rec1%Recruits + Rec2%Recruits
  Do MC = 1,5
    SRec%MeatCounts(MC) = Rec1%MeatCounts(MC) + Rec2%MeatCounts(MC)
  Enddo
  SRec%LPUE = Rec1%LPUE + Rec2%LPUE
  SRec%DAS = Rec1%DAS + Rec2%DAS 
  SRec%DredgeTime = Rec1%DredgeTime + Rec2%DredgeTime
  SRec%DredgeArea = Rec1%DredgeArea + Rec2%DredgeArea
  SRec%PctClosed = Rec1%PctClosed + Rec2%PctClosed
  SRec%Area = Rec1%Area + Rec2%Area
  Rec1 = SRec
ENDSUBROUTINE SumRec

SUBROUTINE SSRec(Rec1,Rec2)
Type(PopSummaryRecord), Intent(In) :: Rec2
Type(PopSummaryRecord),Intent(InOut) :: Rec1
Type(PopSummaryRecord) :: SSqRec
Integer :: MC
  SSqRec%Bms = Rec1%Bms + Rec2%Bms**2
  SSqRec%EBms = Rec1%EBms + Rec2%EBms**2
  SSqRec%BmsMT = Rec1%BmsMT + Rec2%BmsMT**2
  SSqRec%EBmsMT = Rec1%EBmsMT + Rec2%EBmsMT**2
  SSqRec%Num = Rec1%Num + Rec2%Num**2
  SSqRec%ENum = Rec1%ENum + Rec2%ENum**2
  SSqRec%AvWt = Rec1%AvWt + Rec2%AvWt**2
  SSqRec%AvEWt = Rec1%AvEWt + Rec2%AvEWt**2
  SSqRec%Catch = Rec1%Catch + Rec2%Catch**2
  SSqRec%CatchMT = Rec1%CatchMT + Rec2%CatchMT**2
  SSqRec%CatchNum = Rec1%CatchNum + Rec2%CatchNum**2 
  SSqRec%Fbm = Rec1%Fbm + Rec2%Fbm**2
  SSqRec%Fn = Rec1%Fn + Rec2%Fn**2
  SSqRec%Eggs = Rec1%Eggs + Rec2%Eggs**2
  SSqRec%AbsEggs = Rec1%AbsEggs + Rec2%AbsEggs**2
  SSqRec%Growth = Rec1%Growth + Rec2%Growth**2
  SSqRec%Recruits = Rec1%Recruits + Rec2%Recruits**2
  Do MC = 1,5
    SSqRec%MeatCounts(MC) = Rec1%MeatCounts(MC) + Rec2%MeatCounts(MC)**2
  Enddo
  SSqRec%LPUE = Rec1%LPUE + Rec2%LPUE**2
  SSqRec%DAS = Rec1%DAS + Rec2%DAS**2
  SSqRec%DredgeTime = Rec1%DredgeTime + Rec2%DredgeTime**2
  SSqRec%DredgeArea = Rec1%DredgeArea + Rec2%DredgeArea**2
  SSqRec%PctClosed = Rec1%PctClosed + Rec2%PctClosed**2
  SSqRec%Area = Rec1%Area + Rec2%Area**2
  Rec1 = SSqRec
ENDSUBROUTINE !Sum

Type(PopSummaryRecord) FUNCTION DiffRec(Rec1,Rec2)
Type(PopSummaryRecord), Intent(In) :: Rec2
Type(PopSummaryRecord),Intent(InOut) :: Rec1
Type(PopSummaryRecord) :: DRec
Integer :: MC
!DiffRec adds fields in Rec1 and Rec2

  DRec%Bms = Rec1%Bms - Rec2%Bms
  DRec%EBms = Rec1%EBms - Rec2%EBms
  DRec%BmsMT = Rec1%BmsMT - Rec2%BmsMT
  DRec%EBmsMT = Rec1%EBmsMT - Rec2%EBmsMT
  DRec%Num = Rec1%Num - Rec2%Num
  DRec%ENum = Rec1%ENum - Rec2%ENum
  DRec%AvWt = Rec1%AvWt - Rec2%AvWt
  DRec%AvEWt = Rec1%AvEWt - Rec2%AvEWt
  DRec%Catch = Rec1%Catch - Rec2%Catch
  DRec%CatchNum = Rec1%CatchNum - Rec2%CatchNum
  DRec%CatchMT = Rec1%CatchMT - Rec2%CatchMT
  DRec%Fbm = Rec1%Fbm - Rec2%Fbm
  DRec%Fn = Rec1%Fn - Rec2%Fn
  DRec%Eggs = Rec1%Eggs - Rec2%Eggs
  DRec%AbsEggs = Rec1%AbsEggs - Rec2%AbsEggs
  DRec%Growth = Rec1%Growth - Rec2%Growth
  DRec%Recruits = Rec1%Recruits - Rec2%Recruits
  Do MC = 1,5
    DRec%MeatCounts(MC) = Rec1%MeatCounts(MC) - Rec2%MeatCounts(MC)
  Enddo
  DRec%DAS = Rec1%DAS - Rec2%DAS
  DRec%LPUE = Rec1%LPUE - Rec2%LPUE
  DRec%DredgeTime = Rec1%DredgeTime - Rec2%DredgeTime
  DRec%DredgeArea = Rec1%DredgeArea - Rec2%DredgeArea
  DRec%PctClosed = Rec1%PctClosed - Rec2%PctClosed
  if (DRec%PctClosed < eps) then
    DRec%PctClosed = eps
  endif
  DRec%Area = Rec1%Area - Rec2%Area
  if (DRec%Area < eps) then
    DRec%Area = eps
  endif
  DiffRec = DRec
ENDFUNCTION DiffRec



Type(PopSummaryRecord) FUNCTION Square(Rec)
Type(PopSummaryRecord), Intent(In) :: Rec
Type(PopSummaryRecord) :: SqRec
Integer :: MC

SqRec%Bms = Rec%Bms**2
SqRec%EBms = Rec%EBms**2
SqRec%BmsMT = Rec%BmsMT**2
SqRec%EBmsMT = Rec%EBmsMT**2
SqRec%Num = Rec%Num**2
SqRec%ENum = Rec%ENum**2
SqRec%AvWt = Rec%AvWt**2
SqRec%AvEWt = Rec%AvEWt**2
SqRec%Catch = Rec%Catch**2
SqRec%CatchMT = Rec%CatchMT**2
SqRec%CatchNum = Rec%CatchNum**2
SqRec%Fbm = Rec%Fbm**2
SqRec%Fn = Rec%Fn**2
SqRec%Eggs = Rec%Eggs**2
SqRec%AbsEggs = Rec%AbsEggs**2
SqRec%Growth = Rec%Growth**2
SqRec%Recruits = Rec%Recruits**2
Do MC = 1,5
  SqRec%MeatCounts(MC) = Rec%MeatCounts(MC)**2
Enddo
SqRec%LPUE = Rec%LPUE**2
SqRec%DAS = Rec%DAS**2
SqRec%DredgeTime = Rec%DredgeTime**2
SqRec%DredgeArea = Rec%DredgeArea**2
SqRec%PctClosed = Rec%PctClosed**2
Square = SqRec

ENDFUNCTION !square


Real*8 FUNCTION SafeSq(Number)
Real*8, Intent(In) :: Number

if (Number > Dble(0)) then
  SafeSq = sqrt(Number)
else
  SafeSq = Dble(0)
endif

ENDFUNCTION !SafeSq

Type(PopSummaryRecord) FUNCTION SquareRt(Rec)
Type(PopSummaryRecord), Intent(In) :: Rec
Type(PopSummaryRecord) :: SqRec
Integer :: MC

SqRec%Bms = safesq(Rec%Bms)
SqRec%EBms = safesq(Rec%EBms)
SqRec%BmsMT = safesq(Rec%BmsMT)
SqRec%EBmsMT = safesq(Rec%EBmsMT)
SqRec%Num = safesq(Rec%Num)
SqRec%ENum = safesq(Rec%ENum)
SqRec%AvWt = safesq(Rec%AvWt)
SqRec%AvEWt = safesq(Rec%AvEWt)
if (Rec%Catch > 1e-10) then
  SqRec%Catch = safesq(Rec%Catch)
  SqRec%CatchMT = safesq(Rec%CatchMT)
  SqRec%CatchNum = safesq(Rec%CatchNum)
  Do MC = 1,5
     SqRec%MeatCounts(MC) = safesq(Rec%MeatCounts(MC))
  Enddo
  SqRec%DAS = safesq(Rec%DAS)
else
   SqRec%Catch = 0
  SqRec%CatchMT = 0
  SqRec%CatchNum = 0
  Do MC = 1,5
    SqRec%MeatCounts(MC) = 0
  EndDo
  SqRec%DAS = 0
endif
if (Rec%Fbm >= 0) then
   SqRec%Fbm = safesq(Rec%Fbm)
else
  Print *,"neg F",Rec%Fbm
  SqRec%FBm = Dble(0)
endif
SqRec%Fn = safesq(Rec%Fn)
SqRec%Eggs = safesq(Rec%Eggs)
SqRec%AbsEggs = safesq(Rec%AbsEggs)
SqRec%Growth = safesq(Rec%Growth)
SqRec%Recruits = safesq(Rec%Recruits)
SqRec%LPUE = safesq(Rec%LPUE)
SqRec%DredgeTime = safesq(Rec%DredgeTime)
SqRec%DredgeArea = safesq(Rec%DredgeArea)
SqRec%PctClosed = safesq(Rec%PctClosed)
SquareRt = SqRec

ENDFUNCTION !squarert



Type(PopSummaryRecord) FUNCTION Quotient(Rec,n)
Type(PopSummaryRecord), Intent(In) :: Rec
Real*8,Intent(In) :: n
Type(PopSummaryRecord) :: qrec
Integer :: MC
Real*8 :: n1

n1 = n+1
qrec%Bms = Rec%Bms/n1
qrec%EBms = Rec%EBms/n1
qrec%BmsMT = Rec%BmsMT/n1
qrec%EBmsMT = Rec%EBmsMT/n1
qrec%Num = Rec%Num/n1
qrec%ENum = Rec%ENum/n1
qrec%AvWt = Rec%AvWt/n1
qrec%AvEWt = Rec%AvEWt/n1
qrec%Catch = Rec%Catch/n
qrec%CatchMT = Rec%CatchMT/n
qrec%CatchNum = Rec%CatchNum/n
qrec%Fbm = Rec%Fbm/n
qrec%Fn = Rec%Fn/n
qrec%Eggs = Rec%Eggs/n1
qrec%AbsEggs = Rec%AbsEggs/n1
qrec%Growth = Rec%Growth/n1
qrec%Recruits = Rec%Recruits/n
Do MC = 1,5
  qrec%MeatCounts(MC) = Rec%MeatCounts(MC)/n
Enddo
qrec%DAS = Rec%DAS/n1
qrec%LPUE = Rec%LPUE/n1
qrec%DredgeTime = Rec%DredgeTime/n
qrec%DredgeArea = Rec%DredgeArea/n
qrec%PctClosed = Rec%PctClosed/n
qrec%Area = Rec%Area
Quotient = qrec

ENDFUNCTION !quotient


SUBROUTINE WriteOutput(RunCount, Year, NumRegions, NumSubareas, Region,        &
                       Subarea, SubareaSummary, RegionSummary, OverallSummary, &
                       outputcode, SFoutput)
Integer, Intent(In) :: RunCount,Year,Region,Subarea,outputcode,NumSubareas,NumRegions
Logical, Intent(In) :: SFOutput
Type(PopSummaryRecord), Intent(In) :: SubareaSummary(1:NumRegions,1:NumSubareas)
Type(PopSummaryRecord), Intent(InOut) :: RegionSummary(1:NumRegions),OverallSummary
Integer :: Acount
Type(PopSummaryRecord) :: OpArray(1:NumSubareas-opnum+1),ClArray(1:Opnum-1)


if ((outputcode ==1) .AND. Filter(Subarea,Region)) then !if subregions are output
   CALL WritelnOutput(RunCount,Year,Subarea,Region,SubareaSummary(Region,Subarea),SFOutput)
endif
if (Subarea == NumSubareas) then !if last subarea, write region 
   !CALL CalcRegionalPopVector(PopVector,Area)  
   CALL CalcSummary(NumSubAreas,SubareaSummary(Region,:),RegionSummary(Region),Year,Region)
   if (outputcode < 3) then
      if (Region == GBRegion) then
        Do Acount = Opnum,NumSubareas
          OpArray(Acount-Opnum+1) = SubareaSummary(2,Acount)
        EndDo
        Do Acount = 1,opnum-1
          ClArray(Acount) = SubareaSummary(GBRegion,Acount)
        EndDo 
        CALL CalcSummary(3,OpArray,GBop,Year,Region)
        CALL WritelnOutput(RunCount,Year,-1,GBRegion,GBop,SFOutput)
        CALL CalcSummary(6,ClArray,GBcl,Year,Region)  
        CALL WritelnOutput(RunCount,Year,-2,GBRegion,GBcl,SFOutput)
      endif
        CALL WritelnOutput(RunCount,Year,0,Region,RegionSummary(Region),SFOutput)
   endif
   if (Region == NumRegions) then !if last area has been done
       CALL CalcSummary(NumRegions,RegionSummary,OverallSummary,Year,Region)
       CALL WritelnOutput(RunCount,Year,0,0,OverallSummary,SFOutput)
   endif !Region == NumRegions
endif !subarea==Numsubareas

ENDSUBROUTINE !WriteOutput

LOGICAL FUNCTION Filter(Subarea,Region)
Integer, Intent(In) :: Subarea,Region

if (Region >= GBRegion) then
  Filter = .TRUE.
elseif (Subarea <= MASubareas) then
  Filter = .TRUE.
else
  Filter = .FALSE.
endif

ENDFUNCTION

SUBROUTINE WritelnOutput(RunNum,Year,Subarea,Region,DataRec,SFOutput)
Integer, Intent(In) :: RunNum,Year,Region,Subarea
Logical, Intent(In) :: SFOutput
Type(PopSummaryRecord), Intent(In) :: DataRec
Integer :: Formatcode,Formattens,SizeClass,CalendarYear,RegCount
Real*8 :: CatchAll

CalendarYear = Year + StartingYear  
if (Comma) then
  if ((Subarea > 0) .AND. (Region > 0)) then !if subarea output
     Write(15,55) RunNum, CalendarYear, Region, Subarea
     if (SFOutput) then
        Write (16,55) RunNum, CalendarYear, Region, Subarea
        if (year > 0) then
           Write (11,55) RunNum, CalendarYear-1, Region, Subarea
        endif
     endif
  elseif ((Subarea == 0) .AND. (Region > 0)) then !if regional output
    Write(15,56) RunNum, CalendarYear, Region, 'All'
     if (SFOutput) then
        Write (16,56) RunNum, CalendarYear, Region, 'All'
        if (year > 0) then
          Write (11,56) RunNum, CalendarYear-1, Region, 'All'
        endif
     endif
  elseif (Subarea == -1) then
    Write(15,56) RunNum, CalendarYear, Region, 'Op '
    if (SFOutput) then
        Write (16,56) RunNum, CalendarYear, Region, 'Op '
        if (year > 0) then
           Write (11,56) RunNum, CalendarYear - 1, Region, 'Op '
        endif 
    endif
  elseif (Subarea == -2) then
    Write(15,56) RunNum, CalendarYear, Region, 'Cl '
    if (SFOutput) then
        Write (16,56) RunNum, CalendarYear, Region, 'Cl '
        if (year > 0) then
          Write (11,56) RunNum, CalendarYear - 1, Region, 'Cl '
        endif
     endif
  else
    Write(15,57) RunNum, CalendarYear,'All','All'
    if (SFOutput) then
        Write (16,57) RunNum, CalendarYear, 'All','All'
        if (year > 0) then
           Write (11,57) RunNum, CalendarYear-1, 'All','All'
        endif
     endif
  endif !subarea
  if (Year==0) then
    Write(15,58) MV,MV,Round(DataRec%Bms),Round(DataRec%EBms),Round(DataRec%Num),&
             Round(DataRec%ENum),MV,DataRec%AvWt,DataRec%AvEWt,MV,Round(DataRec%Eggs),Round(DataRec%Abseggs),Round(DataRec%BmsMT),&
             Round(DataRec%EBmsMT),MV,MV,MV, MV,MV,MV,MV,MV, DataRec%Growth,Round(DataRec%LPUE),MV,MV,MV,MV,MV 
  else
     if ((Subarea > 0) .AND. (Region > 0)) then !if subarea output
       Write(13,55) RunNum, CalendarYear, Region, Subarea
     elseif ((Subarea == 0) .AND. (Region > 0)) then !if regional output
       Write(13,56) RunNum, CalendarYear, Region, 'All'
     elseif (Subarea == -1) then
       Write(13,56) RunNum, CalendarYear, Region, 'Op '
     elseif (Subarea == -2) then
       Write(13,56) RunNum, CalendarYear, Region, 'Cl '
     else
        Write(13,57) RunNum, CalendarYear,'All','All'
     endif !subarea
!    CALL WritelnBycatch(DataRec%bycatch)
    if (DataRec%AvMC > 0) then 
      Write(15,59) DataRec%Fbm,DataRec%Fn,                  &
                   Round(DataRec%Bms),                      &
                   Round(DataRec%EBms),                     &
                   Round(DataRec%Num),                      &
                   Round(DataRec%ENum),                     &
                   Round(DataRec%Recruits),                 &
                   DataRec%AvWt,DataRec%AvEWt,DataRec%AvMC, &
                   Round(DataRec%Eggs),                     &
                   Round(DataRec%Abseggs),                  &
                   Round(DataRec%BmsMT),                    &
                   Round(DataRec%EBmsMT),                   &
                   Round(DataRec%Catch),DataRec%CatchNum,   &
                   Round(DataRec%CatchMT),                  &
                   Round(DataRec%MeatCounts(1)),            &
                   Round(DataRec%MeatCounts(2)),            &
                   Round(DataRec%MeatCounts(3)),            &
                   Round(DataRec%MeatCounts(4)),            &
                   Round(DataRec%MeatCounts(5)),            &
                   DataRec%Growth,                          &
                   Round(DataRec%LPUE),                     &
                   Round(DataRec%DAS),                      &
                   Round(DataRec%DredgeTime),               &
                   Round(DataRec%DredgeArea),               &
                   Round(100*DataRec%PctClosed + 0.5)
    else
           Write(15,79) DataRec%Fbm,DataRec%Fn,             &
                   Round(DataRec%Bms),                      &
                   Round(DataRec%EBms),                     &
                   Round(DataRec%Num),                      &
                   Round(DataRec%ENum),                     &
                   Round(DataRec%Recruits),                 &
                   DataRec%AvWt,DataRec%AvEWt,              &
                   MV,                                      &
                   Round(DataRec%Eggs),                     &
                   Round(DataRec%Abseggs),                  &
                   Round(DataRec%BmsMT),                    &
                   Round(DataRec%EBmsMT),                   &
                   Round(DataRec%Catch),                    &
                   DataRec%CatchNum,                        &
                   Round(DataRec%CatchMT),                  &
                   Round(DataRec%MeatCounts(1)),            &
                   Round(DataRec%MeatCounts(2)),            &
                   Round(DataRec%MeatCounts(3)),            &
                   Round(DataRec%MeatCounts(4)),            &
                   Round(DataRec%MeatCounts(5)),            &
                   DataRec%Growth,                          &
                   Round(DataRec%LPUE),                     &
                   Round(DataRec%DAS),                      &
                   Round(DataRec%DredgeTime),               &
                   Round(DataRec%DredgeArea),               &
                   Round(100*DataRec%PctClosed+0.5)
    endif
   if ((Subarea == 0) .AND. (Region == 0)) then !if overallsummary
      Write (15,54) DataRec%Price,Round(DataRec%Revenue/1000)
   else
      Write (15,53) MV,MV
   endif !subarea
  endif !year==0
  ! WRITE SIZE FREQ OUTPUT
  if (SFOutput) then
     Do SizeClass = 1,NumClasses
       if (Subarea >= 1) then
         Write (16,44) PopVector(SizeClass,Region,Subarea) !output standing stock by size classes
         if (year > 0) then
            Write (11,44) CatchVector(SizeClass,Region,Subarea) !output catch by size classes
         endif
       elseif (Subarea == 0) then
         Write (16,44) PopVector(SizeClass,Region,Subarea) !output standing stock by size classes
         if (year > 0) then
           if (region > 0) then
            Write (11,44) Dot_product(CatchVector(SizeClass,Region,:),Area(Region,:))/sum(Area(Region,:)) !output catch by size classes
           else
               CatchAll = dot_product(CatchVector(SizeClass,1,:),Area(1,:))/sum(Area(1,:))
               Do RegCount = 2,NumRegions
                   CatchAll = CatchAll + dot_product(CatchVector(SizeClass,RegCount,:),Area(RegCount,:))/sum(Area(RegCount,:))
               enddo
               Write (11,44) CatchAll
           endif  !region > 0
         endif !year > 0
     elseif (subarea < 0 .and. region == GBRegion) then
         Write(16,44) PopVector(SizeClass,Region,subarea) !-1 = open areas, -2 = closed areas
         if (year > 0) then
            if (subarea == -1) then  !open areas
               Write(11,44) dot_product(CatchVector(SizeClass,Region,OpNum:NumSubAreas), &
                                        Area(Region,OpNum:NumSubAreas))                  &
                          / sum(Area(Region,OpNum:NumSubareas))
            elseif (subarea == -2) then !closed areas
               Write(11,44) dot_product(CatchVector(SizeClass,Region,1:(OpNum-1)),       &
                                        Area(Region,1:(OpNum-1)))                        &
                          / sum(Area(Region,1:(OpNum-1)))
            endif
          endif !year
     endif  !(subarea)        
     End Do
     if (Subarea >= -2) then
        Write (16,*)
        if (year > 0) then
           Write (11,*) 
        endif
     endif
  endif
 44     Format(F10.2,",",$)     
else !if comma delimited is turned off
  if ((Subarea > 0) .AND. (Region > 0)) then !if subarea output
     Write(15,65) RunNum, CalendarYear, Region, Subarea
     Write(13,65) RunNum, CalendarYear, Region, Subarea
  elseif ((Subarea == 0) .AND. (Region > 0)) then !if regional output
    Write(15,66) RunNum, CalendarYear, Region, 'All'
    Write(13,66) RunNum, CalendarYear, Region, 'All'
  elseif (Subarea == -1) then
    Write(15,66) RunNum, CalendarYear, Region, 'Op '
     Write(13,66) RunNum, CalendarYear, Region, 'Op '
  elseif (Subarea == -2) then
    Write(15,66) RunNum, CalendarYear, Region, 'Cl '
    Write(13,66) RunNum, CalendarYear, Region, 'Cl '
  else
    Write(15,67) RunNum, CalendarYear,'All','All'
    Write(13,67) RunNum, CalendarYear,'All','All'
  endif !subarea
  if (Year==0) then
    Write(15,68) MV,MV,Round(DataRec%Bms),Round(DataRec%EBms),Round(DataRec%Num),&
             Round(DataRec%ENum),MV,DataRec%AvWt,DataRec%AvEWt,MV,Round(DataRec%Eggs),Round(DataRec%Abseggs),Round(DataRec%BmsMT),&
             Round(DataRec%EBmsMT),MV,MV,MV, MV,MV,MV,MV,MV, DataRec%Growth,Round(DataRec%LPUE),MV,MV,MV,MV,MV,MV,MV 
  else
 !   CALL WritelnBycatch(DataRec%bycatch)
    Write(15,69) DataRec%Fbm,DataRec%Fn,                                       &
            Round(DataRec%Bms),                                                &
            Round(DataRec%EBms),                                               &
            Round(DataRec%Num),                                                &
            Round(DataRec%ENum),                                               &
            Round(DataRec%Recruits),                                           &
            DataRec%AvWt,DataRec%AvEWt,DataRec%AvMC,                           &
            Round(DataRec%Eggs),                                               &
            Round(DataRec%Abseggs),                                            &
            Round(DataRec%BmsMT),                                              &
            Round(DataRec%EBmsMT),                                             &
            Round(DataRec%Catch),                                              &
            DataRec%CatchNum,                                                  &
            Round(DataRec%CatchMT),                                            &
            Round(DataRec%MeatCounts(1)),                                      &
            Round(DataRec%MeatCounts(2)),                                      &
            Round(DataRec%MeatCounts(3)),                                      &
            Round(DataRec%MeatCounts(4)),                                      &
            Round(DataRec%MeatCounts(5)),                                      &
            DataRec%Growth,                                                    &
            Round(DataRec%LPUE),                                               &
            Round(DataRec%DAS),                                                &
            Round(DataRec%DredgeTime),                                         &
            Round(DataRec%DredgeArea)
   if ((Subarea == 0) .AND. (Region == 0)) then !if overallsummary
      Write (15,64) DataRec%Price,Round(DataRec%Revenue/1000)
   else
      Write (15,63) MV,MV
   endif !subarea
  endif !year==0
endif


53 Format (", ",A5,", ",A5)
54 Format (",",F5.2,",",I7)
55 Format (I5,",", I4,",",I3,",",I3,",",$)
56 Format (I5,",", I4,",",I3,",",A5,",",$)  
57 Format (I5,",", I4,",",A5,",",A5,",",$) 
58 Format (A5,", ",A5,", ",I7,",",I7,",",I6,",",I6,", ",A5,", ",F5.1,",",      &
           F5.1,", ",A5,", ",I7,",",I7,",", I9,",",I9,", ",A5,", ",A5,", ",    &
           A5,", ",A5,", ",A5,", ",A5,", ",A5,", ",A5,", ",F7.3,",",I7,",",    &
           A5,",",A5,",",A5,",",A5,",",A5)   
59 Format (F5.2,",",F5.2,",",I7,",",I7,",",I6,",",I6,",",I5,",",F6.1,",",F6.1,",",F6.1,",", I7,",",I7,",",&
           I9,",",I9,",",I6,",",F6.1,",",I9,",",I8,",",I8,",",I8,",",I8,",",I8,",",F8.3,",",I7,",",I7,",",I7,",",I7,",",I5,$)   
79 Format (F6.2,",",F6.2,",",I7,",",I7,",",I6,",",I6,",",I5,",",F6.1,",",F6.1,",",A5,",", I7,",",I7,",",&
           I9,",",I9,",",I6,",",F6.1,",",I9,",",I8,",",I8,",",I8,",",I8,",",I8,",",F7.3,",",I7,",",I7,",",I7,",",I7,",",I5,$) 
63 Format (2X,A5,2X,A5)
64 Format (1X,F5.2,1X,I7)
65 Format (I5,1X, I4,1X,I3,1X,I3,1X,$)
66 Format (I5,1X, I4,1X,I3,1X,A5,1X,$)  
67 Format (I5,1X, I4,1X,A5,1X,A5,1X,$) 
68 Format (A5,2X,A5,2X,I7,1X,I7,1X,I6,1X,I6,2X,A5,2X,F6.1,1X,F6.1,2X,A5,2X,I7,1X,I7,1X,&
           I9,1X,I9,2X,A5,3X,A5,4X,A5,2X,5A8,2X,F8.3,1X,I7,1X,A5,2A7,1X,A7,1X,3A7)   
69 Format (F5.2,1X,F5.2,1X,I7,1X,I7,1X,I6,1X,I6,1X,I5,1X,F5.1,1X,F5.1,1X,F5.1,1X, I7,1X,I7,1X,&
           I9,1X,I9,1X,I6,1X,F6.1,1X,I9,1X,5I8,1X,F7.3,1X,4I7,1X,I6,1X,I6,$)   
ENDSUBROUTINE !WriteOutput

Integer FUNCTION Round(Number)
Real*8,Intent(In) :: Number

Round = Int(Number+0.5)
ENDFUNCTION 



SUBROUTINE WriteRunSummary(RunNum,Years,RunSummary,RunSummarySS)
Integer, Intent(In) :: RunNum,Years
Type(PopSummaryRecord), Intent(InOut) :: RunSummary,RunSummarySS
Type(PopSummaryRecord) :: Stdev,Temp

if (Years > 1) then
  RunSummary = Quotient(RunSummary,Dble(Years-1))
  Write (17,61) RunNum,RunSummary%Fbm,RunSummary%Fn,                           &
          Round(RunSummary%Bms),                                               &
          Round(RunSummary%EBms),                                              &
          Round(RunSummary%Num),                                               &
          Round(RunSummary%ENum),                                              &
          Round(RunSummary%Recruits),                                          &
          RunSummary%AvWt,                                                     &
          RunSummary%AvEWt,                                                    &
          Round(RunSummary%Eggs),                                              &
          Round(RunSummary%Abseggs),                                           &
          Round(RunSummary%BmsMT),                                             &
          Round(RunSummary%EBmsMT),                                            &
          Round(RunSummary%Catch),                                             &
          RunSummary%CatchNum,                                                 &
          Round(RunSummary%CatchMT),                                           &
          Round(RunSummary%MeatCounts(1)),                                     &
          Round(RunSummary%MeatCounts(2)),                                     &
          Round(RunSummary%MeatCounts(3)),                                     &
          Round(RunSummary%MeatCounts(4)),                                     &
          Round(RunSummary%MeatCounts(5)),                                     &
          RunSummary%Growth,                                                   &
          Round(RunSummary%LPUE),                                              &
          Round(RunSummary%DAS),                                               &
          Round(RunSummary%DredgeTime),                                        &
          Round(RunSummary%DredgeArea)
  if (Years > 2) then
    RunSummarySS = Quotient(RunSummarySS,Dble(Years-1))
    Temp = DiffRec(RunSummarySS,Square(RunSummary))
    Temp = Quotient(Temp,Dble(Years-2)/Dble(Years-1))
    Stdev = SquareRt(Temp)
    Write (18,61) RunNum,Stdev%Fbm,Stdev%Fn, Round(Stdev%Bms),                 &
            Round(Stdev%EBms), Round(Stdev%Num), Round(Stdev%ENum),            &
            Round(Stdev%Recruits), Stdev%AvWt,Stdev%AvEWt, Round(Stdev%Eggs),  &
            Round(Stdev%Abseggs), Round(Stdev%BmsMT), Round(Stdev%EBmsMT),     &
            Round(Stdev%Catch), Stdev%CatchNum, Round(Stdev%CatchMT),          &
            Round(Stdev%MeatCounts(1)), Round(Stdev%MeatCounts(2)),            &
            Round(Stdev%MeatCounts(3)), Round(Stdev%MeatCounts(4)),            &
            Round(Stdev%MeatCounts(5)), Stdev%Growth,Round(Stdev%LPUE),        &
            Round(Stdev%DAS), Round(Stdev%DredgeTime), Round(Stdev%DredgeArea)
  endif !Years > 2
endif !Years > 1
61 Format (I6,1X,F5.2,1X,F5.2,1X,I7,1X,I7,1X,I6,1X,I6,1X,I7,1X,F6.1,1X,F6.1,1X, I7,1X,I7,1X,&
           I9,1X,I9,1X,I7,1X,F6.1,1X,I9,1X,5I8,1X,F7.3,1X,4I7)   

ENDSUBROUTINE !WriteRunSummary

Real*8 FUNCTION RegionSum(NumSubAreas,AreaValues,Area)
Integer, Intent(In) :: NumSubAreas
Real*8, Dimension(1:NumSubAreas),Intent(In) :: AreaValues,Area
Real*8 :: Sumproduct,AreaSum

Sumproduct = Dble(0)
AreaSum = Dble(0)
SumProduct = Dot_Product(AreaValues,Area)
AreaSum = Sum(Area)
RegionSum = Sumproduct/AreaSum

END FUNCTION !RegionSum

SUBROUTINE CalcRegionalPopVector(PopVector,Area)  !Calculates regional and overall average size frequencies
Real*8, Dimension(1:NumRegions,1:NumSubareas), Intent(In) :: Area
Real*8, Dimension(1:NumClasses,0:NumRegions,-2:NumSubareas), Intent(InOut) :: PopVector
Integer :: Subarea, Region,SizeClass
Real*8 :: Sumproduct,TotSum,AreaSum(1:NumRegions),TotalArea

TotalArea = Dble(0)
AreaSum = Dble(0)
Do Region = 1,NumRegions
   Do Subarea = 1,NumSubareas
      AreaSum(Region) = AreaSum(Region) + Area(Region,Subarea)
   Enddo
   TotalArea = TotalArea + AreaSum(Region)
Enddo
Do SizeClass = 1,NumClasses
  Do Region = 1,NumRegions

     TotSum = Dble(0)
     SumProduct = Dble(0)
     Do Subarea = 1,NumSubareas
       SumProduct = SumProduct + PopVector(SizeClass,Region,Subarea)*Area(Region,Subarea)
     Enddo
     TotSum = TotSum + Sumproduct
     PopVector(SizeClass,Region,0) = Sumproduct/Areasum(Region)
  Enddo
  PopVector(SizeClass,0,0) = TotSum/Totalarea
Enddo
EndSubroutine

Real*8 FUNCTION RegionSumMT(NumSubareas,AreaValues)
Integer, Intent(In) :: Numsubareas
Real*8, Dimension(1:NumSubAreas), Intent(In) :: AreaValues

RegionSumMT = Sum(AreaValues)

ENDFUNCTION ! RegionSumMT
  



SUBROUTINE CalcSummary(NumSubAreas,SubareaSummary,Summary,Year,Region)
Integer, Intent(In) :: NumSubareas,Year,Region
Type(PopSummaryRecord), Dimension(1:NumSubareas), Intent(In) :: SubareaSummary
Type(PopSummaryRecord), Intent(Out) :: Summary
Integer :: MCCategory,Species,Subarea
Real*8 :: OldEBms,OldENum
!Integer :: Subarea
!Real*8 :: SumBms,SumEBms,SumBmsMT,SumEBms,SumNum,SumENum,Sum


Summary%Bms = RegionSum(NumSubAreas,SubareaSummary%Bms,SubareaSummary%Area)
Summary%EBms = RegionSum(NumSubAreas,SubareaSummary%EBms,SubareaSummary%Area)
Summary%Num = RegionSum(NumSubAreas,SubareaSummary%Num,SubareaSummary%Area)  
OldENum = Summary%ENum
Summary%ENum = RegionSum(NumSubAreas,SubareaSummary%ENum,SubareaSummary%Area)
Summary%CatchNum = RegionSum(NumSubAreas,SubareaSummary%CatchNum,SubareaSummary%Area)
Summary%Catch = RegionSum(NumSubAreas,SubareaSummary%Catch,SubareaSummary%Area)
Summary%AvWt = Summary%Bms/Summary%Num
Summary%AvEWt = Summary%EBms/Summary%ENum
Summary%BmsMT = RegionSumMT(NumSubAreas,SubareaSummary%BmsMT)
OldEBms = Summary%EBmsMT
!Print *,OldEBms,OldENum
Summary%EBmsMT = RegionSumMT(NumSubAreas,SubareaSummary%EBmsMT)
Summary%Eggs = RegionSum(NumSubareas,SubareaSummary%Eggs,SubareaSummary%Area)
Summary%AbsEggs = RegionSumMT(NumSubareas,SubareaSummary%AbsEggs)
Summary%Growth = RegionSum(NumSubareas,SubareaSummary%Growth,SubareaSummary%Area)
if (Summary%EBms > 0) then
  Summary%LPUE = RegionSum(NumSubareas,SubareaSummary%LPUE,SubareaSummary%Area)
else
  Summary%LPUE = -9999
endif
Survey(Region,Year) = Summary%Bms
ESurvey(Region,Year) = Summary%EBms
if (Year > 0) then
  Summary%CatchMT = RegionSumMT(NumSubAreas,SubareaSummary%CatchMT)
  Landings(Region,Year) = Summary%CatchMT
  LPUE(Region,Year) = Summary%LPUE
  if (Summary%CatchMT > 1e-10) then
     Summary%AvMC = RegionSum(NumSubAreas,SubareaSummary%AvMC,SubareaSummary%CatchMT)
  else
     Summary%AvMC = -1
  endif
  Summary%Fbm = Summary%CatchMT/safesq(Summary%BmsMT*OldEBms+eps)*(1+MeanIncidentalM(Region))
  Summary%Fn = RegionSum(NumSubareas,SubareaSummary%Fn,SubareaSummary%ENum)
  RegionalF(Region,Year) = Summary%Fn
  Summary%Recruits = RegionSum(NumSubareas,SubareaSummary%Recruits,SubareaSummary%Area)
  Do MCCategory = 1,5
    Summary%MeatCounts(MCCategory) = RegionSumMT(NumSubareas,SubareaSummary%MeatCounts(MCCategory))
  Enddo
  Summary%DAS = RegionSumMT(NumSubareas,SubareaSummary%DAS)
  if (Summary%DAS > eps) then
    Summary%LPUE = Dble(2204)*Summary%CatchMT/Summary%DAS
  endif
  Summary%DredgeTime = RegionSumMT(NumSubareas,SubareaSummary%DredgeTime)
  Summary%DredgeArea = RegionSumMT(NumSubareas,SubareaSummary%DredgeArea)
  !Do Species = 1,NumBycatch
  !  Summary%bycatch(species) = RegionSumMT(NumSubareas,SubareaSummary%bycatch(species))
  !Enddo
  Summary%PctClosed = RegionSum(NumSubareas,SubareaSummary%PctClosed,SubareaSummary%EBmsMT)
endif
  if (Summary%CatchMT > eps + 1e9) then
    Summary%Price = 0.1
    Summary%Revenue = 0.1
    !Summary%Price = exp(LnPrice(Summary%CatchMT+1,Summary%AvEWt+1))
    !Summary%Revenue = Revenue(Summary%CatchMT+1,Summary%AvEWt+1)
  else 
    Summary%Price = 99
    Summary%Revenue = 99
  endif
ENDSUBROUTINE !CalcSummary


SUBROUTINE InitializeSummaryRecords(Area,SubAreaSummary,RegionalSummary,OverallSummary)
Real*8, Dimension(1:NumRegions,1:NumSubAreas) :: Area
Type(PopSummaryRecord), Intent(Out) :: SubAreaSummary(1:NumRegions,1:NumSubAreas),RegionalSummary(1:NumRegions),OverallSummary
Integer :: Region,SubArea
Real*8 :: RegionSum,TotalSum

TotalSum = Dble(0)
Do Region = 1,NumRegions
  RegionSum = Dble(0)
  Do SubArea = 1,NumSubAreas
    CALL InitRecord(Region,Subarea,SubareaSummary(Region,Subarea))
    SubAreaSummary(Region,Subarea)%Area = Area(Region,Subarea)
    RegionSum = RegionSum+Area(Region,Subarea)
  EndDo !Subarea
  CALL InitRecord(Region,0,RegionalSummary(Region))
  RegionalSummary(Region)%Area = RegionSum
  RegionalSummary(Region)%YearsClosed = 9999
  TotalSum = TotalSum + RegionSum
EndDo !Region
CALL InitRecord(0,0,OverallSummary)
CALL InitRecord(2,-1,GBop)
CALL InitRecord(2,-2,GBcl)
OverallSummary%Area = TotalSum
OverallSummary%YearsClosed = 9999
OverallSummary%PctClosed = 0
ENDSUBROUTINE !IntializeSummaryRecords


SUBROUTINE InitRecord(Region,Subarea,Rec)
Integer, Intent(In) :: Region,Subarea
Type(PopSummaryRecord),Intent(InOut) :: Rec
Integer :: MCCat
 
    Rec%Bms = Dble(0)
    Rec%EBms = Dble(0)
    Rec%CatchNum = Dble(0)
    Rec%Catch = Dble(0)
    Rec%AvWt = Dble(0)
    Rec%AvEWt = Dble(0)
    Rec%BmsMT = Dble(0)
    Rec%EBmsMT = Dble(0)
    Rec%Eggs = Dble(0)
    Rec%AbsEggs = Dble(0)
    Rec%Growth = Dble(0)
    Rec%LPUE = Dble(0)
    Rec%CatchMT = Dble(0)
    Rec%Fbm = Dble(0)
    Rec%Fn = Dble(0)
    Rec%Recruits = Dble(0)
    Rec%ENum = Dble(0)
    Rec%ENum4 = Dble(0)
    Rec%MeatCounts = Dble(0)
    Rec%DAS = Dble(0)
    Rec%DredgeTime = Dble(0)
    Rec%DredgeArea = Dble(0)
    Rec%bycatch = Dble(0)
    Rec%YearsClosed = 0
    Rec%PctClosed= Dble(0)
    Rec%Region = Region
    Rec%Subarea = Subarea
ENDSUBROUTINE !InitRecord


SUBROUTINE WriteRegionalSummary(Run,NumRegions,NumYears)
Integer,Intent(In) :: Run,NumRegions,NumYears
Integer :: Year,Region

Do Region = 1,NumRegions
  Write (12,504) "Region",Region," Linf = ",MeanLinf(Region)," K = ",MeanK(Region)," M = ",MeanM(Region)
Enddo
Write(12,503) "Run#",Run
Write(12,*)
Do Region = 1,NumRegions
  Write(12,502) "Region ",Region
  Write (12,*) "Landings"
  Do Year = 1,NumYears
     Write(12,500) Landings(Region,Year)
  EndDo
  Write (12,*) "LPUE"
  Do Year = 1,NumYears
     Write(12,500) LPUE(Region,Year)
  EndDo
  Write (12,*) "Survey Bms      Expl Survey Bms"
  Do Year = 0,NumYears-1
     Write(12,500) Survey(Region,Year),ESurvey(Region,Year)
  EndDo
  Write (12,*) "Fishing mortality"
  Do Year = 1,NumYears
     Write(12,501) RegionalF(Region,Year)
  EndDo
  Write (12,*)
EndDo
Write(12,*)

500 Format(F11.1,2X,F11.1)
501 Format(F10.4)
502 Format(A6,1X,I2)
503 Format(A4,1X,I3)
504 Format (A7,I3,A7,F7.2,A5,F7.4,A5,F7.4)
ENDSUBROUTINE




SUBROUTINE CalcBins(FirstBin,SecondBin,PopVector,SimYear)
Real*8, Dimension(1:NumRegions,0:NumYears),Intent(InOut) :: FirstBin,SecondBin
Real*8, Intent(In) :: PopVector(1:NumClasses,0:NumRegions,0:NumSubareas)
Integer, Intent(In) :: SimYear
Real*8 :: RegionalMean, StartSecondBin, SH,FractionInSecondBin
Integer :: SizeClass,Region,Subarea,Year

Year = SimYear + 1
Do Region = 1,NumRegions
   FirstBin(Region,Year) = 0
   SecondBin(Region,Year) = 0
   StartSecondBin = Grow(StartFirstBin,Dble(1),MeanLinf(Region),MeanK(Region))
   !Write(12,*) Region," StartSecondBin ",StartSecondBin
   Do SizeClass = 1,NumClasses
     SH = StartingSH + ClassWidth*(SizeClass-1)     
     RegionalMean = PopVector(SizeClass,Region,0)
     if (SH >= StartSecondBin) then
       SecondBin(Region,Year) = SecondBin(Region,Year)+RegionalMean
     elseif (SH+ClassWidth > StartSecondBin) then
        FractionInSecondBin = (SH + ClassWidth - StartSecondBin)/ClassWidth
        !Write (12,*) Region,SH,FractionInSecondBin,StartSecondBin
        FirstBin(Region,Year) = FirstBin(Region,Year)+(1-FractionInSecondBin)*RegionalMean
        SecondBin(Region,Year) = SecondBin(Region,Year)+FractionInSecondBin*RegionalMean
     elseif (SH >= StartFirstBin) then
        FirstBin(Region,Year) = FirstBin(Region,Year)+RegionalMean
     elseif (SH+ClassWidth >= StartFirstBin) then
        FirstBin(Region,Year) = FirstBin(Region,Year)+RegionalMean*(SH+ClassWidth-StartFirstBin)/ClassWidth
     endif
    EndDo !SizeClass
EndDo !Region

ENDSUBROUTINE

SUBROUTINE CalcTwoBinF(FirstBin,SecondBin,TwoBinF)
Real*8, Dimension(1:NumRegions,0:NumYears), Intent(In) :: FirstBin,SecondBin
Real*8, Intent(Out) :: TwoBinF(1:NumRegions,1:NumYears)
Integer :: Year,Region
Real*8 :: MeanF,TwoBinSum,CatchBiomassSum,CatchBiomassF


Do Region = 1,NumRegions
  TwoBinSum = 0
  CatchBiomassSum = 0
  Do Year = 0,NumYears-1
    TwoBinSum = TwoBinSum + log(SecondBin(Region,Year+1)/(FirstBin(Region,Year)+SecondBin(Region,Year)))
    CatchBiomassSum = CatchBiomassSum + Landings(Region,Year+1)/ESurvey(Region,Year)
  EndDo
  CatchBiomassSum = CatchBiomassSum + Landings(Region,NumYears)/ESurvey(Region,NumYears-1)
  MeanF = -TwoBinSum/Dble(NumYears) - MeanM(Region)
  Write(12,*) "MeanF = ",MeanF,"CatchBmsF = ",CatchBiomassSum/Dble(NumYears)," StartFirstBin ",StartFirstBin
  CatchBiomassF = CatchBiomassSum/Dble(NumYears) 
  Do Year = 1,NumYears
     TwoBinF(Region,Year) = Landings(Region,Year)/ESurvey(Region,Year-1)*MeanF/CatchBiomassF
  EndDo
EndDo
ENDSUBROUTINE

SUBROUTINE WriteTwoBinF(FirstBin,SecondBin,TwoBinF)
Real*8, Dimension(1:NumRegions,1:NumYears),Intent(In) :: TwoBinF
Real*8, Dimension(1:NumRegions,0:NumYears), Intent(In) :: FirstBin,SecondBin
Real*8, Parameter :: eps = 1e-8
Integer :: Year,Region

Write(12,*)
Write (12,*) "Two Bin Fishing Mortality Estimates"
Write(12,*) "Region Year    FirstBin   SecondBin  TwoBinF  RescaledF    TrueF    %Error"
Do Region = 1,NumRegions
   Write(12,512) Region,StartingYear,FirstBin(Region,0),SecondBin(Region,0)
   Do Year = 1,NumYears-1
     if (RegionalF(Region,Year) > eps) then
      Write(12,510,advance='no') Region, Year+StartingYear
      Write(12,510,advance='no') FirstBin(Region,Year),SecondBin(Region,Year)
      Write(12,510,advance='no') -log(SecondBin(Region,Year)/(FirstBin(Region,Year-1)+SecondBin(Region,Year-1)))-MeanM(Region)
      Write(12,510,advance='no') TwoBinF(Region,Year),RegionalF(Region,Year)
      Write(12,510) (TwoBinF(Region,Year)-RegionalF(Region,Year))/RegionalF(Region,Year)
     else
      Write(12,511,advance='no') Region,Year+StartingYear,FirstBin(Region,Year)
      Write(12,511,advance='no') SecondBin(Region,Year)
      Write(12,511,advance='no') -log(SecondBin(Region,Year)/(FirstBin(Region,Year-1)+SecondBin(Region,Year-1)))-MeanM(Region)
      Write(12,511)              TwoBinF(Region,Year),RegionalF(Region,Year)
     endif
    Enddo
Enddo

510 Format (I3,3X,I5,1X,F10.1,1X,F10.1,1X,F10.4,1X,F10.4,1X,F10.4,1X,F10.5)
511 Format (I3,3X,I5,1X,F10.1,1X,F10.1,1X,F10.4,1X,F10.4,1X,F10.4)
512 Format (I3,3X,I5,1X,F10.1,1X,F10.1)
ENDSUBROUTINE


end program SAMS
