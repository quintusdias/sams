FC=gfortran
FCFLAGS=-g

all: sams25 normal_dataset

sams25: sams25.o mvnorm.o imsl_wrapper.o 
	${FC} -o sams25 sams25.o mvnorm.o imsl_wrapper.o

imsl_wrapper.o: imsl_wrapper.f90
	${FC} ${FCFLAGS} -c imsl_wrapper.f90

sams25.o: sams25.f90
	${FC} ${FCFLAGS} -c sams25.f90

mvnorm.o: mvnorm.f90
	${FC} ${FCFLAGS} -c mvnorm.f90

normal_dataset: normal_dataset.o mvnorm.o
	${FC} -o normal_dataset normal_dataset.o mvnorm.o

normal_dataset.o: normal_dataset.f90
	${FC} ${FCFLAGS} -c normal_dataset.f90

clean:
	rm *.o sams25 normal_dataset
