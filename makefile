# Name of the project
PROJ_NAME=pxp_model
 
# module files
M_SOURCE := ./src/m_entity.f90 \
            ./src/m_brain.f90 \
            ./src/m_plant.f90 \
            ./src/m_animal.f90 \
            ./src/m_prey.f90 \
            ./src/m_predator.f90 \
            ./src/m_environment.f90 

# Object files
OBJ=$(subst .f90,.o,$(subst src,obj,$(M_SOURCE)))

# Compiler
CC=gfortran
 

#
# Compilation and linking
#

all: all_
	rm m_*.mod

all_: objFolder $(PROJ_NAME)

$(PROJ_NAME): $(OBJ)
	$(CC) -o ./$@ main.f90 $^;
 
./obj/%.o: ./src/%.f90
	$(CC) -c -o $@ $<
 
objFolder:
	@ mkdir -p obj

clean:
	rm -rf obj/*.o $(PROJ_NAME) *~
