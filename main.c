
#include <stdlib.h>
#include <stdio.h>
#include<string.h>
#include <math.h>
#include<time.h>
#include <mpi.h>
#define TAMANO 15000

double** matrix_create(int ancho, int largo){
    int i;
    double** matrix = (double**) malloc (largo * sizeof(double*)); //array para largo
    for(i=0;i<largo;i++){
        matrix[i]= (double*) malloc (ancho * sizeof(double)); //array de ancho para cada largo 
    }
    
    return matrix;
    
}
void matrixxvector(double** matrix, double* vect_origen, double* vect_destino, int N, int div){

    int i,j;
    double var=0;
    for(i=0;i<div;i++){ //Fila
        var=0;
        for(j=0;j<N;j++){//columna
        var+=vect_origen[j]*matrix[i][j];
        }
        vect_destino[i]=var; //utilizamos el mismo iterador que utilizamos para recorrer la matrix fila a fila para recorrer el vector destino elemento a elemeto
    }
}
//Funcion que crea vectores de tamaño N y los inicializa a un valor ini que le pasaremos por valor
double* vector_create(int N, double ini){
    double* vector;
    int i;
    vector= (double*) malloc (N * sizeof(double)); 
    for(i=0;i<N;i++)
    vector[i]=ini;

    return vector;
}

void matrix_rellena(double** matrix, int N, char* fichero){
    int i,j;
    time_t t;
	FILE *file;
    if (file = fopen (fichero, "rb")){
        for(i=0;i<N;i++){	     
            fread(matrix[i],sizeof(double), N, file);	    	
        }
        fclose(file);
		
    }
	else{
		//CODIGO DE COMPROBACION
		/*for (i=0;i<N;i++){

			for (j=0;j<N;j++){
				if (i == j){
					matrix[i][j] = 1.0;
				}
				else if(i > j){
					matrix[i][j]=(double)50*(i+1)*(j+1)/(N+N); //Triangular superior
				}
				else{
					matrix[i][j]=(double)-50*(i+1)*(j+1)/(N+N); //Triangular inferior
				}

			}
	
		}*/
	
		for(i=0;i<N;i++){ //Fila matrix
			for(j=0;j<N;j++){ //recorre filas
				if(i==j){ //diagonal
					matrix[i][j]=1;
				}
				else if(i>j){//Diagonal inferior
					matrix[i][j]= rand() / ((double) RAND_MAX)*-50;
				}
				else{//Diagonal superior
					matrix[i][j]= rand() / ((double) RAND_MAX)*50; 
				}
			}
		}
		
		file = fopen (fichero, "wb"); 
        for(i=0;i<N;i++){	     
            fwrite(matrix[i],sizeof(double), N, file);
		}
		
	}
}

void dividiendo(double* vector,int N,double divisor){
	int i;
    for(i=0;i<N;i++){
        vector[i]=vector[i]/divisor;
    }
}

double calcular_abs(double* vect, int N){
int i;
    double temp=0;
    double valor_literal=25.0;
    

    //Recorre el vector y encuentra el máximo valor absoluto
    for(i=0;i<N;i++){
        if(sqrt(pow(vect[i],2))>temp){
            temp=sqrt(pow(vect[i],2));
        }
    }
    //divide el vector por el maximo valor absoluto
    for(i=0;i<N;i++){

        //lo comparamos con el valor literal 
        if (vect[i]>valor_literal)
        {
        vect[i]/temp;
        }
        
    }
    return temp;

	

}

int comp_fichero (char *fichero){
    FILE *f= fopen(fichero,"r");
    if (f==NULL){
        return 0;
        //Si no es un fichero devuelve 0 
    }
    fclose(f);
    return 1;
        //Si es un fichero devuelve 1
}

void ImpResultados(int itera,int nproces,double* resultados, double tt, double t_com_ej, double te,char* fichero){
    char nombre[50];
    int contador=0;
    FILE *f;
    int i;
    
    sprintf(nombre,"Resultados_itera_%d.txt",itera);
     while(comp_fichero(nombre)==1){
        contador++;
        sprintf(nombre,"Resultados_itera_%d(%d).txt",itera,contador);
    }
   

    f=fopen(nombre,"w");
    fprintf(f,"Numero de iteraciones: %d\n\n",itera);
	fprintf(f,"Numero de procesos: %d\n\n",nproces);
    fprintf(f,"La primera iteracion no genera valor absoluto\n");
    for(i=0;i<itera-1;i++){
        fprintf(f,"Mayor absoluto iteración %d: ",i+2);
        fprintf(f,"%f  ",resultados[i]);
        fprintf(f,"\n");
    }

    fprintf(f,"\n\nTiempo paralelo considerando tratamiento de ficheros, comunicaciones iniciales y ejecución es %f\n",tt);
	fprintf(f,"Tiempo paralelo considerando comunicaciones iniciales y ejecución es %f\n",t_com_ej);
    fprintf(f,"Tiempo paralelo considerando solo la ejecucion: %f\n",te);
	fprintf(f,"El fichero de entrada y de salida es %s\n",fichero);
	

    fclose(f);
}  

int main(int argc, char* argv[]){

	MPI_Init(&argc,&argv);
	int i,j,k,myrank, nproces,err;
	MPI_Status status;
	MPI_Comm_rank(MPI_COMM_WORLD,&myrank);
	MPI_Comm_size(MPI_COMM_WORLD,&nproces);
	srand(time(NULL));
	//Variables de ejecucion
	int extra= TAMANO%nproces;
	int div;
	double** trozo; //Trozo de matrix que utilizarán todos los procesos
	double** M; //matrix incial
	double absolut; //Variable que almacena absoluto
	int itera=atoi(argv[1]); //Numero de iteraciones
	double* fragmento; //fragmento   de resultados de cada matrix
	double* absolutos; //Array donde el 0 almacena los absolutos que le envian el resto de procesos
	double* V0 = vector_create(TAMANO,1); //Vector V0 que utilizan los procesos para comunicarse y matrixxvectorplicar por la matrix
	char* fichero=argv[2]; //Fichero que leera o almacenará la matrix
	
	
	// Variables del gather
	int gather_desp[nproces];
	int gather_num[nproces];
	
	
	div= TAMANO/nproces;
	
	
	//Variables de resultados
	double* resultados;
	double t_com_ini0, t_com_ini1,tt,te,t_ej,t_com_ej0,t_com_ej;
	
	//Array de numero de datos que enviara el gather
	for(i=0;i<nproces;i++){
		if(i==nproces-1){
			gather_num[i]=div+extra;
		}else{
		gather_num[i]=div;
		}
	}
	
	//Array de desplazamientos del gather
	gather_desp[0]=0;
	for(i=1;i<nproces;i++){
		gather_desp[i]=div*i;
	}
	
	if(myrank==nproces-1){
		div+= extra;
	}
		trozo= matrix_create(TAMANO,div);
		fragmento=vector_create(div,2);
		
		
	 t_com_ini0= MPI_Wtime();
	
	if(myrank==0){
		M= matrix_create(TAMANO,TAMANO);
		matrix_rellena(M,TAMANO,fichero);
		absolutos= vector_create(nproces,0);
		resultados=vector_create(itera-1,0);
		
			
		 for(i=0;i<div;i++){
			memcpy(trozo[i],M[i],TAMANO* sizeof(double));
		}
		t_com_ej0= MPI_Wtime();
		if(nproces>1){
			
			//Enviar los fragmentomentos al resto de procesos
			for(j=1; j<nproces-1; j++){
				for(i=0; i<div; i++){
				MPI_Send(M[div * j + i],TAMANO,MPI_DOUBLE, j, 99,MPI_COMM_WORLD);
				}
			}
			for(i=0; i<div+extra; i++){
				
					MPI_Send(M[div *(nproces-1)+ i],TAMANO,MPI_DOUBLE, (nproces-1), 99, MPI_COMM_WORLD);
			}
			
		}
		
		free(M);

	}
	else{
		for(i=0; i<div; i++){
		MPI_Recv(&trozo[i][0],TAMANO, MPI_DOUBLE, 0, 99,MPI_COMM_WORLD ,&status);
		}
	}
	t_com_ini1= MPI_Wtime();
	
	matrixxvector(trozo,V0,fragmento,TAMANO,div);
	
	err=MPI_Gatherv(fragmento,div,MPI_DOUBLE,V0,gather_num,gather_desp,MPI_DOUBLE,0,MPI_COMM_WORLD);
	
	for(k=1;k<itera;k++){
		
		if(myrank==0){
			for(i=1;i<nproces;i++)
			MPI_Send(V0,TAMANO,MPI_DOUBLE,i,99,MPI_COMM_WORLD);
		}
		else{
			MPI_Recv(V0,TAMANO,MPI_DOUBLE,0,99,MPI_COMM_WORLD, &status);
		}
		
		matrixxvector(trozo,V0,fragmento,TAMANO,div);
		
		absolut=calcular_abs(fragmento,div);
		
		if(myrank!=0){
			MPI_Send(&absolut,1,MPI_DOUBLE,0,99,MPI_COMM_WORLD);
			MPI_Bcast(&absolut,1,MPI_DOUBLE,0,MPI_COMM_WORLD);
		}
		else{
			absolutos[0]=absolut;
			for(i=1;i<nproces;i++){
			MPI_Recv(&absolutos[i],1,MPI_DOUBLE,i,99,MPI_COMM_WORLD,&status);
			}
			absolut=calcular_abs(absolutos,nproces);
			resultados[k-1]=absolut;
			MPI_Bcast(&absolut,1,MPI_DOUBLE,0,MPI_COMM_WORLD);
		}
		
		dividiendo(fragmento,div,absolut);
		err=MPI_Gatherv(fragmento,div,MPI_DOUBLE,V0,gather_num,gather_desp,MPI_DOUBLE,0,MPI_COMM_WORLD);
		
		
		
	}
	t_ej= MPI_Wtime();
	if(myrank==0){
		tt=t_ej-t_com_ini0;
		te=t_ej-t_com_ini1;
		t_com_ej=t_ej-t_com_ej0;
		ImpResultados(itera,nproces,resultados,tt,t_com_ej,te,fichero);
	}
	
	MPI_Finalize();
	exit(0);

}

