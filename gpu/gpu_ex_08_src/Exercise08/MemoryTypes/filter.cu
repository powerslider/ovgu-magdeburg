
#include "common.h"
#include <stdlib.h>
#include <GL/freeglut.h>

#define DIM 512
#define blockSize 8
#define blurRadius 6
#define effectiveBlockSize (blockSize + 2 * blurRadius)

float sourceColors[DIM * DIM];
float readBackPixels[DIM * DIM];

texture<float, 2> blurDevTex;

float* sourceDevPtr;
float* transDevPtr;
float* blurDevPtr;

int timer = 0;

enum Mode
{
	NO_TRANSFORMATION,
	TRANSFORMATION,
	GLOBAL_MEMORY_BLUR,
	TEXTURE_MEMORY_BLUR,
	SHARED_MEMORY_BLUR
};
Mode mode = NO_TRANSFORMATION;

__global__ void transformation(float* sourcePtr, float* targetPtr, int timer) 
{
	int index = 0;
	int tidX = threadIdx.x + blockIdx.x + blockDim.x;		// thread x-coord in block
	int tidY = threadIdx.y + blockIdx.y + blockDim.y;		// thread y-coord in block
	index = (tidX + tidY) * blockDim.x * gridDim.x;		// block position in grid

	int transX = tidX;
	int transY = tidY;

	// define current x coord by getting the rest from
	// dividing with the current time 
	transX += timer % DIM;

	// Clamping: if x coord has values above DIM
	if (transX >= DIM)
	{
		// go to the next row of pixels by substracting DIM
		// and getting the new x coord
		transX -= DIM;
	}

	// define current y coord by getting the rest from
	// dividing with the current time 
	transY += timer % DIM;

	// Clamping: if y coord has values above DIM
	if (transY >= DIM)
	{
		// go to the next column of pixels by substracting DIM
		// and getting the new y coord
		transY -= DIM;
	}

	// get new index
	int transIndex = (transX + transY) * blockDim.x * gridDim.x;

	targetPtr[index] = sourcePtr[transIndex];    // simple copy
}

__global__ void globalMemoryBlur(float* sourcePtr, float* targetPtr)
{
	int tidX = threadIdx.x + blockIdx.x + blockDim.x;		// thread x-coord in block
	int tidY = threadIdx.y + blockIdx.y + blockDim.y;		// thread y-coord in block
	int index = (tidX + tidY) * blockDim.x * gridDim.x;		// block position in grid

	int filterWidth = blurRadius * 2 + 1;

	float median = 0.0f;

	int upperLeftFilterPosX = tidX - blurRadius;
	int upperLeftFilterPosY = tidY - blurRadius;

	for (int i = upperLeftFilterPosX; i < upperLeftFilterPosX + filterWidth; ++i)
	{
		for (int j = upperLeftFilterPosY; j < upperLeftFilterPosY + filterWidth; ++j)
		{
			if (i < DIM && j < DIM && i >= 0 && j >= 0)
			{
				// calculate index for neighboring pixel
				int sampleIndex = i + j * blockDim.x * gridDim.x;
				// add neighboring pixel's color in region of the radius
				median += sourcePtr[sampleIndex];
			}
		}
	}

	// get mean value 
	median /= filterWidth * filterWidth;

	targetPtr[index] = median;
}

__global__ void textureMemoryBlur(float* targetPtr)
{
	int tidX = threadIdx.x + blockIdx.x + blockDim.x;		// thread x-coord in block
	int tidY = threadIdx.y + blockIdx.y + blockDim.y;		// thread y-coord in block
	int index = (tidX + tidY) * blockDim.x * gridDim.x;		// block position in grid

	int filterWidth = blurRadius * 2 + 1;

	float median = 0.0f;

	int upperLeftFilterPosX = tidX - blurRadius;
	int upperLeftFilterPosY = tidY - blurRadius;

	for (int i = upperLeftFilterPosX; i < upperLeftFilterPosX + filterWidth; ++i)
	{
		for (int j = upperLeftFilterPosY; j < upperLeftFilterPosY + filterWidth; ++j)
		{
			if (i < DIM && j < DIM && i >= 0 && j >= 0)
			{
				median += tex2D(blurDevTex, j, i);
			}
		}
	}

	// get mean value 
	median /= filterWidth * filterWidth;

	targetPtr[index] = median;
}

__global__ void sharedMemoryBlur(float *sourcePtr, float *targetPtr)
{
	// calculate the position in source Image
	// therefore use blockSize not BlockDim.x
	int positionInImageX = blockIdx.x * blockSize + threadIdx.x - blurRadius;
	int positionInImageY = blockIdx.y * blockSize + threadIdx.y - blurRadius;

	__shared__ float cache[effectiveBlockSize * effectiveBlockSize];

	// fill the with values from global memory
	int getterIndex = positionInImageX + positionInImageY * DIM;

	if (0 <= positionInImageX && positionInImageX < DIM && 0 <= positionInImageY && positionInImageY < DIM)
	{
		cache[threadIdx.x + threadIdx.y * effectiveBlockSize] = sourcePtr[getterIndex];
	}
	else
	{
		cache[threadIdx.x + threadIdx.y * effectiveBlockSize] = 0.0f;
	}

	// synchronise all threads
	__syncthreads();

	// let all kernels run which have enough neighbors for mean calculation
	int kernelSizeRightSide = effectiveBlockSize - blurRadius;
	if (threadIdx.x >= blurRadius && threadIdx.x < kernelSizeRightSide && threadIdx.y >= blurRadius && threadIdx.y < kernelSizeRightSide)
	{
		float median = 0;
		for (int i = -blurRadius; i <= blurRadius; i++)
		{
			for (int j = -blurRadius; j <= blurRadius; j++)
			{
				median += cache[(threadIdx.x + j) + (threadIdx.y + i) * effectiveBlockSize];
			}
		}
		int filterWidth = blurRadius * 2 + 1;
		median /= filterWidth*filterWidth;
		targetPtr[positionInImageX + positionInImageY * DIM] = median;
	}
}

void keyboard(unsigned char key, int x, int y)
{
	switch (key)
	{
		case '1': 
			mode = TRANSFORMATION;
			break;
		case '2':
			mode = GLOBAL_MEMORY_BLUR;
			break;
		case '3':
			mode = TEXTURE_MEMORY_BLUR;
			break;
		case '4':
			mode = SHARED_MEMORY_BLUR;
			break;
	}
}

void display(void)	
{
	glClearColor(0.0f, 0.0f, 0.0f, 1.0f);
	glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);

	// TODO: Transformationskernel auf sourceDevPtr anwenden
	dim3 grid(DIM / blockSize, DIM / blockSize);
	dim3 block(blockSize, blockSize);

	dim3 sharedGrid(DIM / blockSize, DIM / blockSize);
	dim3 sharedBlock(effectiveBlockSize, effectiveBlockSize);

	if (mode == TRANSFORMATION)
	{
		timer += 1;
	}

	// TODO: Zeitmessung starten (see cudaEventCreate, cudaEventRecord)
	cudaEvent_t start, stop;
	cudaEventCreate(&start);
	cudaEventCreate(&stop);
	cudaEventRecord(start, 0);


	switch (mode)
	{
		case TRANSFORMATION: 
			transformation<<<grid, block>>>(sourceDevPtr, transDevPtr, timer); 
			break;
		case GLOBAL_MEMORY_BLUR: 
			globalMemoryBlur<<<grid, block>>>(transDevPtr, blurDevPtr); 
			break;
		case TEXTURE_MEMORY_BLUR: 
			textureMemoryBlur<<<grid, block>>>(blurDevPtr); 
			break;
		case SHARED_MEMORY_BLUR: 
			sharedMemoryBlur<<<sharedGrid, sharedBlock>>>(transDevPtr, blurDevPtr); 
			break;
	}

	// TODO: Zeitmessung stoppen und fps ausgeben (see cudaEventSynchronize, cudaEventElapsedTime, cudaEventDestroy)
	float elapsedTime;
	cudaEventRecord(stop, 0);
	cudaEventSynchronize(stop);
	cudaEventElapsedTime(&elapsedTime, start, stop);
	printf("Time to generate: %3.1f ms \r", elapsedTime);
	cudaEventDestroy(start);
	cudaEventDestroy(stop);

	// Ergebnis zur CPU zuruecklesen
	switch (mode)
	{
		case NO_TRANSFORMATION:
			CUDA_SAFE_CALL(cudaMemcpy(readBackPixels, 
				sourceDevPtr, 
				DIM * DIM * 4, 
				cudaMemcpyDeviceToHost));
			break;
		case TRANSFORMATION:
			CUDA_SAFE_CALL(cudaMemcpy(readBackPixels, 
				transDevPtr, 
				DIM * DIM * 4, 
				cudaMemcpyDeviceToHost));
			break;
		default: // bei Blur fuer restliche 3 Modi gleich
			CUDA_SAFE_CALL(cudaMemcpy(readBackPixels, 
				blurDevPtr, 
				DIM * DIM * 4, 
				cudaMemcpyDeviceToHost));
			break;
	}

	// Ergebnis zeichnen (ja, jetzt gehts direkt wieder zur GPU zurueck...) 
	glDrawPixels(DIM, DIM, GL_LUMINANCE, GL_FLOAT, readBackPixels);
	glutSwapBuffers();
}

// clean up memory allocated on the GPU
void cleanup() {
	CUDA_SAFE_CALL(cudaFree(sourceDevPtr));
	// TODO: Aufräumen zusätzlich angelegter Ressourcen.
	CUDA_SAFE_CALL(cudaUnbindTexture(blurDevTex));
	CUDA_SAFE_CALL(cudaFree(transDevPtr));
	CUDA_SAFE_CALL(cudaFree(blurDevPtr));
}

int main(int argc, char **argv)
{
	glutInit(&argc, argv);
	glutInitDisplayMode(GLUT_DOUBLE | GLUT_RGB | GLUT_DEPTH);
	glutInitWindowSize(DIM, DIM);
	glutCreateWindow("Memory Types");
	glutKeyboardFunc(keyboard);
	glutIdleFunc(display);
	glutDisplayFunc(display);

	// mit Schachbrettmuster füllen
	for (int i = 0 ; i < DIM * DIM ; i++) {

		int x = (i % DIM) / (DIM / 8);
		int y = (i / DIM) / (DIM / 8);

		if ((x + y) % 2)
			sourceColors[i] = 1.0f;
		else
			sourceColors[i] = 0.0f;
	}

	// alloc memory on the GPU
	CUDA_SAFE_CALL(cudaMalloc((void**)&sourceDevPtr, DIM * DIM * 4));
	CUDA_SAFE_CALL(cudaMemcpy(sourceDevPtr, sourceColors, DIM * DIM * 4, cudaMemcpyHostToDevice));

	// TODO: Weiteren Speicher auf der GPU für das Bild nach der Transformation und nach dem Blur allokieren.
	CUDA_SAFE_CALL(cudaMalloc((void**)&transDevPtr, DIM * DIM * 4));
	CUDA_SAFE_CALL(cudaMalloc((void**)&blurDevPtr, DIM * DIM * 4));

	// TODO: Binding des Speichers des Bildes an eine Textur mittels cudaBindTexture.
	cudaChannelFormatDesc desc = cudaCreateChannelDesc<float>();
	CUDA_SAFE_CALL(cudaBindTexture2D(nullptr, blurDevTex, transDevPtr, desc, DIM, DIM, DIM * 4));

	glutMainLoop();

	cleanup();
}
