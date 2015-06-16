#include <stdio.h>
#include <math.h>
#include "common.h"
#include "bmp.h"
#include <stdlib.h>
#include <GL/freeglut.h>

#define DIM 512
#define blockSize 8

#define PI 3.1415926535897932f
#define centerX (DIM / 2)
#define centerY (DIM / 2)

float sourceColors[DIM * DIM];	// host memory for source image
float readBackPixels[DIM * DIM];	// host memory for swirled image

// Variable for memory allocation on the device
size_t size;

float* sourceDevPtr;			// device memory for source image
float* swirlDevPtr;				// device memory for swirled image

float userXCoord;						// user parameter for picture rotation on the X axis
float userYCoord;						// user parameter for picture rotation on the Y axis

__global__ void swirlKernel(float* sourcePtr, float* targetPtr, float userXCoord, float userYCoord)
{
	// TODO: Index berechnen	
	// blockDim(x, y, z) - number of threads in a block in each dimension
	// gridDim(x, y, z) - number of blocks in a grid in each dimension
	// blockIdx(x, y, z) - block index within a grid
	// threadIdx(x, y, z) - thread index within a block

	int tidX = threadIdx.x + blockIdx.x * blockDim.x;
	int tidY = threadIdx.y + blockIdx.y * blockDim.y;
	int index = tidX + tidY * blockDim.x * gridDim.x;

	// TODO: Den swirl invertieren.
	int relX = tidX - centerX;	// x position relative to center
	int relY = tidY - centerY;	// y position relative to center
	float originalAngle;		// angle before transformation, seen from a center based coordinate system

	if (relX != 0)
	{
		originalAngle = atan(((float)abs(relY) / (float)abs(relX)));

		if (relX > 0 && relY < 0)
			originalAngle = 2.0f * PI - originalAngle;
		else if (relX <= 0 && relY >= 0)
			originalAngle = PI - originalAngle;
		else if (relX <= 0 && relY < 0)
			originalAngle += PI;
	}
	else
	{
		// Take care of rare special case
		if (relY >= 0)
			originalAngle = 0.5f * PI;
		else
			originalAngle = 1.5f * PI;
	}

	// Calculate Rotation angle
	float r = sqrt((float)(relX * relX + relY * relY));
	float alpha = userXCoord * pow(r, userYCoord);

	float transformedAngle = originalAngle + alpha;

	// Transform in Pixel Coordinates
	int transX = (int)(floor(r * cos(transformedAngle) + 0.5f)) + centerX;
	int transY = (int)(floor(r * sin(transformedAngle) + 0.5f)) + centerY;

	// Clamping (handle edge cases of values less than or greater than the 
	// specified dimensions)
	if (transX < 0)
		transX = 0;
	if (transX >= DIM)
		transX = DIM - 1;
	if (transY < 0)
		transY = 0;
	if (transY >= DIM)
		transY = DIM - 1;

	// new Index
	int transIndex = transX + transY * blockDim.x * gridDim.x;

	targetPtr[transIndex] = sourcePtr[index];    // simple copy
}

void display(void)
{
	glClearColor(0.0f, 0.0f, 0.0f, 1.0f);
	glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);

	// TODO: Swirl Kernel aufrufen
	dim3 grid(blockSize * blockSize, blockSize * blockSize);
	dim3 block(blockSize, blockSize);
	swirlKernel<<<grid, block>>>(sourceDevPtr, swirlDevPtr, userXCoord, userYCoord);

	// TODO: Ergebnis zu host memory zuruecklesen.
	CUDA_SAFE_CALL(cudaMemcpy(readBackPixels, swirlDevPtr, size, cudaMemcpyDeviceToHost));

	// Ergebnis zeichnen (ja, jetzt gehts direkt wieder zur GPU zurueck...) 
	glDrawPixels(DIM, DIM, GL_LUMINANCE, GL_FLOAT, readBackPixels);

	glutSwapBuffers();
}

// clean up memory allocated on the GPU
void cleanup() {
	CUDA_SAFE_CALL(cudaFree(sourceDevPtr));
	CUDA_SAFE_CALL(cudaFree(swirlDevPtr));
}

void keyboard(int key, int x, int y)
{
	switch (key)
	{
		case GLUT_KEY_LEFT: userXCoord -= 0.01;
			if (userXCoord < -2) userXCoord = -2;
			printf("X-axis: %.2f , Y-axis: %.3f\r", userXCoord, userYCoord);
			break;
		case GLUT_KEY_RIGHT: userXCoord += 0.01;
			if (userXCoord > 2) userXCoord = 2;
			printf("X-axis: %.2f , Y-axis: %.3f\r", userXCoord, userYCoord);
			break;
		case GLUT_KEY_DOWN: userYCoord -= 0.001;
			if (userYCoord < 0) userYCoord = 0;
			printf("X-axis: %.2f , Y-axis: %.3f\r", userXCoord, userYCoord);
			break;
		case GLUT_KEY_UP: userYCoord += 0.001;
			if (userYCoord > 1) userYCoord = 1;
			printf("X-axis: %.2f , Y-axis: %.3f\r", userXCoord, userYCoord);
			break;
	}
}

int main(int argc, char **argv)
{
	glutInit(&argc, argv);
	glutInitDisplayMode(GLUT_DOUBLE | GLUT_RGB | GLUT_DEPTH);
	glutInitWindowSize(DIM, DIM);
	glutCreateWindow("Simple OpenGL CUDA");
	glutSpecialFunc(keyboard);
	glutIdleFunc(display);
	glutDisplayFunc(display);

	// load bitmap	
	Bitmap bmp = Bitmap("who-is-that.bmp");
	if (bmp.isValid())
	{
		for (int i = 0; i < DIM * DIM; i++)
		{
			sourceColors[i] = bmp.getR(i / DIM, i % DIM) / 255.0f;
		}
	}

	// Initalize parameters userXCoord and userYCoord
	userXCoord = 0.3;
	userYCoord = 0.7;

	// TODO: allocate memory at sourceDevPtr on the GPU and copy sourceColors into it.
	size = DIM * DIM * sizeof(float);
	CUDA_SAFE_CALL(cudaMalloc((void**)&sourceDevPtr, size));
	CUDA_SAFE_CALL(cudaMemcpy(sourceDevPtr, sourceColors, size, cudaMemcpyHostToDevice));

	// TODO: allocate memory at swirlDevPtr for the unswirled image.
	CUDA_SAFE_CALL(cudaMalloc((void**)&swirlDevPtr, size));

	glutMainLoop();

	cleanup();
}
