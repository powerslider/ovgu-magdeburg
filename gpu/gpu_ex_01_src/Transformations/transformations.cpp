// *** Transformationen

#include <math.h>
#include <GL/freeglut.h>

#define PI 3.141592f

#define ROTATE 1
#define MOVE 2

int width = 600;
int height = 600;

float theta = PI / 2.0f - 0.4f;
float phi = 0.0f;
float distance = 25.0f;
float oldX, oldY;
int motionState;

// Winkel, der sich kontinuierlich erhöht. (Kann für die Bewegungen auf den Kreisbahnen genutzt werden)
float angle = 0.0f;

float toDeg(float angle) { return angle / PI * 180.0f; }
float toRad(float angle) { return angle * PI / 180.0f; }

float cosLawDistance(float a, float b, float angle)
{
	return sqrt(a * a + b * b - 2 * a * b * cos(toRad(180 + angle)));
}

float cosLawAngle(float a, float b, float c, float angle)
{
	return toDeg(acos(
		(a * a - b * b - c * c) /
		(-2 * b * c)));
}

// Zeichnet einen Kreis mit einem bestimmten Radius und einer gewissen Anzahl von Liniensegmenten (resolution) in der xz-Ebene.
void drawCircle(float radius, int resolution)
{
	// Abschalten der Beleuchtung.
	glDisable(GL_LIGHTING);

	// TODO: Zeichnen eines Kreises. 
	// Nutzen Sie die Methoden glBegin, glEnd, glVertex3f und ggf. glColor3f um einen GL_LINE_STRIP zu rendern.
	float stepAngle = (float)(360 / resolution);
	glBegin(GL_LINE_STRIP);
	glColor3f(0, 0, 0);
	for (int i = 0; i <= resolution; i++) 
	{
		glVertex3f(sin(toRad(stepAngle * i)) * radius, 0, cos(toRad(stepAngle * i)) * radius);
	}
	glEnd();

	// Anschalten der Beleuchtung.
	glEnable(GL_LIGHTING);
}

void display(void)	
{
	// Buffer clearen
	glClearColor(1.0f, 1.0f, 1.0f, 1.0f);
	glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);

	// View Matrix erstellen
	glLoadIdentity();
	float x = distance * sin(theta) * cos(phi);
	float y = distance * cos(theta);
	float z = distance * sin(theta) * sin(phi);
	gluLookAt(x, y, z, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0);

	// Teekanne rendern.
	glutSolidTeapot(1);

	// TODO: Den Matrix-Stack sichern.	
	glPushMatrix();
	// TODO: Zeichnen der Kugelkreisbahn.

	float bigCircleRadius = 10.0f;
	drawCircle(bigCircleRadius, 40);
		
	// TODO: Zeichnen der Kugel.
		// Wenden Sie eine Translation und eine Rotation an, bevor sie die Kugel zeichnen. Sie können die Variable 'angle' für die Rotation verwenden.
		// Bedenken Sie dabei die richtige Reihenfolge der beiden Transformationen.
	glRotatef(angle, 0, 1, 0);
	glTranslatef(10, 0, 0);
	glutSolidSphere(1, 30, 10);
		
	// TODO: Zeichnen der Würfelkreisbahn.
		// Hinweis: Der Ursprung des Koordinatensystems befindet sich nun im Zentrum des Würfels.
		// Drehen Sie das Koordinatensystem um 90° entlang der Achse, die für die Verschiebung des Würfels genutzt wurde.
		// Danach steht die Würfelkreisbahn senkrecht zur Tangentialrichtung der Kugelkreisbahn.
	
	float verticalCircleRadius = 5.0f;
	glRotatef(90, 1, 0, 0);
	drawCircle(verticalCircleRadius, 40);

	// TODO: Zeichnen des Würfels.
		// Wenden Sie die entsprechende Translation und Rotation an, bevor sie den Würfel zeichnen.
	glRotatef(angle, 0, 1, 0);
	glTranslatef(5, 0, 0);
	glutSolidCube(1);

	// TODO: Zeichnen einer Linie von Würfel zu Kegel.
	glColor3f(1, 0, 0);
	glRotatef(-90, 0, 0, 1);
	float lineCubeConeLength = 3.0f;
	glBegin(GL_LINE_STRIP);
	glVertex3f(0, 0, 0);
	glVertex3f(0, lineCubeConeLength, 0);
	glEnd();

	//glTranslatef(0, 3, 0);

				
		// TODO: Drehung anwenden, sodass Koordinatensystem in Richtung Ursprung orientiert ist. (Hinweis: Implementieren Sie dies zuletzt.)		
		
		// TODO: Zeichnen der Linie von Kegel zu Urpsrung.	
	
	// TODO: Zeichnen des Kegels.
	float lineBigCircleConeLength = verticalCircleRadius + lineCubeConeLength;

	/*float distTeepot = sqrt(bigCircleRadius * bigCircleRadius + lineBigCircleConeLength * lineBigCircleConeLength -
		2 * bigCircleRadius * lineBigCircleConeLength * cos(toRad(180 + angle)));*/
	float distTeepot = cosLawDistance(bigCircleRadius, lineBigCircleConeLength, angle);

	/*float coneAngle = toDeg(acos(
		(bigCircleRadius * bigCircleRadius - lineBigCircleConeLength * lineBigCircleConeLength - distTeepot * distTeepot) / 
		(-2 * lineBigCircleConeLength * distTeepot)));*/
	float coneAngle = cosLawAngle(bigCircleRadius, lineBigCircleConeLength, distTeepot, angle);

	if ((int)angle % 360 >= 180) coneAngle *= -1;
	glTranslatef(0, 3, 0);
	glRotatef(90 + coneAngle, 1, 0, 0);
	glutSolidCone(0.5, 1, 30, 30);

	glColor3f(1, 0, 0);
	glBegin(GL_LINE_STRIP);
	glVertex3f(0, 0, 0);
	glVertex3f(0, 0, distTeepot);
	glEnd();
		
	// TODO: Den Matrix-Stack wiederherstellen.	
	glPopMatrix();
	
	glFlush();
	glutSwapBuffers();	

	angle += 45.0f / 60.0f;
}

void mouseMotion(int x, int y)
{
	float deltaX = x - oldX;
	float deltaY = y - oldY;
	
	if (motionState == ROTATE) {
		theta -= 0.01f * deltaY;

		if (theta < 0.01f) theta = 0.01f;
		else if (theta > PI/2.0f - 0.01f) theta = PI/2.0f - 0.01f;

		phi += 0.01f * deltaX;	
		if (phi < 0) phi += 2*PI;
		else if (phi > 2*PI) phi -= 2*PI;
	}
	else if (motionState == MOVE) {
		distance += 0.01f * deltaY;
	}

	oldX = (float)x;
	oldY = (float)y;

	glutPostRedisplay();

}

void mouse(int button, int state, int x, int y)
{
	oldX = (float)x;
	oldY = (float)y;

	if (button == GLUT_LEFT_BUTTON) {
		if (state == GLUT_DOWN) {
			motionState = ROTATE;
		}
	}
	else if (button == GLUT_RIGHT_BUTTON) {
		if (state == GLUT_DOWN) {
			motionState = MOVE;
		}
	}
}

void idle(void)
{
	glutPostRedisplay();
}


int main(int argc, char **argv)
{
	glutInit(&argc, argv);
	glutInitDisplayMode(GLUT_DOUBLE | GLUT_RGB | GLUT_DEPTH);
	glutInitWindowSize(width, height);
	glutCreateWindow("Transformationen");

	glutDisplayFunc(display);
	glutMotionFunc(mouseMotion);
	glutMouseFunc(mouse);
	glutIdleFunc(idle);

	glEnable(GL_LIGHTING);
	glEnable(GL_LIGHT0);
	
	glEnable(GL_DEPTH_TEST);

	glViewport(0,0,width,height);					
	glMatrixMode(GL_PROJECTION);					
	glLoadIdentity();								

	gluPerspective(45.0f,(GLfloat)width/(GLfloat)height,0.1f,100.0f);

	glMatrixMode(GL_MODELVIEW);
	glLoadIdentity();

	glutMainLoop();
	return 0;
}

