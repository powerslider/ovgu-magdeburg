//
uniform sampler2D imageTexture;
uniform mat4 Projection;

const float SCREEN_WIDTH = 512.0;

in vec2 texturePosition;

void main()
{
	// TODO: Farbe auslesen
	texturePosition = texturePosition / SCREEN_WIDTH;
	vec4 color = texture2D(imageTexture, texturePosition);

	// TODO: Grauwert berechnen
	// Formel: Grauwert = 0,299 * Rotanteil + 0,587 * Gruenanteil + 0,114 * Blauanteil -> Wikipedia
	float gray = 0.299 * color.x + 0.587 * color.y + 0.114 * color.z;

	// TODO: x-Position berechnen. Das Zielpixel ist zwischen (0,0) und (255,0)
	// da Viewport bei 512, 512
	// linke untere Bildschirmecke: x=0, y=0
	// und Grauwert zwischen [0,1]
	float x = gray * 255.0;

	// TODO: Die Position in [0,1] auf das Intervall [-1,1] abbilden.
	// da Clipping Space auch so begrenzt, Position einfach mit Projektionsmatrix umwandeln
	gl_Position = Projection *  vec4(x, 0.0, 0.0, 1.0); // w = 1, da Position ein Punkt im Raum ist
}