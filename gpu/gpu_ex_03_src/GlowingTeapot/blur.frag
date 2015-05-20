uniform sampler2D texture;

void main()
{
	// Hier soll der Filter implementiert werden -> 2D Box Filter

	// Schrittweite fuer ein Pixel (bei Aufloesung 512)
	float texCoordDelta = 1. / 512.;

	// Filtergroesse (gesamt)
	int filterWidth = 55;

	// linker Ecke von Filter
	vec2 texCoord;
	texCoord.x = gl_TexCoord[0].s - (float(filterWidth / 2) * texCoordDelta); // s = x-Achse
	texCoord.y = gl_TexCoord[0].t - (float(filterWidth / 2) * texCoordDelta); // t = y-Achse

	// Wert zum Aufakkumulieren der Farbwerte
	vec3 val = vec3(0);

	for (int i = 0; i < filterWidth; i++)
	{
		for (int j = 0; j < filterWidth; j++)
		{
			//val = val + texture2D(texture, gl_TexCoord[0].st).xyz;
			val = val + texture2D(texture, vec2(texCoord.x, texCoord.y), 0.0).xyz;

			//TODO: Verschieben der Texturkoordinate -> naechstes Pixel in x Richtung
			texCoord.x += texCoordDelta;
		}
		// TODO: Zurücksetzen von texCoord.x und weiterschieben von texCoord.y
		texCoord.x = gl_TexCoord[0].s - (float(filterWidth / 2) * texCoordDelta);
		texCoord.y += texCoordDelta;
	}

	// Durch filterWidth^2 teilen, um zu normieren.
	val = 2.0 * val / float(filterWidth*filterWidth);

	// TODO: Ausgabe von val
	gl_FragColor.xyz = val; // da val Farbe, Ausgabe in Farbe

	// Die folgende Zeile dient nur zu Debugzwecken!
	// Wenn das Framebuffer object richtig eingestellt wurde und die Textur an diesen Shader übergeben wurde
	// wird die Textur duch den folgenden Befehl einfach nur angezeigt.
	//gl_FragColor.rgb = texture2D(texture,gl_TexCoord[0].st).xyz;
}