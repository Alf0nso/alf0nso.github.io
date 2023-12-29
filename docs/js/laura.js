let img; // variable to hold the image
let scaleFactor = 2; // Factor to scale the image and canvas size
let particleSize = 3; // Size of the dots (adjust to a larger value for fewer particles)
let particles = []; // Array to store particle objects

function preload() {
    // Load the image

    img = loadImage('/images/laururiruSinFShort.png');
}

function setup() {
    var canvas = createCanvas(img.width * scaleFactor, img.height * scaleFactor); // Create an enlarged canvas
    canvas.parent("laura");
    img.loadPixels(); // Load image pixels

    // Loop through the pixels of the image with increased increments
    for (let y = 0; y < img.height; y += particleSize) {
	for (let x = 0; x < img.width; x += particleSize) {
	    let index = (x + y * img.width) * 4; // Get the pixel index

	    // Extract color information (for grayscale images)
	    let r = img.pixels[index];
	    let g = img.pixels[index + 1];
	    let b = img.pixels[index + 2];
	    
	    // Calculate brightness
	    let brightness = (r + g + b) / 3;
	    
	    // Invert brightness for particle size
	    let invertedBrightness = map(brightness, 135, 255, 350, 170);

	    // Map inverted brightness to particle size
	    let size = map(invertedBrightness, 0, 255, 1, particleSize * 1.5);

	    // Create a particle object at (x, y) with the determined size and initial position
	    let particle = {
		x: x * scaleFactor,
		y: y * scaleFactor,
		size: size,
		originalX: x * scaleFactor, // Store the original position
		originalY: y * scaleFactor,
	    };

	    // Add the particle to the particles array
	    particles.push(particle);
	}
    }
}

function draw() {
    background(255); // Clear background

    // Display particles
    for (let i = 0; i < particles.length; i++) {
	let p = particles[i];
	fill(0); // Set dot color
	noStroke();
	ellipse(p.x, p.y, p.size, p.size); // Draw dot at particle position

	// Calculate distance between particle and mouse
	let mouseDistX = mouseX - p.x;
	let mouseDistY = mouseY - p.y;
	let distance = dist(mouseX, mouseY, p.x, p.y);

	// If the particle is close to the mouse, move away from it (with reduced movement)
	if (distance < 30) {
	    p.x -= 2 * (mouseDistX / distance); // Reduced movement
	    p.y -= 2 * (mouseDistY / distance); // Reduced movement
	} else {
	    // Move back to original position if the mouse is not nearby (with reduced movement)
	    let returnDistX = p.originalX - p.x;
	    let returnDistY = p.originalY - p.y;
	    p.x += returnDistX * 0.05; // Adjusted return speed (reduced movement)
	    p.y += returnDistY * 0.05; // Adjusted return speed (reduced movement)
	}
    }
}
