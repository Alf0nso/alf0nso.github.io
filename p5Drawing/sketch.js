function setup() {
    createCanvas(windowWidth - 30, windowHeight - 30);
}

var x = 40;
var y = 260;
var speed1 = 4;
var speed2 = 3;

function draw() {
    background('black');
    stroke(82, 200, 200);
    strokeWeight(20);
    fill(82, 200, 200);
    triangle(x, 180, x, 390, y, 290);
    x = x + speed1;
    y = y + speed2;
    if (y > width || x < 0) {
	speed1 = speed1 * -1;
	speed2 = speed2 * -1;
    }
}
