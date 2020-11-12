function navSlide(){

var nav_selected = document.getElementById('nav-links');
var navLinks= document.querySelectorAll('.nav-links li ')
nav_selected.classList.toggle('nav-active');

navLinks.forEach((link, i) => {
  link.style.animation = `navLinkFader 0.5s ease forwards ${i / 7 + 1}s`
});


}
