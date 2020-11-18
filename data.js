function navSlide(){

var nav_selected = document.getElementById('nav-links');
var navLinks= document.querySelectorAll('.nav-links li ')
nav_selected.classList.toggle('nav-active');

navLinks.forEach((link, i) => {
  link.style.animation = `navLinkFader 0.5s ease forwards ${i / 7 + 1}s`
});


}

var direction_func = d3.scaleOrdinal()
  .domain(['Up', 'Down', 'Same'])
  .range(['Cases are rising compared to the previous week', 'Cases are falling compared to the previous week', 'The case numbers are the same as the previous week'])

// Get data
var request = new XMLHttpRequest();
request.open("GET", "./Outputs/case_summary.json", false);
request.send(null);
var case_data = JSON.parse(request.responseText);

// Which area to show
var areas_summary = ['West Sussex', 'Adur', 'Arun', 'Chichester', 'Crawley', 'Horsham', 'Mid Sussex', 'Worthing', 'South East region', 'England']

// We need to create a dropdown button for the user to choose which area to be displayed on the figure.
d3.select("#select_summary_area_button")
  .selectAll('myOptions')
  .data(areas_summary)
  .enter()
  .append('option')
  .text(function(d) {
    return d;
  })
  .attr("value", function(d) {
    return d;
  })


function update_summary(){
// Retrieve the selected area name
var chosen_summary_area = d3.select('#select_summary_area_button').property("value")

chosen_case_data = case_data.filter(function(d) {
  return d.Name == chosen_summary_area &&
         d.Age == 'All ages'
})

// Update text based on selected area
d3.select("#what_am_i_showing")
  .html(function(d) {
    return 'Showing data for ' + chosen_summary_area.replace('West Sussex',' the whole county')
  });

d3.select('#latest_seven_days_all_ages_1')
  .data(chosen_case_data)
  .html(function(d) {
    return d3.format(',.0f')(d.Rolling_7_day_new_cases) + ' cases in the seven days to ' + d.Rate_date + '. This is ' + d3.format(',.0f')(d.Rolling_7_day_new_cases_per_100000) + ' per 100,000 population.'})

d3.select('#latest_seven_days_all_ages_2')
  .data(chosen_case_data)
  .html(function(d) {
    return direction_func(d.Change_direction)})

chosen_case_data_60 = case_data.filter(function(d) {
  return d.Name == chosen_summary_area &&
         d.Age == '60+ years'
})

d3.select('#latest_seven_days_60_plus_1')
  .data(chosen_case_data_60)
  .html(function(d) {
    return d3.format(',.0f')(d.New_cases) + ' cases in last seven days.<br>This is a rate of ' + d3.format(',.0f')(d.Rolling_7_day_new_cases_per_100000) + ' per 100,000 population.'})

d3.select('#latest_seven_days_60_plus_2')
  .data(chosen_case_data_60)
  .html(function(d) {
  return direction_func(d.Change_direction)})


}

update_summary()

d3.select("#select_summary_area_button").on("change", function(d) {
  var chosen_summary_area = d3.select('#select_summary_area_button').property("value")
  update_summary()
})
