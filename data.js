var width = window.innerWidth * 0.9 - 20;
var height = width * 0.6;
if (width > 1200) {
  var width = 750;
}

console.log(width, height);

// This function listens to if there is a window size change
var globalResizeTimer_ut = null;

$(window).resize(function () {
  if (globalResizeTimer_ut != null) window.clearTimeout(globalResizeTimer_ut);
  globalResizeTimer_ut = window.setTimeout(function () {
    var width = window.innerWidth * 0.9 - 20;
    if (width > 1200) {
      var width = 750;
    }
    var height = width * 0.6;
    console.log(width, height);
  });
});

function navSlide() {
  var nav_selected = document.getElementById("nav-links");
  var navLinks = document.querySelectorAll(".nav-links li ");
  nav_selected.classList.toggle("nav-active");

  navLinks.forEach((link, i) => {
    link.style.animation = `navLinkFader 0.5s ease forwards ${i / 7 + 1}s`;
  });
}

var direction_func = d3
  .scaleOrdinal()
  .domain(["Up", "Down", "Same"])
  .range([
    "Cases are rising compared to the previous week",
    "Cases are falling compared to the previous week",
    "The case numbers are the same as the previous week",
  ]);

// Get data
var request = new XMLHttpRequest();
request.open("GET", "./Outputs/case_summary.json", false);
request.send(null);
var case_data = JSON.parse(request.responseText);

var request = new XMLHttpRequest();
request.open("GET", "./Outputs/trust_bed_summary.json", false);
request.send(null);
var bed_data = JSON.parse(request.responseText);

var se_bed_all = bed_data.filter(function (d) {
  return d.Name == "South East";
});

var request = new XMLHttpRequest();
request.open("GET", "./Outputs/trust_meta.json", false);
request.send(null);
var bed_data_publish_date = JSON.parse(request.responseText)[0].Date_published;

var request = new XMLHttpRequest();
request.open("GET", "./Outputs/mortality_summary.json", false);
request.send(null);
var mortality_data = JSON.parse(request.responseText);

// Which area to show
var areas_summary = [
  "West Sussex",
  "Adur",
  "Arun",
  "Chichester",
  "Crawley",
  "Horsham",
  "Mid Sussex",
  "Worthing",
  "South East region",
  "England",
];

// We need to create a dropdown button for the user to choose which area to be displayed on the figure.
d3.select("#select_summary_area_button")
  .selectAll("myOptions")
  .data(areas_summary)
  .enter()
  .append("option")
  .text(function (d) {
    return d;
  })
  .attr("value", function (d) {
    return d;
  });

// Daily cases bar chart

var svg_daily_case_bars = d3
  .select("#daily_case_bars")
  .append("svg")
  .attr("width", 100)
  .attr("height", 300)
  .append("g")
  .attr("transform", "translate(" + 60 + "," + 20 + ")");

function update_summary() {
  // Retrieve the selected area name
  var chosen_summary_area = d3
    .select("#select_summary_area_button")
    .property("value");

  chosen_case_data = case_data.filter(function (d) {
    return d.Name == chosen_summary_area && d.Age == "All ages";
  });

  chosen_death_data_all = mortality_data.filter(function (d) {
    return d.Name == chosen_summary_area && d.Type == "All places";
  });

  // Update text based on selected area
  d3.select("#what_am_i_showing").html(function (d) {
    return `Showing COVID-19 data for ${chosen_summary_area.replace(
      "West Sussex",
      " the whole county"
    )}`;
  });

  d3.select("#latest_seven_days_all_ages_1")
    .data(chosen_case_data)
    .html(function (d) {
      return (
        "<b class = 'highlight'>" +
        d3.format(",.0f")(d.Rolling_7_day_new_cases) +
        "</b> cases in the seven days to " +
        d.Rate_date +
        ". This is <b class = 'highlight'>" +
        d3.format(",.0f")(d.Rolling_7_day_new_cases_per_100000) +
        " </b>per 100,000 population."
      );
    });

  d3.select("#latest_seven_days_all_ages_2")
    .data(chosen_case_data)
    .html(function (d) {
      return direction_func(d.Change_direction);
    });

  d3.select("#latest_seven_days_all_ages_3")
    .data(chosen_case_data)
    .html(function (d) {
      return "This is for the 7 days to " + d.Rate_date;
    });

  chosen_case_data_60 = case_data.filter(function (d) {
    return d.Name == chosen_summary_area && d.Age == "60+ years";
  });

  d3.select("#latest_seven_days_60_plus_1")
    .data(chosen_case_data_60)
    .html(function (d) {
      return (
        "<b class = 'highlight'>" +
        d3.format(",.0f")(d.New_cases) +
        "</b> cases in last seven days.<br>This is a rate of <b class = 'highlight'>" +
        d3.format(",.0f")(d.Rolling_7_day_new_cases_per_100000) +
        "</b> per 100,000 population."
      );
    });

  d3.select("#latest_seven_days_60_plus_2")
    .data(chosen_case_data_60)
    .html(function (d) {
      return direction_func(d.Change_direction);
    });

  d3.select("#latest_seven_days_60_plus_3")
    .data(chosen_case_data_60)
    .html(function (d) {
      return "This is for the 7 days to " + d.Rate_date;
    });

  d3.select("#latest_covid_beds_1")
    .data(se_bed_all)
    .html(function (d) {
      return (
        "<b class = 'highlight'>" +
        d3.format(",.0f")(d.Patients_occupying_beds) +
        "</b> COVID-19 positive patients in hospital beds across the South East of England on " +
        d.Bed_date +
        ". Of these, <b class = 'highlight'>" +
        d3.format(",.0f")(d.Patients_occupying_mv_beds) +
        "</b> were occupying beds capable of mechanical ventilation."
      );
    });

  d3.select("#latest_covid_beds_2")
    .data(se_bed_all)
    .html(function (d) {
      return (
        "The number of people in hospital beds with COVID-19 has " +
        d.Change_direction +
        " compared to the 7 days before (" +
        d3.format(",.0f")(d.Previous_occupying_beds) +
        ")."
      );
    });

  d3.select("#latest_covid_beds_3").html(function (d) {
    return "This was updated on " + bed_data_publish_date;
  });

  d3.select("#daily_bars_text_1").html(function (d) {
    return (
      "The figure below shows the daily confirmed cases of Covid-19 over time in " +
      chosen_summary_area +
      ". The black line represents the rolling average number of new cases confirmed in the previous seven days.<br>It can take several days for results to be reported and we consider the last four days as incomplete as more cases may be added to the totals for these days."
    );
  });

  d3.select("#latest_covid_deaths_1")
    .data(chosen_death_data_all)
    .html(function (d) {
      return (
        "<b class = 'highlight'>" +
        d3.format(",.0f")(d.COVID_deaths_in_week) +
        "</b> COVID-19 deaths in " +
        d.Label +
        ". There have been <b class = 'highlight'>" +
        d3.format(",.0f")(d.COVID_deaths_cumulative) +
        " </b> deaths where COVID-19 was mentioned on death certificate in " +
        chosen_summary_area +
        " to date."
      );
    });
}

update_summary();

d3.select("#select_summary_area_button").on("change", function (d) {
  var chosen_summary_area = d3
    .select("#select_summary_area_button")
    .property("value");
  update_summary();
});
