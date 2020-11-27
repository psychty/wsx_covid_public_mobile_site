// var width = window.innerWidth * 0.9 - 20;
var width = document.getElementById("daily_case_bars").offsetWidth;
var height = document.getElementById("daily_case_bars").offsetHeight;
if (width > 900) {
  var width = 900;
}
var width_margin = width * 0.12;

var complete_colour_func = d3
  .scaleOrdinal()
  .domain(["Complete", "Considered incomplete"])
  .range(["#071b7c", "#ff5f07"]);

// This function listens to if there is a window size change
var globalResizeTimer_ut = null;

// ! If there is a window size change then re run update summary to resize some of the elements
$(window).resize(function () {
  if (globalResizeTimer_ut != null) window.clearTimeout(globalResizeTimer_ut);
  globalResizeTimer_ut = window.setTimeout(function () {
    update_summary();
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
    "However, cases are rising compared to the previous week",
    "However, cases are falling compared to the previous week",
    "The case numbers are the same as in the previous week",
  ]);

// ! Get date label data
var request = new XMLHttpRequest();
request.open("GET", "./Outputs/case_dates.json", false);
request.send(null);
var case_dates_df = JSON.parse(request.responseText);

var latest_date = case_dates_df.filter(function (d) {
  return d.Item === "latest_daily_case";
})[0]["Label"];

var data_refreshed_date = case_dates_df.filter(function (d) {
  return d.Item === "daily_case_update_date";
})[0]["Label"];

var complete_date = case_dates_df.filter(function (d) {
  return d.Item === "complete_date";
})[0]["Label"];

var first_case_period = case_dates_df.filter(function (d) {
  return d.Item === "first_case_period";
})[0]["Label"];

var last_case_period = case_dates_df.filter(function (d) {
  return d.Item === "last_case_period";
})[0]["Label"];

// Update text based on selected area
d3.select("#update_date").html(function (d) {
  return (
    "The case data was last refreshed on " +
    data_refreshed_date +
    " with cases confirmed up to " +
    latest_date +
    ". However, as it can take a few days for test results to be confirmed, we look at the 7 days up to " +
    complete_date +
    "."
  );
});

// ! Get data
var request = new XMLHttpRequest();
request.open("GET", "./Outputs/case_summary.json", false);
request.send(null);
var case_summary_data = JSON.parse(request.responseText);

var request = new XMLHttpRequest();
request.open("GET", "./Outputs/daily_cases.json", false);
request.send(null);
var case_data = JSON.parse(request.responseText);

var request = new XMLHttpRequest();
request.open("GET", "./Outputs/trust_bed_summary.json", false);
request.send(null);
var bed_data = JSON.parse(request.responseText);

var request = new XMLHttpRequest();
request.open("GET", "./Outputs/trust_meta.json", false);
request.send(null);
var bed_data_publish_date = JSON.parse(request.responseText)[0].Date_published;

var request = new XMLHttpRequest();
request.open("GET", "./Outputs/mortality_summary.json", false);
request.send(null);
var mortality_data = JSON.parse(request.responseText);

// * Which area to show
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

// For hospital admissions we should stick to the region
var se_bed_all = bed_data.filter(function (d) {
  return d.Name == "South East";
});

// ! Daily cases bar chart

var svg_daily_case_bars = d3
  .select("#daily_case_bars")
  .append("svg")
  .attr("width", width) // This compensates for the 25px margin styling
  .attr("height", height)
  .append("g")
  .attr("transform", "translate(" + width_margin + "," + 0 + ")");

var x_daily_cases = d3
  .scaleBand()
  .domain(
    case_data.map(function (d) {
      return d.Period;
    })
  )
  .range([0, width - width_margin]);

var xAxis_daily_cases = svg_daily_case_bars
  .append("g")
  .attr("transform", "translate(0," + (height - 40) + ")")
  .call(
    d3
      .axisBottom(x_daily_cases)
      .tickValues([first_case_period, last_case_period])
  );

// This will give the first tick start and the second one end text anchor points
xAxis_daily_cases.selectAll("text").style("text-anchor", function (d, i) {
  return i % 2 ? "end" : "start";
});

// We need to create the first figure scale
var chosen_summary_area = d3
  .select("#select_summary_area_button")
  .property("value");

var bars_daily_cases_chosen = case_data.filter(function (d) {
  return d.Name === chosen_summary_area && d.Age === "All ages";
});

max_limit_y_1 = d3.max(bars_daily_cases_chosen, function (d) {
  return +d.Cases;
});

var y_daily_cases = d3
  .scaleLinear()
  .domain([0, max_limit_y_1])
  .range([height - 40, 10])
  .nice();

var yAxis_daily_cases = svg_daily_case_bars
  .append("g")
  .call(d3.axisLeft(y_daily_cases));

// ! daily case bars
var daily_new_case_bars = svg_daily_case_bars
  .selectAll("mybar")
  .data(bars_daily_cases_chosen)
  .enter()
  .append("rect")
  .attr("x", function (d) {
    return x_daily_cases(d.Period);
  })
  .attr("y", function (d) {
    return y_daily_cases(d.Cases);
  })
  .attr("width", x_daily_cases.bandwidth())
  .attr("height", function (d) {
    return height - 40 - y_daily_cases(d.Cases);
  })
  .attr("fill", function (d) {
    return complete_colour_func(d.Data_completeness);
  })
  .style("opacity", 0.75);

var daily_average_case_bars = svg_daily_case_bars
  .append("path")
  .datum(bars_daily_cases_chosen)
  .attr("fill", "none")
  .attr("stroke", "#000000")
  .attr("stroke-width", 2)
  .attr(
    "d",
    d3
      .line()
      .defined((d) => !isNaN(d.Rolling_7_day_average_new_cases))
      .x(function (d) {
        return x_daily_cases(d.Period) + x_daily_cases.bandwidth() / 2;
      })
      .y(function (d) {
        return y_daily_cases(d.Rolling_7_day_average_new_cases);
      })
  );

function update_summary() {
  var width = document.getElementById("daily_case_bars").offsetWidth;
  var height = document.getElementById("daily_case_bars").offsetHeight;
  if (width > 900) {
    var width = 900;
  }
  var width_margin = width * 0.1;
  // console.log(width, height, width_margin);

  // Retrieve the selected area name
  var chosen_summary_area = d3
    .select("#select_summary_area_button")
    .property("value");

  chosen_case_summary_data = case_summary_data.filter(function (d) {
    return d.Name == chosen_summary_area && d.Age == "All ages";
  });

  chosen_case_summary_data_60 = case_summary_data.filter(function (d) {
    return d.Name == chosen_summary_area && d.Age == "60+ years";
  });

  chosen_death_data_all = mortality_data.filter(function (d) {
    return d.Name == chosen_summary_area && d.Type == "All places";
  });

  // * Update text based on selected area
  d3.select("#what_am_i_showing").html(function (d) {
    return `Showing COVID-19 data for ${chosen_summary_area.replace(
      "West Sussex",
      " the whole county"
    )}`;
  });

  var bars_daily_cases_chosen = case_data.filter(function (d) {
    return d.Name === chosen_summary_area && d.Age === "All ages";
  });

  max_limit_y_1 = d3.max(bars_daily_cases_chosen, function (d) {
    return +d.Cases;
  });

  y_daily_cases.domain([0, max_limit_y_1]).nice();

  // Redraw axis
  yAxis_daily_cases
    .transition()
    .duration(1000)
    .call(d3.axisLeft(y_daily_cases));

  daily_new_case_bars
    .data(bars_daily_cases_chosen)
    .transition()
    .duration(1000)
    .attr("x", function (d) {
      return x_daily_cases(d.Period);
    })
    .attr("y", function (d) {
      return y_daily_cases(d.Cases);
    })
    .attr("height", function (d) {
      return height - 40 - y_daily_cases(d.Cases);
    })
    .attr("fill", function (d) {
      return complete_colour_func(d.Data_completeness);
    })
    .style("opacity", 0.75);

  daily_average_case_bars
    .datum(bars_daily_cases_chosen)
    .transition()
    .duration(1000)
    .attr(
      "d",
      d3
        .line()
        .defined((d) => !isNaN(d.Rolling_7_day_average_new_cases))
        .x(function (d) {
          return x_daily_cases(d.Period) + x_daily_cases.bandwidth() / 2;
        })
        .y(function (d) {
          return y_daily_cases(d.Rolling_7_day_average_new_cases);
        })
    );

  // ! Text for daily bars

  d3.select("#daily_bars_text_1").html(function (d) {
    return (
      "The figure below shows the daily confirmed cases of Covid-19 over time in " +
      chosen_summary_area +
      ". The black line represents the rolling average number of new cases confirmed in the previous seven days. The black line ends on the day we believe to be the latest complete date."
    );
  });

  d3.select("#daily_bars_text_2")
    .data(chosen_case_summary_data)
    .html(function (d) {
      return (
        "In the seven days to " +
        d.Rate_date +
        " there were <b>" +
        d3.format(",.0f")(d.Rolling_7_day_new_cases) +
        " new cases (" +
        d3.format(",.0f")(d.Rolling_7_day_new_cases_per_100000) +
        " per 100,000 population)</b>. This means on average " +
        d3.format(",.0f")(d.Rolling_7_day_average_new_cases) +
        " people are testing positive for COVID-19 each day in " +
        chosen_summary_area +
        "." +
        direction_func(d.Change_direction)
      );
    });

  d3.select("#daily_bars_text_3")
    .data(chosen_case_summary_data)
    .html(function (d) {
      return (
        "The latest data indicates there have been <b>" +
        d3.format(",.0f")(d.Cumulative_cases) +
        " cases so far </b>in " +
        chosen_summary_area +
        " as at " +
        data_refreshed_date +
        ". This is " +
        d3.format(",.0f")(d.Cumulative_per_100000) +
        " cases per 100,000 population."
      );
    });

  // ! Summary tile text

  d3.select("#latest_seven_days_all_ages_1")
    .data(chosen_case_summary_data)
    .html(function (d) {
      return (
        "<b class = 'highlight'>" +
        d3.format(",.0f")(d.Rolling_7_day_new_cases) +
        "</b> cases in the seven days to " +
        d.Rate_date +
        ". This is <b class = 'highlight'>" +
        d3.format(",.1f")(d.Rolling_7_day_new_cases_per_100000) +
        " </b>per 100,000 population."
      );
    });

  d3.select("#latest_seven_days_all_ages_2")
    .data(chosen_case_summary_data)
    .html(function (d) {
      return direction_func(d.Change_direction);
    });

  d3.select("#latest_seven_days_all_ages_3")
    .data(chosen_case_summary_data)
    .html(function (d) {
      return "This is for the 7 days to " + d.Rate_date;
    });

  d3.select("#latest_seven_days_60_plus_1")
    .data(chosen_case_summary_data_60)
    .html(function (d) {
      return (
        "<b class = 'highlight'>" +
        d3.format(",.0f")(d.Rolling_7_day_new_cases) +
        "</b> cases in the seven days to " +
        d.Rate_date +
        ". This is a rate of <b class = 'highlight'>" +
        d3.format(",.1f")(d.Rolling_7_day_new_cases_per_100000) +
        "</b> per 100,000 population."
      );
    });

  d3.select("#latest_seven_days_60_plus_2")
    .data(chosen_case_summary_data_60)
    .html(function (d) {
      return direction_func(d.Change_direction);
    });

  d3.select("#latest_seven_days_60_plus_3")
    .data(chosen_case_summary_data_60)
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
        d.Beds_date +
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
        " since the start of the pandemic."
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
