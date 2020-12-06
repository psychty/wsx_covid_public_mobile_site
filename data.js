// var width = window.innerWidth * 0.9 - 20;
var width = document.getElementById("daily_case_bars").offsetWidth;
var height = document.getElementById("daily_case_bars").offsetHeight;
if (width > 900) {
  var width = 900;
}
var width_margin = width * 0.15;

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
    if (link.style.animation) {
      link.style.animation = "";
    } else {
      link.style.animation = `navLinkFader 0.5s ease forwards ${i / 7 + 0.5}s`;
    }
  });
}

window.onclick = function (event) {
  var nav_selected = document.getElementById("nav-links");
  if (event.target == nav_selected) {
    navSlide();
  }
};

var direction_func = d3
  .scaleOrdinal()
  .domain(["Up", "Down", "Same"])
  .range([
    "However, cases are rising compared to the previous week.",
    "However, cases are falling compared to the previous week.",
    "The case numbers are the same as in the previous week.",
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

var previous_week_period = case_dates_df.filter(function (d) {
  return d.Item === "previous_week_period";
})[0]["Label"];

var request = new XMLHttpRequest();
request.open("GET", "./Outputs/mortality_dates.json", false);
request.send(null);
var mortality_dates_df = JSON.parse(request.responseText);

var deaths_published_period = mortality_dates_df.filter(function (d) {
  return d.Item === "published_on";
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

d3.select("#case_date_heading").html(function (d) {
  return "Number of cases in the 7 days to " + complete_date;
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

var request = new XMLHttpRequest();
request.open("GET", "./Outputs/table_summary.json", false);
request.send(null);
var at_a_glance = JSON.parse(request.responseText);

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

// at_a_glance.sort(function (a, b) {
//   return +a.index - +b.index;
// });

console.log(at_a_glance);

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

// ! At a glance table

window.onload = () => {
  loadTable(at_a_glance);
};

function loadTable(at_a_glance) {
  const tableBody = document.getElementById("at_glance_table");
  var dataHTML = "";

  for (let item of at_a_glance) {
    dataHTML += `<tr><td>${item.Name}</td><td>${d3.format(",.0f")(
      item.Rolling_7_day_new_cases
    )}</td><td>${d3.format(",.1f")(
      item.Rolling_7_day_new_cases_per_100000
    )}</td><td>${item.Change_direction}</td></tr>`;
  }

  tableBody.innerHTML = dataHTML;
}

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
xAxis_daily_cases
  .selectAll("text")
  .style("text-anchor", function (d, i) {
    return i % 2 ? "end" : "start";
  })
  .style("font-size", ".8rem");

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

yAxis_daily_cases.selectAll("text").style("font-size", ".8rem");

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

  chosen_death_data_ch = mortality_data.filter(function (d) {
    return d.Name == chosen_summary_area && d.Type == "Care homes";
  });

  // * Update text based on selected area
  d3.select("#what_am_i_showing").html(function (d) {
    return `Showing COVID-19 data for ${chosen_summary_area.replace(
      "West Sussex",
      " the whole county"
    )}`;
  });

  // ! Summary tile text

  d3.select("#area_seven_days_title").html(function (d) {
    return "Number of cases in " + chosen_summary_area;
  });

  d3.select("#latest_seven_days_all_ages_1")
    .data(chosen_case_summary_data)
    .html(function (d) {
      return (
        "<b class = 'highlight'>" +
        d3.format(",.0f")(d.Rolling_7_day_new_cases) +
        "</b> cases in the seven days to " +
        d.Rate_date +
        "."
      );
    });

  d3.select("#latest_seven_days_all_ages_2")
    .data(chosen_case_summary_data)
    .html(function (d) {
      return (
        "This is <b class = 'highlight'>" +
        d3.format(",.1f")(d.Rolling_7_day_new_cases_per_100000) +
        " </b>per 100,000 population."
      );
    });

  d3.select("#latest_seven_days_all_ages_3")
    .data(chosen_case_summary_data)
    .html(function (d) {
      return direction_func(d.Change_direction);
    });

  d3.select("#latest_seven_days_all_ages_4")
    .data(chosen_case_summary_data)
    .html(function (d) {
      return "This is for the 7 days to " + d.Rate_date;
    });

  d3.select("#area_over_60_title").html(function (d) {
    return "Cases among those aged 60+ in " + chosen_summary_area;
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
        d.Beds_date
      );
    });

  d3.select("#latest_covid_beds_2")
    .data(se_bed_all)
    .html(function (d) {
      return (
        "Of these, <b class = 'highlight'>" +
        d3.format(",.0f")(d.Patients_occupying_mv_beds) +
        "</b> were occupying beds capable of mechanical ventilation."
      );
    });

  d3.select("#latest_covid_beds_3")
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

  d3.select("#latest_covid_beds_4").html(function (d) {
    return "This was updated on " + bed_data_publish_date;
  });

  d3.select("#latest_covid_deaths_1")
    .data(chosen_death_data_all)
    .html(function (d) {
      return (
        "<b class = 'highlight'>" +
        d3.format(",.0f")(d.COVID_deaths_in_week) +
        "</b> COVID-19 deaths (where COVID-19 was mentioned on death certificate) in " +
        d.Label +
        ".<br>There have been <b class = 'highlight'>" +
        d3.format(",.0f")(d.COVID_deaths_cumulative) +
        " </b> COVID-19 deaths in " +
        chosen_summary_area +
        " since the start of the pandemic."
      );
    });

  d3.select("#latest_covid_deaths_2")
    .data(chosen_death_data_ch)
    .html(function (d) {
      return (
        "There were <b class = 'highlight'>" +
        d3.format(",.0f")(d.COVID_deaths_in_week) +
        "</b> COVID-19 deaths in care homes this week with <b class = 'highlight'>" +
        d3.format(",.0f")(d.COVID_deaths_cumulative) +
        " </b> care home deaths since the start of the pandemic."
      );
    });

  d3.select("#latest_covid_deaths_4").html(function (d) {
    return "This was updated on " + deaths_published_period;
  });

  // ! Daily confirmed COVID-19 cases in chosen area

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

  yAxis_daily_cases.selectAll("text").style("font-size", ".8rem");

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

  d3.select("#daily_confirmed_title").html(function (d) {
    return "Daily confirmed COVID-19 cases in " + chosen_summary_area;
  });

  d3.select("#daily_bars_text_1")
    .data(chosen_case_summary_data)
    .html(function (d) {
      return (
        "In the seven days to " +
        d.Rate_date +
        " there were <b class = 'case_latest'>" +
        d3.format(",.0f")(d.Rolling_7_day_new_cases) +
        " </b> new cases (<b>" +
        d3.format(",.0f")(d.Rolling_7_day_new_cases_per_100000) +
        " per 100,000 population</b>). This means on average " +
        d3.format(",.0f")(d.Rolling_7_day_average_new_cases) +
        " people are testing positive for COVID-19 each day in " +
        chosen_summary_area +
        ". <b>" +
        direction_func(d.Change_direction) +
        "</b>"
      );
    });

  d3.select("#daily_bars_text_2")
    .data(chosen_case_summary_data)
    .html(function (d) {
      return (
        "The latest data indicates there have been <b class = 'case_latest'>" +
        d3.format(",.0f")(d.Cumulative_cases) +
        "</b> cases so far in " +
        chosen_summary_area +
        " as at " +
        data_refreshed_date +
        ". This is <b>" +
        d3.format(",.0f")(d.Cumulative_per_100000) +
        " cases per 100,000 population</b>."
      );
    });

  d3.select("#daily_bars_text_3").html(function (d) {
    return (
      "This figure shows the daily confirmed cases of Covid-19 over time in " +
      chosen_summary_area +
      ". Remember, you can change the area by using the menu at the top of this page."
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

// ! Postcode lookup and msoa map

// var case_key_msoa = [
//   "0-2 cases",
//   "3-5 cases",
//   "6-10 cases",
//   "11-15 cases",
//   "More than 15 cases",
// ];
// var case_key_msoa_colours = [
//   "#f1eef6",
//   "#d7b5d8",
//   "#df65b0",
//   "#dd1c77",
//   "#980043",
// ];

// var msoa_case_colour_func = d3
//   .scaleOrdinal()
//   .domain(case_key_msoa)
//   .range(case_key_msoa_colours);

var request = new XMLHttpRequest();
request.open("GET", "./Outputs/msoa_summary.json", false);
request.send(null);
var msoa_summary_data = JSON.parse(request.responseText); // parse the fetched json data into a variable

var request = new XMLHttpRequest();
request.open("GET", "./Outputs/utla_summary.json", false);
request.send(null);
var utla_summary_data = JSON.parse(request.responseText); // parse the fetched json data into a variable

// // Add AJAX request for data
// var msoa_map_data = $.ajax({
//   url: "./Outputs/msoa_covid_rate_latest.geojson",
//   dataType: "json",
//   success: console.log("MSOA boundary for cases successfully loaded."),
//   error: function (xhr) {
//     alert(xhr.statusText);
//   },
// });

// var tileUrl = "https://{s}.basemaps.cartocdn.com/light_all/{z}/{x}/{y}{r}.png";
// var attribution =
//   '&copy; <a href="https://www.openstreetmap.org/copyright">OpenStreetMap</a> contributors &copy; <a href="https://carto.com/attributions">CARTO</a><br> Contains Ordnance Survey data © Crown copyright and database right 2020.<br>Contains Parliamentary information licensed under the Open Parliament Licence v3.0.<br>Zoom in/out using your mouse wheel or the plus (+) and minus (-) buttons. Click on an area to find out more.';

// function msoa_case_colour(d) {
//   return d === case_key_msoa[0]
//     ? case_key_msoa_colours[0]
//     : d === case_key_msoa[1]
//     ? case_key_msoa_colours[1]
//     : d === case_key_msoa[2]
//     ? case_key_msoa_colours[2]
//     : d === case_key_msoa[3]
//     ? case_key_msoa_colours[3]
//     : d === case_key_msoa[4]
//     ? case_key_msoa_colours[4]
//     : "#feebe2";
// }

// function style_msoa_cases(feature) {
//   return {
//     fillColor: msoa_case_colour(feature.properties.Case_key),
//     weight: 1,
//     opacity: 0.6,
//     color: "white",
//     dashArray: "3",
//     fillOpacity: 0.7,
//   };
// }

// $.when(msoa_map_data).done(function () {
//   var msoa_map = L.map("msoa_map_place");

//   var msoa_basemap = L.tileLayer(tileUrl, {
//     attribution,
//     minZoom: 8,
//   }).addTo(msoa_map);

//   var msoa_map_hcl = L.geoJSON(msoa_map_data.responseJSON, {
//     style: style_msoa_cases,
//   })
//     .addTo(msoa_map)
//     .bindPopup(function (layer) {
//       return (
//         "<p style = 'font-size: .8rem'>" +
//         layer.feature.properties.Label +
//         "</p>"
//       );
//     });

//   msoa_map.fitBounds(msoa_map_hcl.getBounds());

//   var marker_chosen = L.marker([0, 0]).addTo(msoa_map_hcl);

//search event
$(document).on("click", "#btnPostcode", function () {
  var input = $("#txtPostcode").val();
  var url = "https://api.postcodes.io/postcodes/" + input;

  post(url).done(function (postcode) {
    var chosen_msoa = postcode["result"]["msoa"];
    var chosen_lat = postcode["result"]["latitude"];
    var chosen_long = postcode["result"]["longitude"];

    document.getElementById("local_tiles").classList.add("local_tiles"); // give the div the class of 'local_tiles' when the search button is clicked (otherwise the psuedo empty div takes up too much space).

    // marker_chosen.setLatLng([chosen_lat, chosen_long]);
    // msoa_map.setView([chosen_lat, chosen_long], 11);

    var msoa_summary_data_chosen = msoa_summary_data.filter(function (d) {
      return d.MSOA11NM == chosen_msoa;
    });

    chosen_utla = msoa_summary_data_chosen[0]["UTLA19NM"];

    var utla_summary_data_chosen = utla_summary_data.filter(function (d) {
      return d.Name == chosen_utla;
    });

    d3.select("#local_case_summary_1")
      .data(msoa_summary_data_chosen)
      .html(function (d) {
        return d.MSOA11NM + " (" + d.msoa11hclnm + ")";
      });

    if (msoa_summary_data_chosen[0]["Latest_rate"] == "No rate available") {
      d3.select("#local_case_summary_2")
        .data(msoa_summary_data_chosen)
        .html(function (d) {
          return (
            "<b class = 'case_latest'>" +
            d.This_week +
            "</b> cases in the seven days to " +
            complete_date
          );
        });
    }

    if (msoa_summary_data_chosen[0]["Latest_rate"] != "No rate available") {
      d3.select("#local_case_summary_2")
        .data(msoa_summary_data_chosen)
        .html(function (d) {
          return (
            "<b class = 'case_latest'>" +
            d.This_week +
            "</b> cases in the seven days to " +
            complete_date +
            ". This is <b>" +
            d.Latest_rate +
            " cases per 100,000 population</b>."
          );
        });
    }

    d3.select("#local_case_summary_3")
      .data(msoa_summary_data_chosen)
      .html(function (d) {
        return d.Change_label;
      });

    d3.select("#local_case_summary_4")
      .data(utla_summary_data_chosen)
      .html(function (d) {
        return (
          "This local area is within <b>" +
          chosen_utla +
          "</b> which is currently in <b class = 'tier'>" +
          d.Tier +
          "</b>."
        );
      });

    d3.select("#local_case_summary_5")
      .data(utla_summary_data_chosen)
      .html(function (d) {
        return (
          "In <b>" +
          chosen_utla +
          " overall</b>, in the seven days to " +
          complete_date +
          " there have been " +
          d.Rolling_7_day_new_cases +
          " cases (" +
          d3.format(",.1f")(d.Rolling_7_day_new_cases_per_100000) +
          " cases per 100,000).<b> " +
          direction_func(d.Change_direction) +
          "</b> In the seven days to " +
          previous_week_period +
          ", there were " +
          d3.format(",.0f")(d.Previous_7_days_sum) +
          " cases and so cases have " +
          d.Change_label
        );
      });
  });
});

//enter event - search
$("#txtPostcode").keypress(function (e) {
  if (e.which === 13) {
    $("#btnPostcode").click();
  }
});

//ajax call
function post(url) {
  return $.ajax({
    url: url,
    success: function () {
      //woop
    },
    error: function (desc, err) {
      $("#result_text").html("Details: " + desc.responseText);

      d3.select("#local_case_summary_1").html(function (d) {
        return "The postcode you entered does not seem to be valid, please check and try again.";
      });
      d3.select("#local_case_summary_2").html(function (d) {
        return "This could be because there is a problem with the postcode look up tool we are using.";
      });
      d3.select("#local_case_summary_3").html(function (d) {
        return "";
      });
    },
  });
}
// });

// function key_msoa_cases() {
//   case_key_msoa.forEach(function (item, index) {
//     var list = document.createElement("li");
//     list.innerHTML = item + " in 7 days to " + complete_date;
//     list.className = "key_list_msoa_cases";
//     list.style.borderColor = msoa_case_colour_func(index);
//     var tt = document.createElement("div");
//     tt.className = "side_tt";
//     tt.style.borderColor = msoa_case_colour_func(index);
//     var tt_h3_1 = document.createElement("h3");
//     tt_h3_1.innerHTML = item.Case_key;

//     tt.appendChild(tt_h3_1);
//     var div = document.getElementById("msoa_case_key");
//     div.appendChild(list);
//   });
// }

// key_msoa_cases();

// d3.select("#local_case_map_title").html(function (d) {
//   return (
//     "Number of confirmed COVID-19 cases in the seven days to " +
//     complete_date +
//     "; South East England MSOAs"
//   );
// });

// ! utla restrictions

var restriction_type = [
  "Tier 1 (medium)",
  "Tier 2 (high)",
  "Tier 3 (very high)",
];
// var restriction_colours = ["#ffb400", "#9762a2", "#374776"];
// var restriction_colours = ["#B9DDF1", "#6A9BC3", "#2A5783"];
var restriction_colours = ["#ffb400", "#6A9BC3", "#2A5783"];

var restriction_colour_func = d3
  .scaleOrdinal()
  .domain(restriction_type)
  .range(restriction_colours);

// Add AJAX request for data
var utla_restrictions = $.ajax({
  url: "./Outputs/utla_covid_latest.geojson",
  dataType: "json",
  success: console.log("UTLA boundary for restrictions successfully loaded."),
  error: function (xhr) {
    alert(xhr.statusText);
  },
});

var tileUrl = "https://{s}.basemaps.cartocdn.com/light_all/{z}/{x}/{y}{r}.png";
var attribution =
  '&copy; <a href="https://www.openstreetmap.org/copyright">OpenStreetMap</a> contributors &copy; <a href="https://carto.com/attributions">CARTO</a><br>Click on an area to find out more.';

function restriction_utla_colour(d) {
  return d === restriction_type[0]
    ? restriction_colours[0]
    : d === restriction_type[1]
    ? restriction_colours[1]
    : d === restriction_type[2]
    ? restriction_colours[2]
    : "#feebe2";
}

function style_restriction(feature) {
  return {
    fillColor: restriction_utla_colour(feature.properties.Tier),
    weight: 1,
    opacity: 0.6,
    color: "white",
    dashArray: "3",
    fillOpacity: 1,
  };
}

$.when(utla_restrictions).done(function () {
  var utla_map_restrictions = L.map("utla_restrictions_map");

  var basemap_restriction = L.tileLayer(tileUrl, {
    attribution,
    minZoom: 5,
  }).addTo(utla_map_restrictions);

  var utla_restrictions_hcl = L.geoJSON(utla_restrictions.responseJSON, {
    style: style_restriction,
  })
    .addTo(utla_map_restrictions)
    .bindPopup(function (layer) {
      return (
        "<b>" +
        layer.feature.properties.ctyua19nm +
        "</b><br>" +
        layer.feature.properties.Tier
      );
    });

  utla_map_restrictions.fitBounds(utla_restrictions_hcl.getBounds());
});

if (width < 1300) {
  var scaled_icon_size = 30;
}

if (width > 1300) {
  var scaled_icon_size = 50;
}

// var svg = d3.select('.container')
//     .appendHTML('<svg xmlns="http://www.w3.org/2000/svg"><g><circle class="circle1" cx="50" cy="50" r="50"></circle></g></svg>')
//     .select('g');

// // Icons
// svg_ut_walkthrough.selectAll('icons_yo')
//   .data(selected_ut_area_df)
//   .enter()
//   .append("svg:image")
//   .attr("x", function(d) { return x_pos(d.x) - scaled_icon_size/2; })
//   .attr('y', function(d) { return y_pos(d.y) - scaled_icon_size/2; })
//   .attr('width', scaled_icon_size)
//   .attr('height', scaled_icon_size)
//   .on("mousemove", showTooltip_ut)
//   .on('mouseout', mouseleave_ut)
//   .on('click', choose_an_indicator)
//   .attr('class', 'icons_yo')
//   .attr("xlink:href", function(d) {return d.img_path; })
//   .attr('id', 'ut_indicator_icon_images');
