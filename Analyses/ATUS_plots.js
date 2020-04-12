
// set the dimensions and margins of the graph
var margin = {top: 10, right: 50, bottom: 60, left: 75},
    width = 600 - margin.left - margin.right,
    height = 350 - margin.top - margin.bottom;
    
// Function to parse the date / time
var	parseDate = d3.timeParse("%Y-%m-%d");

// set opacity to de-emphasize lines and circles when using tooltip
var de_emphasize_opacity = 0.4;

// style the line to be drawn to connect 2020-2019 points
function styleConn(selection){
    selection
        .style('stroke', '#737373')
        .attr('opacity', 0)
        .style("stroke-width", 2.5)
        .style("stroke-dasharray", ("6, 4"));
}

// Set the axis ranges
var x = d3.scaleLinear().range([ 0, width ]);
var y = d3.scaleLinear().range([ height, 0 ]);

// set the number of and y ticks and gridlines
var my_nXticks = 10;
var my_nYticks = 5;

// set the padding around the ticks
var my_tickPadsize = 5;

// set the size of the tick; also controls the grid 'overflow'
var my_tickSize = 15;

// gridlines in x axis function
function make_x_gridlines() {
      return d3.axisBottom(x)
          .ticks(my_nXticks);
}

// gridlines in y axis function
function make_y_gridlines() {		
    return d3.axisLeft(y)
        .ticks(my_nYticks);
}

// Define the x axis label
function drawXlabel(selection) {
  selection
    .attr("transform",
            "translate(" + (width/2) + " ," + 
                           (height + margin.top + 40) + ")")
    .attr("text-anchor", "middle")
    .attr("fill", "#333333")
    .attr("font-size", 12)
    .text("Age");
}

// Define the y axis label
function drawYlabel(selection) {
  selection
    .attr("transform", "rotate(-90)")
    .attr("y", 0 - margin.left)
    .attr("x",0 - (height / 2))
    .attr("dy", "1em")
    .attr("text-anchor", "middle")
    .attr("fill", "#333333")
    .attr("font-size", 12)
    .text("Hours:minutes");
}

// Style the plot line
function styleLine(selection) {
  selection
    .attr("fill", "none")
    .attr("stroke-width", 3)
    .style("stroke-dasharray", ("10, 10"))
    .attr("opacity", 0.9);
}

function styleMaleCircle(selection){
    selection
        .attr("r", 4)
        .attr('opacity', 0.9)
        .attr("fill", "#66d8db");
}

function styleFemaleCircle(selection){
    selection
      .attr("r", 4)
      .attr('opacity', 0.9)
      .attr("fill", "#fbada7");
}

function styleEmphCircle(selection){
    selection
      .attr('r', 6)
      .attr('fill', '#737373')
      .attr('opacity', 1);
}

// function to print the "3 hours, 15 minutes"
function printTime(n){
  hour = Math.floor(n / 60);
  minute = Math.round(n - (hour * 60));
  if (hour === 0) {
    return minute + " minutes";
    } else if (hour === 1){
      return hour + ' hour, ' + minute + " minutes";
    } else {
          return hour + ' hours, ' + minute + " minutes";
    }
}

// function to add leading zero
function pad(num, size) {
    var s = num+"";
    while (s.length < size) s = "0" + s;
    return s;
}

// function to print "hours:minutes"
function printHourMinute(n){
  hour = Math.floor(n / 60);
  minute = Math.round(n - (hour * 60));
  return hour + ":" + pad(minute, 2);
}


// Define the tooltip
var Tooltip = d3.select("body")
  .append("div")
  .style("opacity", 0)
  .attr("class", "tooltip")
  .style("background-color", "white")
  .style("color", "#333333")
  .style("border", "solid")
  .style("border-width", "2px")
  .style("border-radius", "0px")
  .style("padding", "5px");

// append the svg object
var svg = d3.select("div#TV")
  .append("div")
  // Container class to make it responsive.
  .classed("svg-container", true) 
  .append("svg")
  // Responsive SVG needs these 2 attributes and no width and height attr.
  .attr("preserveAspectRatio", "xMinYMin meet")
  .attr("viewBox", "0 0 600 350")
  // Class to make it responsive.
  .classed("svg-content-responsive", true)
  .append("g")
    .attr("transform",
          "translate(" + margin.left + "," + margin.top + ")");

//Read the data
d3.csv("/d3/ATUS/d3_tv.csv",

  // When reading the csv, I must format variables:
  function(d){
    return { TEAGE : d.TEAGE, Male : d.Male, Female : d.Female, Male_smooth : d.Male_smooth, Female_smooth : d.Female_smooth };
  },

  // Now I can use this dataset:
  function(data) {
  
  // TV plot
    // Scale the range of the data
    x.domain(d3.extent(data, function(d) { return d.TEAGE; }));
    y.domain( [100, 350] );
    
    // add the X gridlines
    svg.append("g")			
      .attr("class", "grid")
      .attr("transform", "translate(0," + height + ")")
      .call(make_x_gridlines()
          .tickSize(-height-my_tickSize)
          .tickFormat("")
      );
      
    // add the Y gridlines
    svg.append("g")			
      .attr("class", "grid")
      .call(make_y_gridlines()
          .tickSize(-width-my_tickSize)
          .tickFormat("")
      );
      
    // add the X axis
    svg.append("g")
      .attr("class", "grid")
      .attr("transform", "translate(0," + height + ")")
      .call(d3.axisBottom(x)
        .tickPadding(my_tickPadsize)
        .tickSize(my_tickSize)
        .ticks(my_nXticks));

    // Add Y axis
    svg.append("g")
      .attr("class", "grid")
      .call(d3.axisLeft(y)
        .tickPadding(my_tickPadsize)
        .tickSize(my_tickSize)
        .ticks(my_nYticks)
        .tickFormat(printHourMinute));
    
    // text label for the x and y axis
    svg.append("text").call(drawXlabel);
    svg.append("text").call(drawYlabel);
    
    // add label for males
    svg.append("text")
        .attr("class", "plot_label")
        .attr("x", (width * 0.60))             
        .attr("y", (height * 0.3))
        .attr("text-anchor", "right")  
        .style("font-size", "16px") 
        .style("font-weight", "700") 
        .style("fill", "#66d8db")
        .text("Males");
    
    // add label for females
    svg.append("text")
        .attr("class", "plot_label")
        .attr("x", (width * 0.7))             
        .attr("y", (height * 0.8))
        .attr("text-anchor", "right")  
        .style("font-size", "16px") 
        .style("font-weight", "700") 
        .style("fill", "#fbada7")
        .text("Females");
    
    // add line between male and female points
    svg
      .append('g')
      .selectAll("line")
      .data(data)
      .enter()
      .append("line")
        .call(styleConn)
        .attr("class", function(d, i) {return "line_conn line_conn" + i;})
        .attr("x1", function(d, i) { return x(d.TEAGE) } )
        .attr("y1", function(d, i) { return y(d.Male) } )
        .attr("x2", function(d, i) { return x(d.TEAGE) } )
        .attr("y2", function(d, i) { return y(d.Female) } );

    // Add the male points
    svg
      .append("g")
      .selectAll("dot")
      .data(data)
      .enter()
      .append("circle")
        .attr("cx", function(d) { return x(d.TEAGE) } )
        .attr("cy", function(d) { return y(d.Male) } )
        .attr("class", function(d, i) {return "pt_TV pt_TV_male pt_TV_male" + i;})
        .call(styleMaleCircle)
        // Three function that change the tooltip when user hover / move / leave a cell
        .on('mouseover', function(d, i) {
            console.log("mouseover on", this);
              
            // de-emphasize lines, circles, text labels, 
            d3.selectAll("path.path_TV")
              .attr('opacity', de_emphasize_opacity);
            d3.selectAll("circle.pt_TV")
              .attr('opacity', de_emphasize_opacity);
            d3.selectAll('text.plot_label')
              .attr('opacity', de_emphasize_opacity);
              
            // make the mouseover'd element big
            d3.selectAll("circle.pt_TV_male" + i)
              .transition()
              .duration(75)
              .call(styleEmphCircle)
              .attr('fill', '#333333');
            
            // highlight the male point
            d3.selectAll("circle.pt_TV_female" + i)
              .transition()
              .duration(75)
              .call(styleEmphCircle);
              
            // highlight the selected points connection
            d3.selectAll("line.line_conn" + i)
              .attr('opacity', 1);
            
            // this makes the tooltip show
            Tooltip.style("opacity", 1);
          })
          
        .on('mousemove', function(d, i) {
          // this makes the tooltip move
          Tooltip
            .html(
              "<b> Age " + d.TEAGE + "</b><br>" + 
              "Males: " + printTime(d.Male) + "<br>" +
              "Females: " + printTime(d.Female) + "<br>" +
              "Gap of " + printTime(d.Male - d.Female)
              )
            .style("left", d3.event.pageX + 10 + "px")
            .style("top", d3.event.pageY + "px");
        })
        
        .on('mouseout', function(d, i) {
            console.log("mouseout", this);
            
            // return the mouseover'd element to the original format
            d3.selectAll("circle.pt_TV_male")
              .transition()
              .duration(200)
              .call(styleMaleCircle);
            
            d3.selectAll("circle.pt_TV_female")
              .transition()
              .duration(200)
              .call(styleFemaleCircle); 
            
            // re-emphasis lines and circles
            d3.selectAll("path.path_TV")
              .transition()
              .duration(200)
              .call(styleLine);
            d3.selectAll('text.plot_label')
              .attr('opacity', 1);

            // remove the selected points connection
            d3.selectAll("line.line_conn")
              .call(styleConn);

            // this makes the tooltip disappear
            Tooltip.style("opacity", 0);
          });


    // Add the female points
    svg
      .append("g")
      .selectAll("dot")
      .data(data)
      .enter()
      .append("circle")
        .attr("cx", function(d) { return x(d.TEAGE) } )
        .attr("cy", function(d) { return y(d.Female) } )
        .attr("class", function(d, i) {return "pt_TV pt_TV_female pt_TV_female" + i;})
        .call(styleFemaleCircle)
        // Three function that change the tooltip when user hover / move / leave a cell
        .on('mouseover', function(d, i) {
            console.log("mouseover on", this);
              
            // de-emphasize lines, circles, text labels, 
            d3.selectAll("path.path_TV")
              .attr('opacity', de_emphasize_opacity);
            d3.selectAll("circle.pt_TV")
              .attr('opacity', de_emphasize_opacity);
            d3.selectAll('text.plot_label')
              .attr('opacity', de_emphasize_opacity);
              
            // make the mouseover'd element big
            d3.selectAll("circle.pt_TV_female" + i)
              .transition()
              .duration(75)
              .call(styleEmphCircle)
              .attr('fill', '#333333');
            
            // highlight the male point
            d3.selectAll("circle.pt_TV_male" + i)
              .transition()
              .duration(75)
              .call(styleEmphCircle);
              
            // highlight the selected points connection
            d3.selectAll("line.line_conn" + i)
              .attr('opacity', 1);
            
            // this makes the tooltip show
            Tooltip.style("opacity", 1);
          })
          
        .on('mousemove', function(d, i) {
          // this makes the tooltip move
          Tooltip
            .html(
              "<b> Age " + d.TEAGE + "</b><br>" + 
              "Males: " + printTime(d.Male) + "<br>" +
              "Females: " + printTime(d.Female) + "<br>" +
              "Gap of " + printTime(d.Male - d.Female)
              )
            .style("left", d3.event.pageX + 10 + "px")
            .style("top", d3.event.pageY + "px");
        })
        
        .on('mouseout', function(d, i) {
            console.log("mouseout", this);
            
            // return the mouseover'd element to the original format
            d3.selectAll("circle.pt_TV_male")
              .transition()
              .duration(200)
              .call(styleMaleCircle);
            
            d3.selectAll("circle.pt_TV_female")
              .transition()
              .duration(200)
              .call(styleFemaleCircle); 
            
            // re-emphasis lines and circles
            d3.selectAll("path.path_TV")
              .transition()
              .duration(200)
              .call(styleLine);
            d3.selectAll('text.plot_label')
              .attr('opacity', 1);

            // remove the selected points connection
            d3.selectAll("line.line_conn" + i)
              .call(styleConn);

              // this makes the tooltip disappear
              Tooltip.style("opacity", 0);
          });

	    // Add male line
    svg.append("path")
      .datum(data)
      .call(styleLine)
      .attr("stroke", "#48bfc2")
      .attr("class", "path_TV path_TV_male")
      .attr("d", d3.line()
        .x(function(d) { return x(d.TEAGE) })
        .y(function(d) { return y(d.Male_smooth) })
        .curve(d3.curveMonotoneX)
        
        // define (ie draw) the line at values not equal to NA
        .defined(function(d) { return d.Male_smooth != "NA" })
        );
    
    // Add the female line
    svg.append("path")
      .datum(data)
      .call(styleLine)
      .attr("stroke", "#d6827c")
      .attr("class", "path_TV path_TV_female")
      .attr("d", d3.line()
        .x(function(d) { return x(d.TEAGE) })
        .y(function(d) { return y(d.Female_smooth) })
        .curve(d3.curveMonotoneX)
        
        // define (ie draw) the line at values not equal to NA
        .defined(function(d) { return d.Female_smooth != "NA" })
        );
}
);

