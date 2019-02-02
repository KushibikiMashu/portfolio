function makeChart(skills) {
  var names = [],
      levels = [],
      colors = [],
      backgroundColors = [];

  skills.map(function(skill) {
    names.push(skill.name);
    levels.push(skill.level);
    colors.push(skill.color);
  });

  var helperColor = Chart.helpers.color;
  colors.map(function(color) {
    backgroundColors.push(helperColor(color).alpha(0.65).rgbString());
  });

  setTimeout(function(){
    var ctx = document.getElementById('language-skills');
    var languageSkillChart = new Chart(ctx, {
      type: getType(),
      data: getData(names, levels, colors, backgroundColors),
      options: getOptions()
    })
  }, 100);
}

function getType() {
  return 'horizontalBar';
}

function getData(names, levels, colors, backgroundColors) {
  return {
    labels: names,
    datasets: [{
      label: "",
      data: levels,
      borderColor: colors,
      backgroundColor: backgroundColors,
      borderWidth: 1.5,
    }],
  };
}

function getOptions() {
  return {
    responsive: true,
    legend: {
      display: false
    },
    scales: {
      xAxes: [{
        ticks: {
          min: 0,
          max: 100
        }
      }],
      yAxes: [{
        ticks: {
          fontSize: 16
        }
      }],
    },
  };
}
