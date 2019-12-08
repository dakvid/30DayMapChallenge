var Shuffle = window.Shuffle;

var MapExplorer = function (element) {
  this.challenges = Array.from(document.querySelectorAll('.mapfilter-challenge button'));
  this.areas = Array.from(document.querySelectorAll('.mapfilter-area button'));
  this.cities = Array.from(document.querySelectorAll('.mapfilter-city button'));
  this.topics = Array.from(document.querySelectorAll('.mapfilter-topic button'));
  this.types = Array.from(document.querySelectorAll('.mapfilter-type button'));
  this.tools = Array.from(document.querySelectorAll('.mapfilter-tool button'));
  this.searchInput = document.querySelector('#searchHandle');
  this.reset = document.querySelector('#mapfilter-reset-button');
  
  this.shuffle = new Shuffle(element, {
    easing: 'cubic-bezier(0.165, 0.840, 0.440, 1.000)',
    sizer: '.my-sizer-element',
    itemSelector: '.map-card',
  });
  
  this.filters = {
    challenges: [],
    areas: [],
    cities: [],
    topics: [],
    types: [],
    tools: [],
    handleTxt: "",
  };
  
  this._bindEventListeners();
  this.addSorting();
  //this.addSearchFilter();
};

MapExplorer.prototype._bindEventListeners = function () {
  this._onChallengeChange = this._handleChallengeChange.bind(this);
  this._onAreaChange = this._handleAreaChange.bind(this);
  this._onCityChange = this._handleCityChange.bind(this);
  this._onTopicChange = this._handleTopicChange.bind(this);
  this._onTypeChange = this._handleTypeChange.bind(this);
  this._onToolChange = this._handleToolChange.bind(this);
  this._onHandleChange = this._handleHandleChange.bind(this);
  this._onResetChange = this._handleResetChange.bind(this);
  
  this.challenges.forEach(function (button) {
    button.addEventListener('click', this._onChallengeChange);
  }, this);
  
  this.areas.forEach(function (button) {
    button.addEventListener('click', this._onAreaChange);
  }, this);
  
  this.cities.forEach(function (button) {
    button.addEventListener('click', this._onCityChange);
  }, this);
  
  this.topics.forEach(function (button) {
    button.addEventListener('click', this._onTopicChange);
  }, this);
  
  this.types.forEach(function (button) {
    button.addEventListener('click', this._onTypeChange);
  }, this);
  
  this.tools.forEach(function (button) {
    button.addEventListener('click', this._onToolChange);
  }, this);
  
  this.searchInput.addEventListener('keyup', this._onHandleChange);
  
  this.reset.addEventListener('click', this._onResetChange);
};

MapExplorer.prototype._getCurrentChallengeFilters = function () {
  return this.challenges.filter(function (button) {
    return button.classList.contains('active');
  }).map(function (button) {
    return button.getAttribute('data-value');
  });
};

MapExplorer.prototype._getCurrentAreaFilters = function () {
  return this.areas.filter(function (button) {
    return button.classList.contains('active');
  }).map(function (button) {
    return button.getAttribute('data-value');
  });
};

MapExplorer.prototype._getCurrentCityFilters = function () {
  return this.cities.filter(function (button) {
    return button.classList.contains('active');
  }).map(function (button) {
    return button.getAttribute('data-value');
  });
};

MapExplorer.prototype._getCurrentTopicFilters = function () {
  return this.topics.filter(function (button) {
    return button.classList.contains('active');
  }).map(function (button) {
    return button.getAttribute('data-value');
  });
};

MapExplorer.prototype._getCurrentTypeFilters = function () {
  return this.types.filter(function (button) {
    return button.classList.contains('active');
  }).map(function (button) {
    return button.getAttribute('data-value');
  });
};

MapExplorer.prototype._getCurrentToolFilters = function () {
  return this.tools.filter(function (button) {
    return button.classList.contains('active');
  }).map(function (button) {
    return button.getAttribute('data-value');
  });
};

MapExplorer.prototype._getCurrentHandleFilters = function () {
  return this.searchInput.value.toLowerCase();
};


MapExplorer.prototype._handleChallengeChange = function (evt) {
  var button = evt.currentTarget;
  
  // Treat these buttons like radio buttons where only 1 can be selected.
  if (button.classList.contains('active')) {
    button.classList.remove('active');
  } else {
    this.challenges.forEach(function (btn) {
      btn.classList.remove('active');
    });
    button.classList.add('active');
  }
  this.filters.challenges = this._getCurrentChallengeFilters();
  this.filter();
};

MapExplorer.prototype._handleAreaChange = function (evt) {
  var button = evt.currentTarget;
  
  // Treat these buttons like radio buttons where only 1 can be selected.
  if (button.classList.contains('active')) {
    button.classList.remove('active');
  } else {
    this.areas.forEach(function (btn) {
      btn.classList.remove('active');
    });
    button.classList.add('active');
  }
  this.filters.areas = this._getCurrentAreaFilters();
  this.filter();
};

MapExplorer.prototype._handleCityChange = function (evt) {
  var button = evt.currentTarget;
  
  // Treat these buttons like radio buttons where only 1 can be selected.
  if (button.classList.contains('active')) {
    button.classList.remove('active');
  } else {
    this.cities.forEach(function (btn) {
      btn.classList.remove('active');
    });
    button.classList.add('active');
  }
  this.filters.cities = this._getCurrentCityFilters();
  this.filter();
};

MapExplorer.prototype._handleTopicChange = function (evt) {
  var button = evt.currentTarget;
  
  // Treat these buttons like radio buttons where only 1 can be selected.
  if (button.classList.contains('active')) {
    button.classList.remove('active');
  } else {
    this.topics.forEach(function (btn) {
      btn.classList.remove('active');
    });
    button.classList.add('active');
  }
  this.filters.topics = this._getCurrentTopicFilters();
  this.filter();
};

MapExplorer.prototype._handleTypeChange = function (evt) {
  var button = evt.currentTarget;
  
  // Treat these buttons like radio buttons where only 1 can be selected.
  if (button.classList.contains('active')) {
    button.classList.remove('active');
  } else {
    this.types.forEach(function (btn) {
      btn.classList.remove('active');
    });
    button.classList.add('active');
  }
  this.filters.types = this._getCurrentTypeFilters();
  this.filter();
};

MapExplorer.prototype._handleToolChange = function (evt) {
  var button = evt.currentTarget;
  
  // Treat these buttons like radio buttons where only 1 can be selected.
  if (button.classList.contains('active')) {
    button.classList.remove('active');
  } else {
    this.tools.forEach(function (btn) {
      btn.classList.remove('active');
    });
    button.classList.add('active');
  }
  this.filters.tools = this._getCurrentToolFilters();
  this.filter();
};

MapExplorer.prototype._handleHandleChange = function (evt) {
  this.filters.handleTxt = this._getCurrentHandleFilters();
  this.filter();
};

MapExplorer.prototype._handleResetChange = function (evt) {
  this.challenges.forEach(function (btn) {
    btn.classList.remove('active');
  });
  this.filters.challenges = [];
  
  this.areas.forEach(function (btn) {
    btn.classList.remove('active');
  });
  this.filters.areas = [];
  
  this.cities.forEach(function (btn) {
    btn.classList.remove('active');
  });
  this.filters.cities = [];
  
  this.topics.forEach(function (btn) {
    btn.classList.remove('active');
  });
  this.filters.topics = [];
  
  this.types.forEach(function (btn) {
    btn.classList.remove('active');
  });
  this.filters.types = [];
  
  this.tools.forEach(function (btn) {
    btn.classList.remove('active');
  });
  this.filters.tools = [];
  
  this.searchInput.value = "";
  this.filters.handleTxt = "";
  
  this.filter();
};





MapExplorer.prototype.filter = function () {
  if (this.hasActiveFilters()) {
    this.shuffle.filter(this.itemPassesFilters.bind(this));
  } else {
    this.shuffle.filter(Shuffle.ALL_ITEMS);
  }
};

MapExplorer.prototype.hasActiveFilters = function () {
  return Object.keys(this.filters).some(function (key) {
    return this.filters[key].length > 0;
  }, this);
};

MapExplorer.prototype.itemPassesFilters = function (element) {
  var challenges = this.filters.challenges; // 0 or 1
  var areas = this.filters.areas;           // 0 or 1
  var cities = this.filters.cities;         // 0 or 1
  var topics = this.filters.topics;         // 0 or 1
  var types = this.filters.types;           // 0 or 1
  var tools = this.filters.tools;           // 0 or 1
  var handleTxt = this.filters.handleTxt;   // "" or nonempty
  var f_challenge = element.getAttribute('data-challenge');
  var area = element.getAttribute('data-area');
  var f_areas = area.split(',');
  var city = element.getAttribute('data-city');
  var f_cities = city.split(',');
  var topic = element.getAttribute('data-topics');
  var f_topics = topic.split(',');
  var type = element.getAttribute('data-types');
  var f_types = type.split(',');
  var tool = element.getAttribute('data-tools');
  var f_tools = tool.split(',');
  var handle = element.getAttribute('data-handle');
  
  // If there is an active XXX filter and this map is not in that array
  // Assume always an Array (cf shuffle.js function)
  
  if (challenges.length > 0 && !challenges.includes(f_challenge)) {
    return false;
  }

  function testArea(an_area) {
    return f_areas.includes(an_area);
  }
  if (areas.length > 0 && !areas.some(testArea)) {
    return false;
  }

  function testCity(a_city) {
    return f_cities.includes(a_city);
  }
  if (cities.length > 0 && !cities.some(testCity)) {
    return false;
  }

  function testTopic(a_topic) {
    return f_topics.includes(a_topic);
  }
  if (topics.length > 0 && !topics.some(testTopic)) {
    return false;
  }

  function testType(a_type) {
    return f_types.includes(a_type);
  }
  if (types.length > 0 && !types.some(testType)) {
    return false;
  }

  function testTool(a_tool) {
    return f_tools.includes(a_tool);
  }
  if (tools.length > 0 && !tools.some(testTool)) {
    return false;
  }
  
  if (handleTxt.length > 0) {
    return handle.indexOf(handleTxt) !== -1;
  }

  return true;
};






MapExplorer.prototype.addSorting = function () {
  document.querySelector('.sort-options').addEventListener('click', this._handleSortChange.bind(this));
};

MapExplorer.prototype._handleSortChange = function (evt) {
  var value = evt.target.control.value;

  function sortByChallenge(element) {
    return element.getAttribute('data-challenge');
  }
  
  function sortByDatePosted(element) {
    return element.getAttribute('data-date-posted');
  }
  
  function sortByHandle(element) {
    return element.getAttribute('data-handle');
  }


  var options;
  if (value === 'challenge') {
    options = {
      by: sortByChallenge,
    };
  } else if (value === 'handle') {
    options = {
      by: sortByHandle,
    };
  } else if (value === 'posted-old-new') {
    options = {
      reverse: false,
      by: sortByDatePosted,
    };
  } else if (value === 'posted-new-old') {
    options = {
      reverse: true,
      by: sortByDatePosted,
    };
  } else {
    options = {};
  }

  this.shuffle.sort(options);
};



document.addEventListener('DOMContentLoaded', function () {
  window.mapex = new MapExplorer(document.querySelector('.my-shuffle-container'));
});
