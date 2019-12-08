# _template.R
# define the header and footer HTML



# Index Page --------------------------------------------------------------

index_header <- glue('
<!doctype html>
<html lang="en">
  <head>
    <meta charset="utf-8">
    <meta name="viewport" content="width=device-width, initial-scale=1, shrink-to-fit=no">

    <link rel="stylesheet" href="css/bootstrap_{BS_THEME}.min.css">

    <title>30 Day Map Challenge #30DayMapChallenge - Introduction</title>
  </head>
  
  
<body>

<nav class="navbar navbar-expand-lg navbar-dark bg-primary">
  <div class="container">
    <span class="navbar-brand">#30DayMapChallenge</span>
    <button class="navbar-toggler" type="button" data-toggle="collapse" data-target="#navbarColor01" aria-controls="navbarColor01" aria-expanded="false" aria-label="Toggle navigation">
      <span class="navbar-toggler-icon"></span>
    </button>
  
    <div class="collapse navbar-collapse" id="navbarColor01">
      <ul class="navbar-nav mr-auto">
        <li class="nav-item active">
          <a class="nav-link" href="#">Intro <span class="sr-only">(current)</span></a>
        </li>
        <li class="nav-item">
          <a class="nav-link" href="maps.html">Map Gallery</a>
        </li>
        <li class="nav-item">
          <a class="nav-link" href="stats.html">Stats</a>
        </li>
        <li class="nav-item">
          <a class="nav-link" href="https://github.com/dakvid/30DayMapChallenge">Source</a>
        </li>
      </ul>
    </div>
  </div>
</nav>

<div class="container">
  
  <div class="page-header" id="banner">
    <div class="row">
      <div class="col-12">
        <h1>30 Day Map Challenge 2019</h1>
        <p class="lead">A collection of awesome maps from Twitter.</p>
      </div>
    </div>
  </div>
')

index_footer <- '
</div>



<script async src="https://platform.twitter.com/widgets.js" charset="utf-8"></script>
</body>
</html>
'



# Stats Page --------------------------------------------------------------

stats_header <- glue('
<!doctype html>
<html lang="en">
  <head>
    <meta charset="utf-8">
    <meta name="viewport" content="width=device-width, initial-scale=1, shrink-to-fit=no">

    <link rel="stylesheet" href="css/bootstrap_{BS_THEME}.min.css">
    <link rel="stylesheet" href="css/custom.css">

    <title>30 Day Map Challenge #30DayMapChallenge - Statistics</title>
  </head>
<body>
')


stats_header_nav <- '
<nav class="navbar navbar-expand-lg navbar-dark bg-primary">
  <div class="container">
    <span class="navbar-brand">#30DayMapChallenge</span>
    <button class="navbar-toggler" type="button" data-toggle="collapse" data-target="#navbarColor01" aria-controls="navbarColor01" aria-expanded="false" aria-label="Toggle navigation">
      <span class="navbar-toggler-icon"></span>
    </button>
  
    <div class="collapse navbar-collapse" id="navbarColor01">
      <ul class="navbar-nav mr-auto">
        <li class="nav-item">
          <a class="nav-link" href="index.html">Intro</a>
        </li>
        <li class="nav-item active">
          <a class="nav-link" href="maps.html">Map Gallery</a>
        </li>
        <li class="nav-item">
          <a class="nav-link" href="#">Stats <span class="sr-only">(current)</span></a>
        </li>
        <li class="nav-item">
          <a class="nav-link" href="https://github.com/dakvid/30DayMapChallenge">Source</a>
        </li>
      </ul>
    </div>
  </div>
</nav>
'


stats_footer <- '
</body>
</html>
'




# Maps Page ---------------------------------------------------------------

maps_header <- glue('
<!doctype html>
<html lang="en">
  <head>
    <meta charset="utf-8">
    <meta name="viewport" content="width=device-width, initial-scale=1, shrink-to-fit=no">

    <link rel="stylesheet" href="css/bootstrap_{BS_THEME}.min.css">
    <link rel="stylesheet" href="css/custom.css">

    <title>30 Day Map Challenge #30DayMapChallenge - Map Gallery</title>
  </head>
<body>
')


maps_header_nav <- '
<nav class="navbar navbar-expand-lg navbar-dark bg-primary">
  <div class="container">
    <span class="navbar-brand">#30DayMapChallenge</span>
    <button class="navbar-toggler" type="button" data-toggle="collapse" data-target="#navbarColor01" aria-controls="navbarColor01" aria-expanded="false" aria-label="Toggle navigation">
      <span class="navbar-toggler-icon"></span>
    </button>
  
    <div class="collapse navbar-collapse" id="navbarColor01">
      <ul class="navbar-nav mr-auto">
        <li class="nav-item">
          <a class="nav-link" href="index.html">Intro</a>
        </li>
        <li class="nav-item active">
          <a class="nav-link" href="#">Map Gallery <span class="sr-only">(current)</span></a>
        </li>
        <li class="nav-item">
          <a class="nav-link" href="stats.html">Stats</a>
        </li>
        <li class="nav-item">
          <a class="nav-link" href="https://github.com/dakvid/30DayMapChallenge">Source</a>
        </li>
      </ul>
    </div>
  </div>
</nav>
'


maps_scripts <- '
<script crossorigin="anonymous" src="https://polyfill.io/v3/polyfill.min.js?features=default%2Ces5%2Ces6%2Ces7"></script>
<script src="js/jquery-3.3.1.slim.min.js"></script>
<script src="js/popper-1.14.7.min.js"></script>
<script src="js/bootstrap-4.3.1.min.js"></script>
<script src="js/lazysizes.min.js" async=""></script>
<script src="js/shuffle.js"></script>
<script src="js/shuffle_the_maps.js"></script>
<script>
// For tooltips on buttons that do other things
$(\'[data-toggle-tt="tooltip"]\').tooltip()
</script>
'


maps_footer <- '
</body>
</html>
'
