library(fundManageR)
library(dplyr)
library(purrr)

# requires github versions of crosstalk and plotly
#devtools::install_github("jcheng5/plotly@joe/feature/crosstalk")
library(plotly)
library(crosstalk)

adv <- get_data_adv_managers_current_period_summary()

ax <- list(
  title = "",
  zeroline = FALSE,
  showline = FALSE,
  showticklabels = FALSE,
  showgrid = FALSE
)

sd <- SharedData$new(
  adv %>%
    # should already be sorted by AUM but make sure
    mutate_all(unclass) %>%
    arrange(desc(amountAUMTotal)) %>%
    head(10),
  key = ~idCRD,
  group = "grp1"
)

p1 <- sd %>%
  plot_ly( y = ~nameEntityManager, x = ~amountAUMTotal) %>%
  add_markers() %>%
  plotly::layout(
    # make room for long names
    margin = list(l = 250),
    xaxis = list(
      rangemode = "tozero"
    ),
    yaxis = c(
      ax,
      categoryorder = "array",
      categoryarray = sd$data() %>%
        arrange(desc(amountAUMTotal)) %>%
        select(nameEntityManager)
    ),
    dragmode = "select",
    hovermode = "x"
  )

p1

# make a blank json editor
#  just discovered that I did not provide elementId arg
#  will fix that later in listviewer
je <- listviewer::jsonedit(width=400)

script <- HTML(sprintf(
"
function(el,x){
  // not good practice but do it because its easy for now
  window.top10_data = %s;
  // nest for easier use
  window.top10_data = Plotly.d3.nest()
    .key(function(d){return d.nameEntityManager})
    .entries(top10_data.idCRD);

  // we made a group named grp1 with our SharedData in R
  var grp = crosstalk.group('grp1');
  // change data in jsonedit when selection changes
  grp.var('selection').on('change',function(val){
    var je = HTMLWidgets.widgets.filter(
      function(widget){
        return widget.name === 'jsonedit';
      }
    )[0];
    if(Array.isArray(val.value) && val.value.length > 0){
      je.renderValue(
        el,
        {data:top10_data.filter(
          function(d){
            return val.value.indexOf(d.values[0].idCRD) > -1;
          }
        )},
        {editor:null}
      );
    }
  });
}
",
jsonlite::toJSON( top10, dataframe = "rows" )
))

# combine the plotly and jsonedit so we can link
#   with our script
browsable(
  tagList(
    tags$div(p1, style="display:inline-block; width:48%"),
    tags$div(
      htmlwidgets::onRender(je, script),
      style="display:inline-block; width:48%"
    )
  )
)
