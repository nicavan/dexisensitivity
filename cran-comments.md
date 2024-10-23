## Resubmission
Thank you for your review.
This is a resubmission. In this version, I have :

* Added relevant references in the DESCRIPTION file.

* Deleted spaces at linebreaks and some linebreaks in the description field.

* Added \value to some .Rd files (compare_scenarios, describe, save_options, save_scenarios, show_scenario).

* Used messages() instead of cat() in monte_carlo(). The argument 'verbose' was deleted.

* Changed estimate_mc_time and estimate_aov_time so that they return a character string with the estimated time rather than writing it in the console. This way, its main goal (preventing too long analyses by showing this estimated time) is still fulfilled, but users can choose to store the result in an object and not show it in the console.

* Introduced on.exit() in plot_sentitivity_index() to restore graphical parameters.


## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new release.
