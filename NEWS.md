# pressure 0.2.4
* arch_index function added (beta version)
* added link to description
* Minor bug fixes

# pressuRe 0.2.3
* citation details added for Scientific Reports paper
* Minor bug fixes
* more improvements to automask

# pressuRe 0.2.2
* some improvements to novel_automask in create_mask_auto
* added support for time series in mask_analysis 
* added support for xsensor insoles

# pressuRe 0.2

* Support for pliance data added 
* Changes to data structure to make analysis more system agnostic
* Masking is now covered by two system agnostic functions, create_mask_manual and create_mask_auto. create_mask_manual wraps create_mask and the custom option in pedar_mask, plus allows the make mask by selecting sensors option available for data from all systems. create_mask_auto wraps automask and pedar_mask1/2/3. These should make workflows much smoother and will let other masking schemes be more easily added in the future

# pressuRe 0.1.1

* Minor formatting updates
* Changes to plotting functions to address problems when plotting pedar data

# pressuRe 0.1.0

* Added a `NEWS.md` file to track changes to the package.
