
python main.py -study provide -n 10000 -r 15000 -t "Provide_bystool_asym_byseason" -u True -c "Initial testing, this is 1.) using unsigned integers to reduce memory usage, and 2.) increased I/O to free up more memory. This is by observation, and the timeseries subsetting is by season rather than strictly by time." -f simple_non_condensed_provide_select_15Jun20.csv -stool asym -type simple_list -se True -opv False -multip True -bystool True -b 12 -jobid ${PBS_JOBID} -seasons T -country none
