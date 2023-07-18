### Experimental Data

This directory contains three data sets that we collected during the field experiment.

`dish_plan.csv`: Contains information on the main dish choices that were available to our participants during our experimental period. Variables are defined as follows:

- `eday`: The experimental day in consecutive numbering.
- `dish`: The name of the main dish. We kept the exact spelling and wording as used within the cash register system to ensure that the data can be matched with cash register data contained in `dish_choices.rds`.
- `co2e100g`: The carbon footprint information for the respective main dish. The calculation of this measure is detailed in the paper. 


`dish_choices.rds`: Contains the transactions data for our experimental participants. This data is derived from raw cash register data. It is organized by transaction items, with several items linked together to form one transaction. An observation is identified by `register_id`/`sheet_id`/`trans_id`/`trans_pos`. Variables are defined as follows:

- `eday`: The experimental day in consecutive numbering.
- `counter`: The canteen has two separate counters: 'yellow', and 'red'. These two counters have distinct food and checkout areas. `counter` identifies where the respective transaction item has been checked out.
- `pcard_id`: A pseudonymized ID identifying a single payment card. While each transaction is linked to such an ID, participants can (and at times do) pay for multiple diners in one transaction. Also, sometimes participants split up their food choice in multiple transactions for technical reasons.
- `register_id`/`sheet_id`: Cash register information identifying a single cash register and a temporal component.
- `trans_id`: A consecutive number identifying transactions for a certain cash register on a given day. Please note that the variable is not consecutive in our data as some transactions contain only non-treated items (such as desserts or drinks) and transactions are sometimes cancelled by participants during the checkout process. 
- `trans_pos`: The position of an item within a transaction. It reflects the order in which the items are listed within the cash register system. Please note that the variable is not consecutive in our data as the items of a transaction also contain non-food (containers and cutlery) or non-treated (e.g., desserts and drinks) items.
- `time`: The time of day of the transaction, measured in hours and minutes.
- `customer_group`: A discrete variable ('Bedienstete', 'Gäste' and 'Studierende'), indicating whether the payment card used for the transactions belongs to a faculty/staff member, a guest, or a student.
- `to_go`: A binary variable indicating whether the food was placed into a to-go container for consumption outside of the canteen.
- `dish`: The dish name as used within the cash register system. The name 'Side dish' indicates containers that contain side dishes that participants can choose besides their main dish. All other dish names are linkable to the data contained in `dish_plan.csv`.
- `weight`: The amount of food contained in the container measured in grams.

  
`tment_data.rds`: Contains information on the treatment administration over the experimental days. Variables are defined as follows:

- `eday`: The experimental day in consecutive numbering.
- `cday`: The calendar day that is identified by the experimental day.
- `tslot1`: The treatment administered during the first treatment slot.
- `tslot2`: The treatment administered during the second treatment slot.
- `exp_eff_size`: The expected pre-registered percentage effect of the second treatment relative to the first treatment.
- `tment_start`: The start of the first treatment, measured in minutes past 11am.
- `tment_change`: The start of the second treatment, measured in minutes past 11am. This is our main treatment change indicator, identified as the event day maximum of `tment_change_max_yellow` and `tment_change_max_red`, presented below. 
- `tment_change_min_red`: As discussed in the paper, we change the treatment so that the participants queuing for food in the respective counter receive consistent treatments throughout their queuing. This means that we start the treatment change in the entrance hall and then change the information on the stage displays and next to the food later. This variable reports the start of the second treatment for the red counter in the entrance hall, measured in minutes past 11am.
- `tment_change_min_yellow`: The start of the second treatment for the yellow counter in the entrance hall, measured in minutes past 11am.
- `tment_change_max_red`: The start of the second treatment for the red counter at the food displays, measured in minutes past 11am.
- `tment_change_max_yellow`: The start of the second treatment for the yellow counter at the food displays, measured in minutes past 11am.
- `tment_end`: As discussed in the paper, on most experimental days some main dishes ran out prior to the end of our experimental period (2pm). On these days, `tment_end` indicates the point of time in minutes past 11am where the first main dish component ran out on both counters.
- `tment_end_red`: The point in time (measured in minutes past 11am) where a main dish ran out for the red counter, yielding a significant change in the choice set for that counter. 
- `tment_end_yellow`: The point in time (measured in minutes past 11am) where a main dish ran out for the yellow counter, yielding a significant change in the choice set for that counter.


Please note: `tment_data.rds` and `dish_choices.rds` are stored in a binary format (RDS) that is R-specific. However, you can easily convert it to CSV by running the following snippet of R code (simply change the file names as desired):

```
df <- readRDS("data/experiment/tment_data.rds")
write.csv(df, "data/experiment/tment_data.csv", row.names = FALSE)
```

  
  
