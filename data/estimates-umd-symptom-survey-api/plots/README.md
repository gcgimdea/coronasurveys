## Plots of batched-smoothed estimates (by country)

Plots represent the effect of several d = population / batch_size. Currently, we show the results of filling in the blanks (NA's) in the tails, after the batching procedure (variable: *batched_pct_cli*). We use the observed  *pct_cli* estimates by UMD to fill these NA's (same as in Folder **fill_pct_cli_pre_batched_smoothing**).

### Folders

Some trials comparing the effect of filling the blanks (NA's) due to the batching procedure (variable: *batched_pct_cli*). We tried different possibilities:

* **fill_pct_cli_smooth_pre_batched_smoothing_only_1st_and_last**: filling in only the first and last NA elements, using the first and last *pct_cli_smooth*. Only then, the estimates are smoothed. 

* **fill_pct_cli_smooth_pre_batched_smoothing**: filling in all the NA's before and after the first and last non-NA, respectively. We use *batched_pct_cli* to fill in and then the estimates are smoothed. 

* **fill_pct_cli_pre_batched_smoothing**: filling in all the NA's before and after the first and last non-NA, respectively. We use *pct_cli* to fill in and then the estimates are smoothed. 

* **fill_pct_cli_smooth_post_batched_smoothing**: we smooth the batched estimates. Then, we discard the estimates obtained for the NA's before and after the first and last non-NA, respectively. Instead, we use the *pct_cli_smooth* estimates.