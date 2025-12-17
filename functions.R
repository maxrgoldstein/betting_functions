implied_probability <- function(odds){
  if (odds < -100){
    return(-odds/(-odds+100))}
  if (odds >= 100){
    return(100/(100+odds))}
  else{
    return(NA)}}
true_over_probability <- function(over_odds,under_odds){
  if ((over_odds < -100 | over_odds >= 100) & (under_odds < -100 | under_odds >= 100) & implied_probability(over_odds)+implied_probability(under_odds) >= 1){
    return(implied_probability(over_odds)/(implied_probability(over_odds)+implied_probability(under_odds)))}
  else{
    return(NA)}}
odds <- function(probability){
  if (probability >= 0 & probability < 0.5){
    return(100/probability-100)}
  if (probability >= 0.5 & probability <= 1){
    return(-100/(1/probability-1))}
  else{
    return(NA)}}
expected_winnings <- function(odds,wtp_odds,stake){
  if (odds < -100){
    return(implied_probability(wtp_odds)*stake*-100/odds-(1-implied_probability(wtp_odds))*stake)}
  if (odds >= 100){
    return(implied_probability(wtp_odds)*stake*odds/100-(1-implied_probability(wtp_odds))*stake)}
  else{
    return(NA)}}
expected_income_change <- function(odds,wtp_odds,stake,tax_rate,loss_deduction_percent){
  if (odds < -100){
    return(implied_probability(wtp_odds)*stake*-100/odds*(100-tax_rate)/100-((1-implied_probability(wtp_odds))*stake+(1-implied_probability(wtp_odds))*stake*(100-loss_deduction_percent)/100))}
  if (odds >= 100){
    return(implied_probability(wtp_odds)*stake*odds/100*(100-tax_rate)/100-((1-implied_probability(wtp_odds))*stake+(1-implied_probability(wtp_odds))*stake*(100-loss_deduction_percent)/100))}
  else{
    return(NA)}}
#assuming bettor will pay taxes on 10 percent of losses
