#Pas d'argent

##Introduction

This is a personal project to create a home economy managent software in R language. I'm not trying to create a commercial product here (yet), but a series of scripts to help me keeping an eye over my economy.

By the way, "pas d'argent" means "no money" in French `:P` Ah, and I am not economist, so... Please, indulge me if I am choosing incorrect terms to describe what I have done here.

##Why?

The main idea behind all this is that I should be able to answer one simple question at any time: how much budget I have this month? In other words, how much money I can spend until the end of the month?

I started keeping track of my incomes and expenses few years ago. At first, I wrote down just the inputs (if I remembered to, which didn't happen frequently). And, even though I am a software engineer, I used an old, analog and faithful Moleskine and a fountain pen to do the job. If you have to do it the old way, at least do it with style.

One of the problems I had with this approach was, of course, how difficult it was to know how much I could spare in a certain moment. There were expenses I knew for sure would happen (like house renting, Internet or electricity), but there were many other expenses I could't keep track of properly.

Then I started making "projections": if I knew I would have an expense but I didn't know the exact amount, I guessed, _and I took the expense for real_. Then, as the real expenses happened, I wrote them down, substracting from the initial projection and adjusting my budget.

But this is all but feverish nonsense without a proper example.

##Example

Let's say I have this sheet of incomes/expenses with totally unrealistic data:

|`Id`|`Date`|`Is.Budget`|`Is.Closed`|`Type`|`Amount`|`Reference`|`Comments`|
|---|---|---|---|---|---|---|---|
|1|01/05/2015|||Payroll|2000|||
|2|02/05/2015|Yes||Fuel|-180||Fuel estimation|
|3|03/05/2015|||House renting|-500|||
|4|04/05/2015|Yes||Life insurance|-200||Life insurance estimation|
|5|05/05/2015|Yes|Yes|Hairdresser|-50||Hairdresser estimation|
|6|06/05/2015|||Fuel|-50|2|Fuel|
|7|07/05/2015|||Hairdresser|-40|5|Hairdresser|
|8|08/05/2015|||Life insurance|-250|4|Life insurance|

The columns are pretty self-explanatory, but still:

* `Id`: Each entry has an unique id. This id is used in the `Reference` column.
* `Date`: The date on which the entry took place.
* `Is.Budget`: A "yes/no" value showing if the entry is an estimation.
* `Is.Closed`: A "yes/no" value which applies only if the entry is an estimation. It shows if the estimation is closed (more on that in a moment).
* `Type`: The type of income/expense. It's entirely up to you, with as many categories as you desire.
* `Amount`: Enough said.
* `Reference`: This column is used to link _real_ entries to _estimated_ entries. It's related to the `Is.Budget` and `Is.Closed` columns.

The nature of the entries deserve further explanation.

##Types of entry

There is a subtle taxonomy which divides the entries considering _their weight into the final balance_. This sounds pretentious. It is.

###Incomes

Self-explanatory. These are the positive entries, and are annotated as-is.

###Direct expenses

These are the expenses which aren't related to any estimation (so, the `Reference` column has no value). For example, what you pay for a coffee in the bar or an online course. These expenses are not related to anything else.

###Estimations

These are expenses which amount I don't know in advance, but I know they will take place eventually, so I guess the amount. The `Is.Budget` column has to have a "Yes" value.

###Linked expenses

These are expenses related to a previous estimation, so the `Reference` column has a value linking the expense to the estimation. There can be more than one expense linked to a single estimation.

##More on that

Well, Ok, maybe it's not crystal clear, but let me explain.

The incomes and the direct expenses are the easy part. With these two, you can keep track of your expenses... But you won't be able to predict if a month will be specially difficult, because you are reactive, not proactive (you are annotating the expenses just as they happen).

But if you want to get a better idea of the picture, you will need to guess sometime. And it will make a _huge_ difference.

For example, let's suppose I know that I will spend 180 €/month in fuel, give or take. Then, at the very beginning of the month, I annotate an expense of -180 and I substract that amount from the income. I don't know how much I will spend refueling the car yet, but I am taking this amount as it had been produced yet.

Let's suppose then that I refuel my car two times that month, one spending 50 € and the other spending 60 €. These two expenses are _linked to the initial guess_, so that they are not substracted from the income up front. Instead, they are just annotated, waiting for the initial estimation to be closed.

What? A closed estimation? Yep, that's the thing...

For the moment, we have:

* An estimation of -180 € in fuel.
* Real expenses with an amount of -110 € in fuel.

We certainly have not reached the limit of the initial estimation, so we have 70 € yet available to refuel, if we need to. The estimation is still open.

So, which are the conditions in which we could consider an estimation as closed? There are three cases:

1. The estimation has been manually closed. This could happen if the estimation is a "one-shot" (for instance, hairdresser) with just one real expense, or if we positively know that no more real expenses will be linked to that estimation.
2. The estimation has been annotated in the past. Every estimation which belongs to a previous month is automatically closed.
3. The estimation has been exceeded. If we miscalculate an estimation, then it no longer makes any sense, so the real expenses linked to that estimation will be took into account.

##Your budget

Remember the main idea: knowing the remaining budget any time over the current month. The formula to calculate the budget is:

```
current.budget = incomes - direct.expenses - open.estimations - real.expenses.of.closed.estimations
```

So, back to our example, the current budget is:

```
incomes = 2000
direct.expenses = (-500) + (-50) + (-40) = -590
open.estimations = -180
real.expenses.of.closed.estimations = (-50) + (-250) = -300
current.budget = 2000 - 590 - 180 - 300 = 930
```

So, with our current estimations, we have yet 930€ to spend this month. Note these peculiarities:

* The entry #2 is open, because the only real expense linked to that estimation (#6) does not exceed the -180€ guessed. So, #2 weighs in the balance, but #6 not.
* The entry #4 is closed, _although it has not been manually closed_. It is closed because the entry #8 exceeds the amount guessed. Remember: when an estimation is exceeded, it does not longer make any sense. So, #8 weighs in the balance, but #4 not.
* The entry #5 has been manually closed because it's a "one-shot" estimation (once finished, there will be no more expenses linked to it). So, #6 weighs in the balance, but #6 not.

##Files

There are two files right now in the repo:

* `init.R`: Initialization functions. This loads a Google Spreadsheet containing the income/expenses data into the `expenses_data` variable.
* `calc.R`: Statistical calculations. This contanins several functions to work with the data loaded in the first script (for example, getting the estimated monthly budget or getting a summary of the monthly budgets).

##Additional notes

* The spreadsheet I am using is private, of course, but I intend to create a playground, public, read-only sheet to illustrate how the system works.