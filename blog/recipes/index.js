// Write functions like you'd write a recipe

// It starts innocently enough - making a peanut butter cookie
// out of some business requirements:

//   Step 1: Preheat oven to 350F, grease cookie sheet
//   Step 2: In bowl, stir peanut butter and sugar until smooth.
//           Beat in eggs, one at a time, then stir in baking soda, salt, and vanilla.
//   Step 3: Roll dough into 1 inch balls and place 2 inches apart on the sheet.
//   Step 4: Bake for 10 minutes, cool for 5, enjoy!
function makeCookies () {
  oven.setTemperature(350)
  sheet.grease()
  bowl.add(peanutButter)
  bowl.add(sugar)
  bowl.stir()
  bowl.beat(egg)
  bowl.beat(egg)
  bowl.stirIn([bakingSoda, salt, vanilla])
  const dough = bowl.makeDough()
  sheet.add(dough.balls())
  oven.add(sheet)
  oven.setTimer(10)
  const cookies = oven.remove(sheet)
  sheet.cool(10)

  return cookies
}

// Then come the change in requirements - "Hey, you know how we have that recipe to
// bake cookies?  Lets extend it from peanut butter cookies to also making oatmeal raisin!"
//   Step 1: Preheat oven to 325F, grease cookie sheet
//   Step 2: In bowl, stir oats, pumpkin puree and sugar until smooth.
//           Beat in one egg white, then stir in baking soda, salt, and vanilla.
//   Step 3: Roll dough into 1 inch balls and place 2 inches apart on the sheet.
//   Step 4: Bake for 8 minutes, cool for 5, enjoy!

// The astute developer may immediately notice many similarities with their original
// code, and begin extending it by adding conditionals inside the function's logic path.
function makeCookies (type = 'peanut-butter') {
  if (type === 'peanut-butter') {
    oven.setTemperature(350)
  }

  if (type === 'oatmeal-raisin') {
    oven.setTemperature(325)
  }

  sheet.grease()

  if (type === 'peanut-butter') {
    bowl.add(peanutButter)
    bowl.add(sugar)
  }

  if (type === 'oatmeal-raisin') {
    bowl.add(oats)
    bowl.add(pumpkinPuree)
    bowl.add(sugar)
  }

  bowl.stir()

  if (type === 'peanut-butter') {
    bowl.beat(egg)
    bowl.beat(egg)
    bowl.stirIn([bakingSoda, salt, vanilla])
  }

  if (type === 'oatmeal-raisin') {
    bowl.beat(eggWhite)
    bowl.stirIn([bakingSoda, salt, cinnamon])
  }

  const dough = bowl.makeDough()
  sheet.add(dough.balls())
  oven.add(sheet)

  if (type === 'peanut-butter') {
    oven.setTimer(10)
  }

  if (type === 'oatmeal-raisin') {
    oven.setTimer(8)
  }

  const cookies = oven.remove(sheet)
  sheet.cool(10)

  return cookies
}
