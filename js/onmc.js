$(document).on('click', '.itemID', function () {
  Shiny.onInputChange('last_btn',this.id);
  var bisabuelo = this.parentNode.parentNode.parentNode
  var button = bisabuelo.querySelector('button')
  var isActive = document.querySelector('.butTemas.active')
  if (isActive) {
    isActive.classList.remove('active')
  }
  button.classList.add('active')
});


$(document).on('click', '.itemIDTabla', function () {
  Shiny.onInputChange('last_btnTabla',this.id);
    var bisabuelo = this.parentNode.parentNode.parentNode
  var button = bisabuelo.querySelector('button')
  var isActive = document.querySelector('.butTemas.activeTab')
  if (isActive) {
    isActive.classList.remove('activeTab')
  }
  button.classList.add('activeTab')

});



$(document).on('click', '.itemIDCron', function () {
  Shiny.onInputChange('last_cron',this.id);
    var bisabuelo = this.parentNode.parentNode.parentNode
  var button = bisabuelo.querySelector('button')
  var isActive = document.querySelector('.butTemas.activeCron')
  if (isActive) {
    isActive.classList.remove('activeCron')
  }
  button.classList.add('activeCron')
});




/*
  var bisabuelo = this.parentNode.parentNode.parentNode
  var button = bisabuelo.querySelector('button')
  var isActive = document.querySelectorAll('.butTemas.active')
  if (isActive) {
    Array.prototype.slice.call(isActive).forEach(function (b) {
      b.classList.remove('active')
    })
  }
  button.classList.add
  */
