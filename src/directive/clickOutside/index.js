import clickOutside from './clickOutside'

const install = function (Vue) {
  Vue.directive('click-outside', clickOutside)
}

if (window.Vue) {
  window['click-outside'] = clickOutside
  Vue.use(install) // eslint-disable-line
}

clickOutside.install = install
export default clickOutside
