import enclosure from './enclosure'

const install = function (Vue) {
  Vue.directive('enclosure', enclosure)
}

if (window.Vue) {
  window['enclosure'] = enclosure
  Vue.use(install) // eslint-disable-line
}

enclosure.install = install
export default enclosure
