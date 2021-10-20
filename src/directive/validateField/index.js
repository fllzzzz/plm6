import validateField from './validateField'

const install = function (Vue) {
  Vue.directive('validate-field', validateField)
}

if (window.Vue) {
  window['validate-field'] = validateField
  Vue.use(install) // eslint-disable-line
}

validateField.install = install
export default validateField
