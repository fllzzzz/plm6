import commonButton from '@comp-common/common-button'
import commonSelect from '@comp-common/common-select'
import commonRadio from '@comp-common/common-radio'
import commonRadioButton from '@comp-common/common-radio-button'

const components = new Map([
  ['commonButton', commonButton],
  ['commonSelect', commonSelect],
  ['commonRadio', commonRadio],
  ['commonRadioButton', commonRadioButton]
])

const useComponents = (app) => {
  // 组件注册
  components.forEach((component, name) => {
    app.component(name, component)
  })
}

export default useComponents
