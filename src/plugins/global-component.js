import commonButton from '@comp-common/common-button'
import commonSelect from '@comp-common/common-select'
import commonRadio from '@comp-common/common-radio'
import commonRadioButton from '@comp-common/common-radio-button'
import commonTable from '@comp-common/common-table'

const components = new Map([
  ['commonButton', commonButton],
  ['commonSelect', commonSelect],
  ['commonRadio', commonRadio],
  ['commonRadioButton', commonRadioButton],
  ['commonTable', commonTable]
])

const useComponents = (app) => {
  // 组件注册
  components.forEach((component, name) => {
    app.component(name, component)
  })
}

export default useComponents
