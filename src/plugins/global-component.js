import commonButton from '@comp-common/common-button'
import commonSelect from '@comp-common/common-select'
import commonRadio from '@comp-common/common-radio'
import commonRadioButton from '@comp-common/common-radio-button'
import commonTable from '@comp-common/common-table'
import commonDrawer from '@comp-common/common-drawer'
import commonDialog from '@comp-common/common-dialog'

const components = new Map([
  ['commonButton', commonButton],
  ['commonSelect', commonSelect],
  ['commonRadio', commonRadio],
  ['commonRadioButton', commonRadioButton],
  ['commonTable', commonTable],
  ['commonDrawer', commonDrawer],
  ['commonDialog', commonDialog]
])

const useComponents = (app) => {
  // 组件注册
  components.forEach((component, name) => {
    app.component(name, component)
  })
}

export default useComponents
