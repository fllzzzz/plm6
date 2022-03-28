import commonInputNumber from '@comp-common/common-input-number'
import commonButton from '@comp-common/common-button'
import commonTipButton from '@comp-common/common-tip-button'
import commonSelect from '@comp-common/common-select'
import commonRadio from '@comp-common/common-radio'
import commonRadioButton from '@comp-common/common-radio-button'
import commonTable from '@comp-common/common-table'
import commonDrawer from '@comp-common/common-drawer'
import commonDialog from '@comp-common/common-dialog'
import projectRadioButton from '@comp-common/project-radio-button'
import printTable from '@comp-common/print/print-table'
import TableCellTag from '@comp-common/table-cell-tag/index.vue'
import ProjectCascader from '@/components-system/base/project-cascader'

const components = new Map([
  ['commonInputNumber', commonInputNumber],
  ['commonButton', commonButton],
  ['commonTipButton', commonTipButton],
  ['commonSelect', commonSelect],
  ['commonRadio', commonRadio],
  ['commonRadioButton', commonRadioButton],
  ['commonTable', commonTable],
  ['commonDrawer', commonDrawer],
  ['commonDialog', commonDialog],
  ['projectRadioButton', projectRadioButton],
  ['printTable', printTable],
  ['tableCellTag', TableCellTag],
  ['projectCascader', ProjectCascader]
])

const useComponents = (app) => {
  // 组件注册
  components.forEach((component, name) => {
    app.component(name, component)
  })
}

export default useComponents
