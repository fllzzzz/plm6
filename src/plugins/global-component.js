import commonSelect from '@common-comp/common-select'
import commonRadio from '@common-comp/common-radio'
import commonRadioButton from '@common-comp/common-radio-button'

const components = [
  commonSelect,
  commonRadio,
  commonRadioButton
]

const useComponents = (app) => {
  // 组件注册
  components.forEach((component) => {
    app.component(component.name, component)
  })
}

export default useComponents
