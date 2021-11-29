import {
  Expand,
  Fold,
  Loading,
  View,
  ArrowDown,
  ArrowRight
} from '@element-plus/icons'

const components = new Map([
  ['ElIconExpand', Expand],
  ['ElIconFold', Fold],
  ['ElIconLoading', Loading],
  ['ElIconView', View],
  ['ElArrowDown', ArrowDown],
  ['ElArrowRight', ArrowRight]
])

const useElementPlus = (app) => {
  components.forEach((component, name) => {
    app.component(name, component)
  })
}

export default useElementPlus
