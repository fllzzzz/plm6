import { Expand, Fold, Loading, View, Plus, ArrowDown, ArrowRight, UploadFilled } from '@element-plus/icons'

const components = new Map([
  ['ElIconExpand', Expand],
  ['ElIconFold', Fold],
  ['ElIconLoading', Loading],
  ['ElIconView', View],
  ['ElIconPlus', Plus],
  ['ElArrowDown', ArrowDown],
  ['ElArrowRight', ArrowRight],
  ['ElIconUploadFilled', UploadFilled]
])

const useElementPlus = (app) => {
  components.forEach((component, name) => {
    app.component(name, component)
  })
}

export default useElementPlus
