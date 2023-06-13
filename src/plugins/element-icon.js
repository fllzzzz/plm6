import { Close, Delete, Expand, Fold, Edit, Loading, View, Plus, ArrowDown, ArrowRight, ArrowLeft, UploadFilled, QuestionFilled } from '@element-plus/icons-vue'

const components = new Map([
  ['ElIconClose', Close],
  ['ElIconDelete', Delete],
  ['ElIconExpand', Expand],
  ['ElIconFold', Fold],
  ['ElIconLoading', Loading],
  ['ElIconView', View],
  ['ElIconPlus', Plus],
  ['ElIconArrowDown', ArrowDown],
  ['ElIconArrowRight', ArrowRight],
  ['ElIconArrowLeft', ArrowLeft],
  ['ElIconEdit', Edit],
  ['ElIconUploadFilled', UploadFilled],
  ['ElIconQuestionFilled', QuestionFilled]
])

const useElementPlus = (app) => {
  components.forEach((component, name) => {
    app.component(name, component)
  })
}

export default useElementPlus
