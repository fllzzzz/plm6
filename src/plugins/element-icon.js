import { Close, Delete, Expand, Fold, Edit, Loading, View, Plus, ArrowDown, ArrowRight, ArrowLeft, UploadFilled, QuestionFilled } from '@element-plus/icons'

const components = new Map([
  ['ElClose', Close],
  ['ElDelete', Delete],
  ['ElIconExpand', Expand],
  ['ElIconFold', Fold],
  ['ElIconLoading', Loading],
  ['ElIconView', View],
  ['ElIconPlus', Plus],
  ['ElArrowDown', ArrowDown],
  ['ElArrowRight', ArrowRight],
  ['ElArrowLeft', ArrowLeft],
  ['ElEdit', Edit],
  ['ElIconUploadFilled', UploadFilled],
  ['ElQuestionFilled', QuestionFilled]
])

const useElementPlus = (app) => {
  components.forEach((component, name) => {
    app.component(name, component)
  })
}

export default useElementPlus
