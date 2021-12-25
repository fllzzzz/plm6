import { isBlank } from '@/utils/data-type'
import { projectNameFormatter } from '@/utils/project'

// 时间转换
export default {
  mounted(el, binding) {
    resolve(el, binding)
  },
  updated(el, binding) {
    resolve(el, binding)
  }
}

function resolve(el, binding) {
  const { value: { project, split = '、', onlyShortName = false }} = binding
  if (isBlank(project)) return
  let p = []
  if (Array.isArray(project)) {
    p = project
  } else {
    p = [project]
  }
  if (onlyShortName) {
    el.innerText = p.map(v => v.shortName).join(split)
  } else {
    el.innerText = p.map((v) => projectNameFormatter(v, null, false)).join(split)
  }
}
