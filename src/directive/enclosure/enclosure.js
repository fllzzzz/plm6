import store from '@/store'
import { projectTypeEnum } from '@/utils/enum/modules/contract'

function delChild(el) {
  for (let i = 0; i < el.children.length; i++) {
    const text = el.children[i].innerText
    if (text.indexOf('结构') !== -1 || text.indexOf('构件') !== -1 || text.indexOf('零件') !== -1) {
      el.removeChild(el.children[i])
      delChild(el)
      break
    }
  }
}

export default {
  inserted(el, binding, vnode) {
    const projectType = store.getters && store.getters.currentProjectType
    // 项目为围护时：el-radio-group 不显示结构、构件、零件等
    if (projectType & projectTypeEnum.ENCLOSURE.V) {
      // 删除结构等按钮
      delChild(el)
      // 只剩两个按钮 且第一个为全部时 删除全部
      if (el.children && el.children.length === 2 && el.children[0].innerText === '全部') {
        el.removeChild(el.children[0])
      }
      // 重新赋值
      const vm = vnode.context
      const { value } = binding
      const label = el.children[0].__vue__.label
      const field = value.field.split('.')
      let _obj = vm
      for (let x = 0; x < field.length; x++) {
        if (x === field.length - 1) {
          _obj[field[x]] = label
        } else {
          _obj = vm[field[x]]
        }
      }
    } else {
      return true
    }
  }
  // // 切换项目可触发
  // update(el, binding, vnode) {
  //   console.log(vnode)
  // }
}
