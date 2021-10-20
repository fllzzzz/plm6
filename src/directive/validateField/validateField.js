export default {
  componentUpdated(el, binding, vnode) {
    const { value } = binding // value = 表单对象字段
    const prop = vnode.componentInstance.prop
    const vm = vnode.context
    const form = value || 'form'
    const newVal = vm[form] && vm[form][prop]
    const oldVal = vm.$__oldForm && vm.$__oldForm[prop]
    if (newVal !== oldVal) {
      if (!vm.$isNotBlank(newVal) && !vm.$isNotBlank(oldVal)) {
        // vm.$nextTick(v => {
        //   vm.clearValidate() // 校验
        // })
      } else {
        vm.validateField(prop) // 校验
      }
    }
    if (!vm.$__oldForm) {
      vm.$__oldForm = {}
    }
    vm.$__oldForm[prop] = newVal
  }
}
