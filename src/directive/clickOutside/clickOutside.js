export default {
  bind(el, binding) {
    // console.log(el, binding)// el为当前绑定自定义指令的dom元素
    // binding为当前元素对象
    // 可以发现在当前元素对象上存在一些参数vue官网都有写到
    const { value, expression } = binding
    const clickHandler = e => {
      // TODO: 注意table fixed
      // console.log('el.contains(e.target)', el, e.target, el.contains(e.target))
      // console.log(el, 'e.target', e.target)
      // event对象
      if (el.contains(e.target)) {
        // contains方法用来查看dom元素的包含关系，
        // 判断当前点击元素是否嵌套在el元素内 是就返回false
        return false
      }
      if (value && value instanceof Object && value['o'] && typeof value['o'][value['k']] === 'boolean') {
        // binding.value为当前指令所绑定的值
        // 因为项目的原因 这里隐藏的条件是接口返回的参数实现条件渲染判断
        value['o'][value['k']] = false
      }
      if (expression && value) {
        // expression为当前指令绑定的函数如果绑定了函数则执行当前函数
        if (typeof value === 'function') {
          value(e)
        }
      }
    } // 绑定变量(随便取)并在unbind销毁
    el.ClickOutHide = clickHandler
    document.addEventListener('click', clickHandler)
    // console.log(el.ClickOutHide)// 这里打印出来发现是当前函数本身
  },
  unbind(el, binding) {
    // 销毁方法
    document.removeEventListener('click', el.ClickOutHide)
    delete el.ClickOutHide
  }
}
