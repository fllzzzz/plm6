<script>
export default {
  // 使用MenuItem会报错
  name: 'Highlight',
  functional: true,
  props: {
    text: {
      type: String,
      default: ''
    },
    highlight: {
      type: String,
      default: ''
    }
  },
  render(h, context) {
    // TODO:批量高亮节点
    // 先按字数排序
    // const { text, highlight } = context.props
    const vnodes = []

    // const replaceReg = new RegExp(highlight, 'g')

    // const elHtml = h('span',
    //   {}, highlight)
    // const ff = h('span',
    //   {
    //     style: 'color:red'
    //   }, text)
    // const t = text.replace(replaceReg, elHtml)
    // console.log(t)
    // console.log(h)
    // vnodes.push(ff)

    // if (icon) {
    //   vnodes.push(<svg-icon icon-class={icon}/>)
    // }

    // if (title) {
    //   vnodes.push(<span slot='title'>{(title)}</span>)
    // }
    // if (icon) {
    //   const elHtml = h('svg-icon', {
    //     attrs: {
    //       'icon-class': icon
    //     }
    //   })

    //   vnodes.push(elHtml)
    // }

    // if (title) {
    //   const elHtml = h('span', {
    //     slot: 'title',
    //     attrs: {
    //       class: 'system-menu-title'
    //     }
    //   }, title)
    //   vnodes.push(elHtml)
    // }
    return vnodes
  }
}
</script>
