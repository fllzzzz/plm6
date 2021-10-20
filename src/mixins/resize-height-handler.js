/**
 * author：duhh
 * 与table-height-handler 基本相同。 用于弹窗处理，参数较为简单
 */
export default {
  beforeMount() {
    window.addEventListener('resize', this.$__windowSizeHandler, { passive: false })
  },
  beforeDestroy() {
    window.removeEventListener('resize', this.$__windowSizeHandler)
  },
  mounted() {
    this.$__windowSizeHandler()
  },
  data() {
    return {
      clientRect: {},
      extraHeight: 0,
      extraElHeight: 0,
      extraEl: '',
      percentage: 1 // 0-1
    }
  },
  computed: {
    $_height() {
      let percentage = this.percentage
      if (percentage > 1) percentage = 1
      if (percentage < 0) percentage = 0
      return +((((this.clientRect.clientHeight || 0) * percentage) - this.extraHeight - this.extraElHeight).toFixed(0))
    }
  },
  methods: {
    $__windowSizeHandler() {
      const rect = document.documentElement
      this.$set(this.clientRect, 'clientHeight', rect.clientHeight || 0)
      this.$set(this.clientRect, 'clientWidth', rect.clientWidth || 0)
      this.extraElHeight = this.$__getExtraElHeight()
    },
    $__getExtraElHeight() {
      let headHeight = 0
      if (typeof this.extraEl === 'string') {
        const heads = this.extraEl.split(',')
        heads.forEach((h) => {
          let type
          let rect
          h = h.trim()
          if (h.substr(0, 1) === '.') {
            type = 'class'
          }
          if (h.substr(0, 1) === '#') {
            type = 'id'
          }
          if (!type) return
          h = h.substr(1, h.length)
          let headDom
          if (type === 'class') {
            headDom = document.getElementsByClassName(h)
            rect = headDom && headDom.length > 0 ? headDom[0].getBoundingClientRect() : {}
            headHeight += rect.height || 0
          }
          if (type === 'id') {
            rect = document.getElementById(h)
            headHeight += rect ? rect.offsetHeight : 0
          }
        })
      }
      return headHeight
    }
  }

}
