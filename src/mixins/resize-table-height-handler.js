/**
 * author: duhh
 * TODO: 可优化
 */
export default {
  beforeMount() {
    window.addEventListener('resize', this.$_windowSizeHandler, { passive: false })
  },
  beforeDestroy() {
    window.removeEventListener('resize', this.$_windowSizeHandler)
  },
  mounted() {
    this.$_windowSizeHandler()
  },
  data() {
    return {
      clientRect: {}, // 当前窗口边距信息
      currentHeader: {}, // 当前高度信息 对象中存放height
      head: {} // 当前头部信息
    }
  },
  provide() {
    return {
      clientRect: this.clientRect,
      $_tableMaxHeight: this.$_tableMaxHeight
      // $_windowSizeHandler: this.$_windowSizeHandler
    }
  },
  methods: {
    /**
     *
     * @param {string} head  所处模块的头部盒子，可以“，”分隔传多个
     * @param {boolean} paginate 表格是否分页
     * @param {boolean} isAppContainer 是否在app-container的盒子中
     * @param {number} extra 额外高度
     * @param {boolean} hasFixedHeader 是否固定头部(TODO: 此处设计有问题，不应该当参数传入，未处理不固定的情况)
     * @param {number} hPct 高度比例
     * @param {dom} dom
     * @returns
     */
    $_tableMaxHeight({ head = '.head-container', paginate = true, isAppContainer = true, extra = 0, hasFixedHeader = true, hPct = 100, dom } = {}) {
      let headHeight = 0
      let fixedHeight = 0
      const paginateHeight = paginate ? 40 : 0
      const containerPadding = isAppContainer ? 40 : 0
      if (head) {
        this.head = head
        headHeight = this.currentHeader.height
        // head为字符串类型，调用获取高度
        if (typeof head === 'string') {
          headHeight = this.getHeadHeight(head)
        }
        // head为对象类型,直接从对象中获取高度
        if (typeof head === 'object') {
          headHeight = head.offsetHeight || 0
        }
      }
      const app = document.getElementById('app')
      if (!app || !dom || app.contains(dom)) { // 判断是否在app的盒子中
        const fixedHeader = document.getElementsByClassName('fixed-header')
        if (hasFixedHeader && fixedHeader && fixedHeader.length > 0) {
          const fixedHeadRect = fixedHeader[0].getBoundingClientRect() || {}
          fixedHeight = fixedHeadRect.height || 0
        }
      }

      const clientHeight = this.clientRect.clientHeight || document.documentElement.clientHeight
      if (clientHeight) {
        const height = clientHeight * (hPct / 100) - fixedHeight - containerPadding - paginateHeight - headHeight - extra
        // console.log(clientHeight, fixedHeight, containerPadding, paginateHeight, headHeight, extra)
        return height && height > 0 ? height : 0
      } else {
        return 0
      }
    },
    $_windowSizeHandler() {
      const rect = document.documentElement
      this.$set(this.clientRect, 'clientHeight', rect.clientHeight || 0)
      this.$set(this.clientRect, 'clientWidth', rect.clientWidth || 0)
      if (this.head) {
        const height = this.getHeadHeight(this.head)
        this.$set(this.currentHeader, 'height', height || 0)
      }
    },
    /**
     * 获取head的高度
     * @param {*} head 可以“，”分隔传入多个（盒子嵌套的情况）。传入head的class（以‘.’开头） 或者 id（以‘#’开头）
     * @returns
     */
    getHeadHeight(head) {
      let headHeight = 0
      if (typeof head === 'string') {
        const heads = head.split(',')
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
