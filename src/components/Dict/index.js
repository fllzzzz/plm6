import Dict from './Dict'

const install = function (Vue) {
  Vue.mixin({
    inject: {
      dictExtend: {
        from: 'dictExtend',
        default: '' // 设置默认值避免出现 injection “xxx” not found
      }
    },
    data() {
      if (this.$options.dicts instanceof Array) {
        const dictData = {
          dict: {},
          label: {}
        }
        return {
          dictData
        }
      }
      return {}
    },
    computed: {
      dict() {
        return Object.assign(this.dictExtend, this.dictData || {})
      }
    },
    watch: {
      dict: {
        handler(val) {
          this.$forceUpdate()
        },
        deep: true
      }
    },
    beforeCreate() {
      if (this.$options.dicts instanceof Array) {
        this._provided = {
          ...this._provided,
          dictExtend: this.dict || {}
        }
      }
    },
    created() {
      if (this.$options.dicts instanceof Array) {
        new Dict(this.dictData).init(this.$options.dicts, () => {
          this.$nextTick(() => {
            this.$emit('dictReady')
          })
        })
      }
    }
  })
}

export default { install }
