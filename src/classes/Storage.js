export default class Storage {
  constructor(props) {
    this.props = props || {}
    this.source = this.props.source || window.localStorage
    this.initRun()
  }

  /**
   * set 存储方法
   * @ param {String}     key 键
   * @ param {String}     value 值，存储的值可能是数组/对象，不能直接存储，需要转换 JSON.stringify
   * @ param {String}     expired 过期时间，以分钟为单位
   */
  initRun() {
    const reg = new RegExp('__expires__')
    const data = this.source
    const list = Object.keys(data)
    if (list.length > 0) {
      list.map((key, v) => {
        if (!reg.test(key)) {
          const now = Date.now()
          const expires = data[`${key}__expires__`] || Date.now + 1
          if (now >= expires) {
            this.remove(key)
          }
        }
        return key
      })
    }
  }

  /**
   * set 存储方法
   * @ param {String}     key 键
   * @ param {String}     value 值，
   * @ param {String}     expired 过期时间，以毫秒为单位，非必须
   */
  set(key, value, expired) {
    const source = this.source
    source[key] = JSON.stringify(value)
    if (expired) {
      source[`${key}__expires__`] = Date.now() + expired
    }
    return value
  }

  /**
   * get 获取方法
   * @ param {String}     key 键
   * @ param {String}     expired 存储时为非必须字段，所以有可能取不到，默认为 Date.now+1
   */
  get(key) {
    const source = this.source
    const expired = source[`${key}__expires__`] || Date.now + 1
    const now = Date.now()

    if (now >= expired) {
      this.remove(key)
      return
    }

    let res = source[key]
    if (source[key]) {
      // 处理json无法解析的格式
      switch (source[key]) {
        case 'undefined': res = undefined
          break
        case undefined: res = undefined
          break
        default:
          res = JSON.parse(source[key])
      }
    }
    return res
  }

  remove(key) {
    const data = this.source
    const value = data[key]
    delete data[key]
    delete data[`${key}__expires__`]
    return value
  }
}
