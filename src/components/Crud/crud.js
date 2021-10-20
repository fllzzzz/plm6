// eslint-disable-next-line no-unused-vars
// import { initData, download } from '@/api/data'
import Vue from 'vue'
import { parseTime } from '@/utils/date'
import checkPermission from '@/utils/permission'
import { fileDownload } from '@/utils/file'
// eslint-disable-next-line no-unused-vars
import { debounce, throttle, deepClone } from '@/utils'

// FIXME: 进入页面第一次loading的时间较长
// TODO: 尝试用更合适的方法解决该问题，并且，由于刷新后js-crud对象依旧是同一个，导致上一个vue实例调用的HOOK方法，可在当前实例的方法上回调（日后尝试销毁CRUD实例重新创建）
// （相同页面，刷新情况）当在上一个页面destroy前调用一次查询，由于toQuery时使用节流，会导致刷新后调用toQuery会被节流阻止，所以在crud重新装载时重置节流（queryPrevious）

// TODO: 弃用节流，改用防抖
// let queryPrevious = 0

// function queryThrottle(func, wait, type = 1) {
//   queryPrevious = 0
//   let timeout
//   return function() {
//     const context = this
//     const args = arguments
//     if (type === 1) {
//       const now = Date.now()
//       if (now - queryPrevious > wait) {
//         func.apply(context, args)
//         queryPrevious = now
//       }
//     } else if (type === 2) {
//       if (!timeout) {
//         timeout = setTimeout(() => {
//           timeout = null
//           func.apply(context, args)
//         }, wait)
//       }
//     }
//   }
// }

/**
 * CRUD配置
 * @author 修改：duhh
 * @param {*} options <br>
 * @return crud instance.
 * @example
 */
function CRUD(options) {
  const queryDebounceTime = 350 // 查询防抖时间
  const defaultOptions = {
    // 标题
    title: '',
    // 请求数据的url
    url: '',
    // 表单ref
    formName: 'form',
    // table,emptyText
    emptyText: '等待加载',
    // 表格数据
    data: [],
    // 额外数据,可用于crud组件之间的数据传输
    extra: {},
    // 选择项
    selections: [],
    // 待查询的对象
    query: {},
    // 必选参数
    requiredQuery: [],
    // 查询数据的参数
    params: {},
    // Form 表单
    form: {},
    // 重置表单
    defaultForm: () => {},
    // 默认隐藏列
    invisibleColumns: ['createTime', 'updateTime'],
    requiredSubmitField: [],
    // 提交回调结果
    submitResult: null,
    // 排序规则，默认 id 降序， 支持多字段排序 ['id.desc', 'createTime.asc']
    sort: ['id.desc'],
    // 等待时间
    time: 50,
    firstQueryTime: null, // 首次查询时间
    // CRUD Method
    crudMethod: {
      add: form => {},
      delete: id => {},
      edit: form => {},
      get: id => {}
    },
    // 主页操作栏显示哪些按钮
    optShow: {
      add: true,
      edit: false,
      del: true,
      download: false
    },
    // 自定义一些扩展属性
    props: {},
    // 是否分页
    hasPagination: true,
    // 权限
    permission: {},
    // 在主页准备
    queryOnPresenterCreated: true,
    // 调试开关
    debug: false
  }
  options = mergeOptions(defaultOptions, options)
  const data = {
    ...options,
    initOptions: options,
    initSort: options.sort, // 避免默认sort变更
    // 记录数据状态
    dataStatus: {},
    // status: {
    //   add: CRUD.STATUS.NORMAL,
    //   edit: CRUD.STATUS.NORMAL,
    //   // 添加或编辑状态
    //   get cu() {
    //     if (this.add === CRUD.STATUS.NORMAL && this.edit === CRUD.STATUS.NORMAL) {
    //       return CRUD.STATUS.NORMAL
    //     } else if (this.add === CRUD.STATUS.PREPARED || this.edit === CRUD.STATUS.PREPARED) {
    //       return CRUD.STATUS.PREPARED
    //     } else if (this.add === CRUD.STATUS.PROCESSING || this.edit === CRUD.STATUS.PROCESSING) {
    //       return CRUD.STATUS.PROCESSING
    //     }
    //     throw new Error('wrong crud\'s cu status')
    //   },
    //   // 标题
    //   get title() {
    //     return this.add > CRUD.STATUS.NORMAL ? `新增${crud.title}` : this.edit > CRUD.STATUS.NORMAL ? `编辑${crud.title}` : crud.title
    //   }
    // },
    msg: {
      submit: '提交成功',
      add: '新增成功',
      edit: '编辑成功',
      del: '删除成功'
    },
    page: {
      // 页码
      page: 1,
      // 每页数据条数
      size: Vue.prototype.$TBS.PAGE_SIZE,
      // 总数据条数
      total: 0,
      hasNextPage: true
    },
    firstLoaded: false,
    // 整体loading
    loading: false,
    // 导出的 Loading
    downloadLoading: false,
    // 删除的 Loading
    delAllLoading: false
  }
  const methods = {
    /**
     * 通用的提示
     */
    submitSuccessNotify() {
      crud.notify(crud.msg.submit, CRUD.NOTIFICATION_TYPE.SUCCESS)
    },
    addSuccessNotify() {
      crud.notify(crud.msg.add, CRUD.NOTIFICATION_TYPE.SUCCESS)
    },
    editSuccessNotify() {
      crud.notify(crud.msg.edit, CRUD.NOTIFICATION_TYPE.SUCCESS)
    },
    delSuccessNotify() {
      crud.notify(crud.msg.del, CRUD.NOTIFICATION_TYPE.SUCCESS)
    },
    // 搜索
    async toQuery() {
      // TODO:可能会有bug，待查，
      // TODO:若不等待加载完vm完毕后再查询，钩子可能会无法触发(例：首次加载通过watch,immediate:true触发),因此在下方加入settimeout，还需优化
      const vmSet = new Set()
      crud.vms.forEach(vm => vm && vmSet.add(vm.vm))
      if (!vmSet.size) {
        setTimeout(() => {
          crud.toQuery()
        }, queryDebounceTime)
        return
      }
      if (!crud.verifyQuery()) {
        crud.data = []
        crud.page.total = 0
        crud.emptyText = '暂无数据'
        return
      }
      const now = Date.now()
      // TODO: 存在问题，待优化，当queryTime时间呗，第二次进入的参数传的不一样时，会被拦截的问题
      const flag = (crud.firstQueryTime && now - crud.firstQueryTime > queryDebounceTime) || (!crud.firstQueryTime && crud.firstQueryTime !== 0)
      if (flag) {
        if (!crud.firstQueryTime) {
          crud._toQuery()
          crud.firstQueryTime = now
        } else {
          crud._toQueryByDebounce()
        }
      }
    },
    _toQueryByDebounce: debounce(
      async () => {
        crud._toQuery()
      },
      queryDebounceTime,
      false
    ),
    async _toQuery() {
      if (!(await callVmHook(crud, CRUD.HOOK.beforeToQuery))) {
        return
      }
      crud.page.page = 1
      crud.page.hasNextPage = true
      crud.firstLoaded = false
      crud.refresh()
      if (!(await callVmHook(crud, CRUD.HOOK.afterToQuery))) {
        return
      }
    },
    verifyQuery() {
      const result = crud.requiredQuery.some(v => crud.query[v] === null || crud.query[v] === undefined)
      return !result
    },
    verifySubmit() {
      const result = crud.requiredSubmitField.some(v => crud.form[v] === null || crud.form[v] === undefined)
      return !result
    },
    // 刷新
    async refresh() {
      if (!checkPermission(crud.permission.get)) {
        return
      }
      crud.emptyText = '加载中'
      if (!(await callVmHook(crud, CRUD.HOOK.beforeRefresh))) {
        crud.data = []
        crud.page.total = 0
        crud.emptyText = '重新加载'
        return
      }
      if (!crud.verifyQuery()) {
        crud.data = []
        crud.page.total = 0
        crud.emptyText = '重新加载'
        return
      }
      return new Promise((resolve, reject) => {
        crud.loading = true
        crud.crudMethod
          .get(crud.getQueryParams())
          .then(async data => {
            crud.emptyText = '暂无数据'
            data.content = data.content || []
            await callVmHook(crud, CRUD.HOOK.handleRefresh, data)
            crud.page.total = data.totalElements
            crud.page.hasNextPage = data.hasNextPage
            crud.data = data.content || []
            crud.resetDataStatus()
            crud.loading = false

            // time 毫秒后显示表格
            resolve(data)
          })
          .catch(err => {
            console.log('err: ', err, err.message)
            crud.page.total = 0
            crud.page.hasNextPage = false
            crud.data = []
            // err?.message?.cancel === true 是接口取消了，重新请求本接口
            if (!err?.message?.cancel) {
              crud.emptyText = '加载失败'
              crud.loading = false
            }
            crud.resetDataStatus()
            reject(err)
          })
          .finally(() => {
            setTimeout(async () => {
              crud.firstLoaded = true
              await callVmHook(crud, CRUD.HOOK.afterRefresh, data)
            }, crud.time)
          })
      })
    },
    /**
     * 启动添加
     * // TODO: 考虑CU在ADD或EDIT的前或后
     */
    async toAdd() {
      if (!((await callVmHook(crud, CRUD.HOOK.beforeToAdd, crud.form)) && (await callVmHook(crud, CRUD.HOOK.beforeToCU, crud.form)))) {
        return
      }
      crud.status.add = CRUD.STATUS.PREPARED
      crud.submitResult = null
      await callVmHook(crud, CRUD.HOOK.afterToAdd, crud.form)
      await callVmHook(crud, CRUD.HOOK.afterToCU, crud.form)
    },
    /**
     * 启动编辑
     * @param {*} data 数据项
     */
    async toEdit(data) {
      crud.resetForm(JSON.parse(JSON.stringify(data)))
      if (!((await callVmHook(crud, CRUD.HOOK.beforeToEdit, crud.form)) && (await callVmHook(crud, CRUD.HOOK.beforeToCU, crud.form)))) {
        return
      }
      crud.status.edit = CRUD.STATUS.PREPARED
      crud.submitResult = null
      crud.getDataStatus(data.id).edit = CRUD.STATUS.PREPARED
      await callVmHook(crud, CRUD.HOOK.afterToEdit, crud.form)
      await callVmHook(crud, CRUD.HOOK.afterToCU, crud.form)
    },
    /**
     * 启动删除
     * @param {*} data 数据项
     */
    toDelete(data) {
      crud.getDataStatus(data.id).delete = CRUD.STATUS.PREPARED
    },
    /**
     * 取消删除
     * @param {*} data 数据项
     */
    async cancelDelete(data) {
      if (!(await callVmHook(crud, CRUD.HOOK.beforeDeleteCancel, data))) {
        return
      }
      crud.getDataStatus(data.id).delete = CRUD.STATUS.NORMAL
      await callVmHook(crud, CRUD.HOOK.afterDeleteCancel, data)
    },
    /**
     * 取消新增/编辑
     */
    async cancelCU() {
      const addStatus = crud.status.add
      const editStatus = crud.status.edit
      if (addStatus === CRUD.STATUS.PREPARED) {
        if (!(await callVmHook(crud, CRUD.HOOK.beforeAddCancel, crud.form))) {
          return
        }
        crud.status.add = CRUD.STATUS.NORMAL
      }
      if (editStatus === CRUD.STATUS.PREPARED) {
        if (!(await callVmHook(crud, CRUD.HOOK.beforeEditCancel, crud.form))) {
          return
        }
        crud.status.edit = CRUD.STATUS.NORMAL
        crud.getDataStatus(crud.form.id).edit = CRUD.STATUS.NORMAL
      }
      crud.resetForm()
      if (addStatus === CRUD.STATUS.PREPARED) {
        await callVmHook(crud, CRUD.HOOK.afterAddCancel, crud.form)
      }
      if (editStatus === CRUD.STATUS.PREPARED) {
        await callVmHook(crud, CRUD.HOOK.afterEditCancel, crud.form)
      }
      // 清除表单验证
      if (crud.findVM(crud.formName).$refs[crud.formName]) {
        crud.findVM(crud.formName).$refs[crud.formName].clearValidate()
      }
    },
    /**
     * 提交新增/编辑
     */
    async submitCU() {
      if (!crud.verifySubmit()) {
        return
      }
      if (!(await callVmHook(crud, CRUD.HOOK.beforeValidateCU))) {
        return
      }
      crud.findVM('form').$refs[crud.formName].validate(async valid => {
        if (!valid) {
          return
        }
        if (!(await callVmHook(crud, CRUD.HOOK.afterValidateCU))) {
          return
        }
        if (crud.status.add === CRUD.STATUS.PREPARED) {
          crud.doAdd()
        } else if (crud.status.edit === CRUD.STATUS.PREPARED) {
          crud.doEdit()
        }
      })
    },
    /**
     * 执行添加
     */
    async doAdd() {
      if (!(await callVmHook(crud, CRUD.HOOK.beforeSubmit))) {
        return
      }
      try {
        crud.submitResult = await crud.crudMethod.add(crud.form)
        crud.status.add = CRUD.STATUS.NORMAL
        crud.resetForm()
        crud.addSuccessNotify()
        await callVmHook(crud, CRUD.HOOK.afterSubmit)
        crud.toQuery()
      } catch (error) {
        console.log('添加', error)
        await callVmHook(crud, CRUD.HOOK.afterAddError)
      }
    },
    /**
     * 执行编辑
     */
    async doEdit() {
      if (!(await callVmHook(crud, CRUD.HOOK.beforeSubmit))) {
        return
      }
      try {
        crud.submitResult = await crud.crudMethod.edit(crud.form)
        crud.status.edit = CRUD.STATUS.NORMAL
        crud.getDataStatus(crud.form.id).edit = CRUD.STATUS.NORMAL
        crud.editSuccessNotify()
        crud.resetForm()
        await callVmHook(crud, CRUD.HOOK.afterSubmit)
        crud.refresh()
      } catch (error) {
        console.log('编辑', error)
        await callVmHook(crud, CRUD.HOOK.afterEditError)
      }
    },
    /**
     * 执行删除
     * @param {*} data 数据项
     */
    async doDelete(data) {
      let delAll = false
      let dataStatus
      const ids = []
      if (data instanceof Array) {
        delAll = true
        data.forEach(val => {
          ids.push(val.id)
        })
      } else {
        ids.push(data.id)
        dataStatus = crud.getDataStatus(data.id)
      }
      if (!(await callVmHook(crud, CRUD.HOOK.beforeDelete, data))) {
        return
      }
      if (!delAll) {
        dataStatus.delete = CRUD.STATUS.PROCESSING
      }
      return crud.crudMethod
        .del(ids)
        .then(async () => {
          if (delAll) {
            crud.delAllLoading = false
          } else dataStatus.delete = CRUD.STATUS.PREPARED
          crud.dleChangePage(1)
          crud.delSuccessNotify()
          await callVmHook(crud, CRUD.HOOK.afterDelete, data)
          crud.refresh()
        })
        .catch(() => {
          if (delAll) {
            crud.delAllLoading = false
          } else dataStatus.delete = CRUD.STATUS.PREPARED
        })
    },
    /**
     * 通用导出
     */
    async doExport(data) {
      try {
        crud.downloadLoading = true
        // await crud.crudMethod.download(data.id)
        await fileDownload(crud.crudMethod.download, data)
      } catch (error) {
        console.log(error)
      } finally {
        crud.downloadLoading = false
      }
      // download(crud.url + '/download', crud.getQueryParams()).then(result => {
      //   downloadFile(result, crud.title + '数据', 'xlsx')
      //   crud.downloadLoading = false
      // }).catch(() => {
      //   crud.downloadLoading = false
      // })
    },
    /**
     * 获取查询参数
     */
    getQueryParams: function () {
      return {
        page: crud.hasPagination ? crud.page.page : undefined,
        size: crud.hasPagination ? crud.page.size : undefined,
        sort: crud.sort,
        ...crud.query,
        ...crud.params
      }
    },
    // 当前页改变
    async pageChangeHandler(e) {
      crud.page.page = e
      await crud.refresh()
    },
    // 每页条数改变
    async sizeChangeHandler(e) {
      crud.page.size = e
      crud.page.page = 1
      await crud.refresh()
    },
    // 预防删除第二页最后一条数据时，或者多选删除第二页的数据时，页码错误导致请求无数据
    dleChangePage(size) {
      if (crud.data.length === size && crud.page.page !== 1) {
        crud.page.page -= 1
      }
    },
    // 选择改变
    selectionChangeHandler(val) {
      // console.log(val)
      crud.selections = val
    },
    /**
     * 重置查询参数
     * @param {Boolean} toQuery 重置后进行查询操作
     */
    async resetQuery(toQuery = true) {
      if (!(await callVmHook(crud, CRUD.HOOK.beforeResetQuery, data))) {
        return
      }
      const defaultQuery = JSON.parse(JSON.stringify(crud.defaultQuery))
      const query = crud.query
      Object.keys(query).forEach(key => {
        if (defaultQuery[key]) {
          if (defaultQuery[key].resetAble) {
            query[key] = defaultQuery[key].value
          }
        } else {
          query[key] = undefined
        }
      })
      if (toQuery) {
        crud.toQuery()
      }
    },
    /**
     * 重置表单
     * @param {Array} data 数据
     */
    resetForm(data) {
      // 清除表单信息
      if (crud.findVM('form').$refs[crud.formName]) {
        // 设置默认值，因此重置放在顶部
        crud.findVM('form').$refs[crud.formName].resetFields()
      }
      const form = data || (typeof crud.defaultForm === 'object' ? JSON.parse(JSON.stringify(crud.defaultForm)) : crud.defaultForm())
      const crudFrom = crud.form
      for (const key in crudFrom) {
        crudFrom[key] = undefined
      }
      for (const key in form) {
        if (Object.prototype.hasOwnProperty.call(crudFrom, key)) {
          crudFrom[key] = form[key]
        } else {
          Vue.set(crudFrom, key, form[key])
        }
      }
    },
    /**
     * 重置数据状态
     */
    resetDataStatus() {
      const dataStatus = {}
      function resetStatus(datas = []) {
        datas.forEach(e => {
          dataStatus[e.id] = {
            delete: 0,
            edit: 0
          }
          if (e.children) {
            resetStatus(e.children)
          }
        })
      }
      resetStatus(crud.data)
      crud.dataStatus = dataStatus || []
    },
    /**
     * 获取数据状态
     * @param {Number | String} id 数据项id
     */
    getDataStatus(id) {
      return crud.dataStatus[id]
    },
    /**
     * 用于树形表格多选, 选中所有
     * @param selection
     */
    selectAllChange(selection) {
      // 如果选中的数目与请求到的数目相同就选中子节点，否则就清空选中
      if (selection && selection.length === crud.data.length) {
        selection.forEach(val => {
          crud.selectChange(selection, val)
        })
      } else {
        crud.findVM('presenter').$refs['table'].clearSelection()
      }
    },
    /**
     * 用于树形表格多选，单选的封装
     * @param selection
     * @param row
     */
    selectChange(selection, row) {
      // 如果selection中存在row代表是选中，否则是取消选中
      if (
        selection.find(val => {
          return val.id === row.id
        })
      ) {
        if (row.children) {
          row.children.forEach(val => {
            crud.findVM('presenter').$refs['table'].toggleRowSelection(val, true)
            selection.push(val)
            if (val.children) {
              crud.selectChange(selection, val)
            }
          })
        }
      } else {
        crud.toggleRowSelection(selection, row)
      }
    },
    /**
     * 切换选中状态
     * @param selection
     * @param data
     */
    toggleRowSelection(selection, data) {
      if (data.children) {
        data.children.forEach(val => {
          crud.findVM('presenter').$refs['table'].toggleRowSelection(val, false)
          for (const i in selection) {
            if (selection[i].id === val.id) {
              selection.splice(i, 1)
              break
            }
          }
          if (val.children) {
            crud.toggleRowSelection(selection, val)
          }
        })
      }
    },
    /**
     * 处理排序
     */
    handleSortChange({ column, prop, order }) {
      let sort = []
      if (order) {
        const shortOrder = order === 'ascending' ? 'asc' : 'desc'
        sort = [`${prop}.${shortOrder}`]
      } else {
        sort = crud.initSort
      }
      crud.sort = sort
      crud.toQuery()
    },
    findVM(type) {
      return crud.vms.find(vm => vm && vm.type === type).vm
    },
    notify(title, type = CRUD.NOTIFICATION_TYPE.INFO) {
      crud.vms[0].vm.$notify({
        title,
        type,
        duration: 2500
      })
    },
    updateProp(name, value) {
      Vue.set(crud.props, name, value)
    }
  }
  const _data = Object.assign(deepClone(data), {
    status: {
      add: CRUD.STATUS.NORMAL,
      edit: CRUD.STATUS.NORMAL,
      // 添加或编辑状态
      get cu() {
        if (this.add === CRUD.STATUS.NORMAL && this.edit === CRUD.STATUS.NORMAL) {
          return CRUD.STATUS.NORMAL
        } else if (this.add === CRUD.STATUS.PREPARED || this.edit === CRUD.STATUS.PREPARED) {
          return CRUD.STATUS.PREPARED
        } else if (this.add === CRUD.STATUS.PROCESSING || this.edit === CRUD.STATUS.PROCESSING) {
          return CRUD.STATUS.PROCESSING
        }
        throw new Error("wrong crud's cu status")
      },
      // 标题
      get title() {
        return this.add > CRUD.STATUS.NORMAL ? `新增${crud.title}` : this.edit > CRUD.STATUS.NORMAL ? `编辑${crud.title}` : crud.title
      }
    }
  })
  const crud = Object.assign({}, _data)
  // const crud = JSON.parse(JSON.stringify(data))
  // 可观测化
  Vue.observable(crud)
  // 附加方法
  Object.assign(crud, methods)
  // 记录初始默认的查询参数，后续重置查询时使用
  Object.assign(crud, {
    defaultQuery: JSON.parse(JSON.stringify(data.query)),
    // 预留4位存储：组件 主页、头部、分页、表单，调试查看也方便找
    vms: Array(4),
    /**
     * 注册组件实例
     * @param {String} type 类型
     * @param {*} vm 组件实例
     * @param {Number} index 该参数内部使用
     */
    registerVM(type, vm, index = -1) {
      const vmObj = {
        type,
        vm: vm
      }
      if (index < 0) {
        this.vms.push(vmObj)
        return
      }
      this.vms.length = Math.max(this.vms.length, index)
      this.vms.splice(index, 1, vmObj)
    },
    /**
     * 取消注册组件实例
     * @param {*} vm 组件实例
     */
    unregisterVM(vm) {
      this.vms.splice(
        this.vms.findIndex(e => e && e.vm === vm),
        1
      )
    },
    init() {
      Object.assign(crud, deepClone(data))
      crud.status.add = CRUD.STATUS.NORMAL
      crud.status.edit = CRUD.STATUS.NORMAL
    }
  })
  // 冻结处理，需要扩展数据的话，使用crud.updateProp(name, value)，以crud.props.name形式访问，这个是响应式的，可以做数据绑定
  Object.freeze(crud)
  return crud
}

// hook VM
async function callVmHook(crud, hook) {
  if (crud.debug) {
    console.log('callVmHook: ' + hook)
  }
  let ret = true
  const nargs = [crud]
  for (let i = 2; i < arguments.length; ++i) {
    nargs.push(arguments[i])
  }
  // 有些组件扮演了多个角色，调用钩子时，需要去重
  const vmSet = new Set()
  crud.vms.forEach(vm => vm && vmSet.add(vm.vm))
  for (const vm of vmSet) {
    // vmSet.forEach(async(vm) => {
    if (vm[hook]) {
      ret = (await vm[hook].apply(vm, nargs)) !== false && ret
    }
  }
  return ret
}

function mergeOptions(src, opts) {
  const optsRet = {
    ...src
  }
  for (const key in src) {
    if (Object.prototype.hasOwnProperty.call(opts, key)) {
      optsRet[key] = opts[key]
    }
  }
  return optsRet
}

/**
 * crud主页
 */
function presenter(crud) {
  function obColumns(columns) {
    return {
      visible(col) {
        return !columns || !columns[col] ? true : columns[col].visible
      }
    }
  }
  return {
    inject: ['crud'],
    beforeCreate() {
      // 由于initInjections在initProvide之前执行，如果该组件自己就需要crud，需要在initInjections前准备好crud
      this._provided = {
        ...this._provided,
        crud,
        permission: crud.permission,
        'crud.query': crud.query,
        'crud.page': crud.page,
        'crud.form': crud.form
      }
    },
    data() {
      return {
        searchToggle: true,
        columns: obColumns()
      }
    },
    methods: {
      parseTime
    },
    created() {
      this.crud.registerVM('presenter', this, 0)
      // 重置
      // queryPrevious = 0
    },
    beforeDestroy() {
      this.crud.unregisterVM(this)
    },
    destroyed() {
      this.crud.init()
    },
    mounted() {
      if (crud.queryOnPresenterCreated) {
        // TODO:toQuery本来是放在created中查询，因钩子写在组件中，此时触发无法触发钩子的函数，故移入mounted，等待created完成再执行(错误)
        crud.toQuery()
      }
      const columns = {}
      if (this.$refs.table) {
        this.$refs.table.columns.forEach(e => {
          if (!e.property || e.type !== 'default') {
            return
          }
          columns[e.property] = {
            label: e.label,
            visible: crud.invisibleColumns.indexOf(e.property) === -1 // 默认隐藏
          }
        })
        this.columns = obColumns(columns)
        this.crud.updateProp('tableColumns', columns)
      }
    }
  }
}

// function judgeVmHasLoad(){
//   const vmSet = new Set()
//   crud.vms.forEach(vm => vm && vmSet.add(vm.vm))
// }

/**
 * crud的table组件
 */
function table(crud) {
  function obColumns(columns) {
    return {
      visible(col) {
        return !columns || !columns[col] ? true : columns[col].visible
      }
    }
  }
  return {
    inject: ['crud'],
    data() {
      return {
        columns: obColumns()
      }
    },
    mounted() {
      const columns = {}
      if (this.$refs.table) {
        this.$refs.table.columns.forEach(e => {
          if (!e.property || e.type !== 'default') {
            return
          }
          columns[e.property] = {
            label: e.label,
            visible: this.crud.invisibleColumns.indexOf(e.property) === -1 // 默认隐藏
          }
        })
        this.columns = obColumns(columns)
        this.crud.updateProp('tableColumns', columns)
      }
    }
  }
}

/**
 * 头部
 */
function header(defaultQuery = {}) {
  return {
    inject: {
      crud: {
        from: 'crud'
      },
      query: {
        from: 'crud.query'
      }
    },
    created() {
      this.crud.registerVM('header', this, 1)
      for (const key in defaultQuery) {
        if (typeof defaultQuery[key] !== 'object') {
          defaultQuery[key] = {
            value: defaultQuery[key], // 默认值
            resetAble: true // 是否可重置
          }
        }
        // TODO: defaultQuery.date是数组
        if (defaultQuery[key] instanceof Array) {
          this.$set(this.crud.query, key, defaultQuery[key])
        } else {
          this.$set(this.crud.query, key, defaultQuery[key].value)
        }
      }
      Object.assign(this.crud.defaultQuery, JSON.parse(JSON.stringify(defaultQuery)))
    },
    mounted() {
      if (crud.queryOnPresenterCreated) {
        // TODO:toQuery本来是放在created中查询，因钩子写在组件中，此时触发无法触发钩子的函数，故移入mounted，等待created完成再执行(错误)
        crud.toQuery()
      }
    },
    beforeDestroy() {
      this.crud.unregisterVM(this)
    }
  }
}

/**
 * 分页
 */
function pagination() {
  return {
    inject: {
      crud: {
        from: 'crud'
      },
      page: {
        from: 'crud.page'
      }
    },
    created() {
      this.crud.registerVM('pagination', this, 2)
    },
    beforeDestroy() {
      this.crud.unregisterVM(this)
    }
  }
}

/**
 * 表单
 */
function form(defaultForm) {
  return {
    inject: {
      crud: {
        from: 'crud'
      },
      form: {
        from: 'crud.form'
      }
    },
    created() {
      this.crud.registerVM('form', this, 3)
      this.crud.defaultForm = defaultForm
      this.crud.resetForm()
    },
    beforeDestroy() {
      this.crud.unregisterVM(this)
    },
    methods: {
      validateField(type) {
        this.$refs[this.crud.formName].validateField(type)
      }
    }
  }
}

/**
 * crud
 */
function crud(options = {}) {
  const defaultOptions = {
    type: undefined
  }
  options = mergeOptions(defaultOptions, options)
  return {
    inject: {
      crud: {
        from: 'crud'
      }
    },
    created() {
      this.crud.registerVM(options.type, this)
    },
    beforeDestroy() {
      this.crud.unregisterVM(this)
    }
  }
}

/**
 * CRUD钩子
 */
CRUD.HOOK = {
  /** 重新查询 - 之前 */
  beforeToQuery: 'beforeCrudToQuery',
  /** 重新查询 - 之后 */
  afterToQuery: 'afterCrudToQuery',
  /** 刷新 - 之前 */
  beforeRefresh: 'beforeCrudRefresh',
  /** 刷新 - 之后 */
  afterRefresh: 'afterCrudRefresh',
  /** 删除 - 之前 */
  beforeDelete: 'beforeCrudDelete',
  /** 删除 - 之后 */
  afterDelete: 'afterCrudDelete',
  /** 删除取消 - 之前 */
  beforeDeleteCancel: 'beforeCrudDeleteCancel',
  /** 删除取消 - 之后 */
  afterDeleteCancel: 'afterCrudDeleteCancel',
  /** 新建 - 之前 */
  beforeToAdd: 'beforeCrudToAdd',
  /** 新建 - 之后 */
  afterToAdd: 'afterCrudToAdd',
  /** 编辑 - 之前 */
  beforeToEdit: 'beforeCrudToEdit',
  /** 编辑 - 之后 */
  afterToEdit: 'afterCrudToEdit',
  /** 开始 "新建/编辑" - 之前 */
  beforeToCU: 'beforeCrudToCU',
  /** 开始 "新建/编辑" - 之后 */
  afterToCU: 'afterCrudToCU',
  /** "新建/编辑" 验证 - 之前 */
  beforeValidateCU: 'beforeCrudValidateCU',
  /** "新建/编辑" 验证 - 之后 */
  afterValidateCU: 'afterCrudValidateCU',
  /** 添加取消 - 之前 */
  beforeAddCancel: 'beforeCrudAddCancel',
  /** 添加取消 - 之后 */
  afterAddCancel: 'afterCrudAddCancel',
  /** 编辑取消 - 之前 */
  beforeEditCancel: 'beforeCrudEditCancel',
  /** 编辑取消 - 之后 */
  afterEditCancel: 'afterCrudEditCancel',
  /** 提交 - 之前 */
  beforeSubmit: 'beforeCrudSubmitCU',
  /** 提交 - 之后 */
  afterSubmit: 'afterCrudSubmitCU',
  afterAddError: 'afterCrudAddError',
  afterEditError: 'afterCrudEditError',
  /** 重置搜索条件 - 之前 */
  beforeResetQuery: 'afterCrudResetQuery',
  /** TODO:未定义 重置搜索条件 - 之后 */
  afterResetQuery: 'afterCrudResetQuery',
  /** 处理刷新数据 */
  handleRefresh: 'handleRefreshData'
}

/**
 * CRUD状态
 */
CRUD.STATUS = {
  NORMAL: 0,
  PREPARED: 1,
  PROCESSING: 2
}

/**
 * CRUD通知类型
 */
CRUD.NOTIFICATION_TYPE = {
  SUCCESS: 'success',
  WARNING: 'warning',
  INFO: 'info',
  ERROR: 'error'
}

export default CRUD

export { presenter, header, table, form, pagination, crud }
