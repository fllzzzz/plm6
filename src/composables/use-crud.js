import { mapGetters } from '@/store/lib'
import { deepClone } from '@data-type/index'
import { debounce } from '@/utils'
import { fileDownload } from '@/utils/file'

import useCheckPermission from './use-check-permission'
import { ElNotification } from 'element-plus'

const CRUD = {} // crud公共信息处理

/**
 *
 */
export default function useCrud(options) {
  // 获取crud实例
  const crud = getCrud(options)
  Object.freeze(crud)
  return { crud }
}

// TODO:组件注册方法

/**
 * 获取主要数据
 * @param {object} options
 */
function getCrud(options) {
  const defaultOption = getDefaultOption()
  // 合并选项
  const hOptions = mergeOptions(defaultOption, options)
  // 添加crud内部选项
  const data = addSystemOptions(hOptions)
  // 拷贝data
  const _data = cloneData(data)
  // 以上为基础数据
  const crud = Object.assign({}, _data)
  // 添加crud默认信息
  addCrudDefaultInfo(crud, data)
  // 添加crud主要业务使用的方法
  addCrudBusinessMethod(crud)
  // 添加crud功能性方法
  addCrudFeatureMethod(crud, data)
  // 添加crud方法
  addCrudMethod(crud, data)

  return crud
}

// 拷贝data
// TODO: 此处status待查
function cloneData(data) {
  return Object.assign(deepClone(data), {
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
        throw new Error('错误的状态')
      },
      // 标题
      get title() {
        return this.add > CRUD.STATUS.NORMAL ? `新增${data.title}` : this.edit > CRUD.STATUS.NORMAL ? `编辑${data.title}` : data.title
      }
    }
  })
}

/**
 * 获取默认选项
 * @returns
 */
function getDefaultOption() {
  return {
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
    // extra: {},
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
    // 提交时必填字段
    requiredSubmitField: [],
    // 提交回调结果
    submitResult: null,
    // 排序规则，默认 id 降序， 支持多字段排序 ['id.desc', 'createTime.asc']
    sort: ['id.desc'],
    // 等待时间
    time: 50,
    // 首次查询时间
    firstQueryTime: null,
    // CRUD Method
    crudMethod: {
      add: (form) => {},
      delete: (id) => {},
      edit: (form) => {},
      get: (id) => {}
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
}

/**
 * 添加crud内部选项
 * @param {object} options
 * @returns
 */
function addSystemOptions(options) {
  // 获取用户分页默认数量
  const { tablePageSize } = mapGetters('tablePageSize')
  const data = {
    ...options,
    initOptions: options,
    dataStatus: {}, // 记录数据状态
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
      size: tablePageSize,
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
  return data
}

// 添加默认信息
function addCrudDefaultInfo(crud, data) {
  Object.assign(crud, {
    // 避免默认sort变更
    defaultSort: data.options.sort,
    // 记录初始默认的查询参数，后续重置查询时使用
    defaultQuery: JSON.parse(JSON.stringify(data.query)),
    // 预留4位存储：组件 主页、头部、分页、表单，调试查看也方便找
    vms: Array(4)
  })
}

// 添加crud主要业务使用的方法
function addCrudBusinessMethod(crud) {
  // 操作提示
  const submitSuccessNotify = () => {
    crud.notify(crud.msg.submit, CRUD.NOTIFICATION_TYPE.SUCCESS)
  }
  const addSuccessNotify = () => {
    crud.notify(crud.msg.add, CRUD.NOTIFICATION_TYPE.SUCCESS)
  }
  const editSuccessNotify = () => {
    crud.notify(crud.msg.edit, CRUD.NOTIFICATION_TYPE.SUCCESS)
  }
  const delSuccessNotify = () => {
    crud.notify(crud.msg.del, CRUD.NOTIFICATION_TYPE.SUCCESS)
  }

  // 校验
  const verifyQuery = () => {
    const result = crud.requiredQuery.some(v => crud.query[v] === null || crud.query[v] === undefined)
    return !result
  }
  // eslint-disable-next-line no-unused-vars
  const verifySubmit = () => {
    const result = crud.requiredSubmitField.some(v => crud.form[v] === null || crud.form[v] === undefined)
    return !result
  }

  // 搜索
  const toQuery = async () => {
    // TODO:【考虑删除】若不等待加载完vm完毕后再查询，钩子可能会无法触发(例：首次加载通过watch,immediate:true触发),因此在下方加入settimeout，还需优化
    const vmSet = new Set()
    crud.vms.forEach(vm => vm && vmSet.add(vm.vm))
    if (!vmSet.size) {
      setTimeout(() => {
        crud.toQuery()
      }, CRUD.QUERY_DEBOUNCE_TIME)
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
    const flag = (crud.firstQueryTime && (now - crud.firstQueryTime) > CRUD.QUERY_DEBOUNCE_TIME) || (!crud.firstQueryTime && crud.firstQueryTime !== 0)
    if (flag) {
      if (!crud.firstQueryTime) {
        _toQuery()
        crud.firstQueryTime = now
      } else {
        _toQueryByDebounce()
      }
    }
  }

  // 刷新
  const refresh = async () => {
    if (!useCheckPermission(crud.permission.get)) {
      return
    }
    crud.emptyText = '加载中'
    if (!await callVmHook(crud, CRUD.HOOK.beforeRefresh) && !verifyQuery()) {
      crud.data = []
      crud.page.total = 0
      crud.emptyText = '重新加载'
      return
    }

    let data = []
    try {
      crud.loading = true
      data = await crud.crudMethod.get(crud.getQueryParams())
      crud.emptyText = '暂无数据'
      data.content = data.content || []
      await callVmHook(crud, CRUD.HOOK.handleRefresh, data)
      crud.page.total = data.totalElements
      crud.page.hasNextPage = data.hasNextPage
      crud.data = data.content || []
      crud.resetDataStatus()
      crud.loading = false
    } catch (error) {
      crud.page.total = 0
      crud.page.hasNextPage = false
      crud.data = []
      // error?.message?.cancel === true 是接口取消了，重新请求本接口
      if (!error?.message?.cancel) {
        crud.emptyText = '加载失败'
        crud.loading = false
      }
      crud.resetDataStatus()
      console.log('crud-加载（刷新）数据', error)
    } finally {
      setTimeout(async () => {
        crud.firstLoaded = true
        await callVmHook(crud, CRUD.HOOK.afterRefresh, data)
      }, crud.time)
    }
  }

  // 启动添加
  const toAdd = async () => {
    if (!(await callVmHook(crud, CRUD.HOOK.beforeToAdd, crud.form) && await callVmHook(crud, CRUD.HOOK.beforeToCU, crud.form))) {
      return
    }
    crud.status.add = CRUD.STATUS.PREPARED
    crud.submitResult = null
    await callVmHook(crud, CRUD.HOOK.afterToAdd, crud.form)
    await callVmHook(crud, CRUD.HOOK.afterToCU, crud.form)
  }

  const toEdit = async (data) => {
    crud.resetForm(JSON.parse(JSON.stringify(data)))
    if (!(await callVmHook(crud, CRUD.HOOK.beforeToEdit, crud.form) && await callVmHook(crud, CRUD.HOOK.beforeToCU, crud.form))) {
      return
    }
    crud.status.edit = CRUD.STATUS.PREPARED
    crud.submitResult = null
    crud.getDataStatus(data.id).edit = CRUD.STATUS.PREPARED
    await callVmHook(crud, CRUD.HOOK.afterToEdit, crud.form)
    await callVmHook(crud, CRUD.HOOK.afterToCU, crud.form)
  }

  // 启动删除
  const toDelete = (data) => {
    crud.getDataStatus(data.id).delete = CRUD.STATUS.PREPARED
  }

  // 取消删除
  const cancelDelete = async (data) => {
    if (!await callVmHook(crud, CRUD.HOOK.beforeDeleteCancel, data)) {
      return
    }
    crud.getDataStatus(data.id).delete = CRUD.STATUS.NORMAL
    await callVmHook(crud, CRUD.HOOK.afterDeleteCancel, data)
  }

  /**
     * 取消新增/编辑
     */
  const cancelCU = async () => {
    const addStatus = crud.status.add
    const editStatus = crud.status.edit
    if (addStatus === CRUD.STATUS.PREPARED) {
      if (!await callVmHook(crud, CRUD.HOOK.beforeAddCancel, crud.form)) {
        return
      }
      crud.status.add = CRUD.STATUS.NORMAL
    }
    if (editStatus === CRUD.STATUS.PREPARED) {
      if (!await callVmHook(crud, CRUD.HOOK.beforeEditCancel, crud.form)) {
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
  }

  // 提交新增/编辑
  const submitCU = async () => {
    if (!crud.verifySubmit()) {
      return
    }
    if (!await callVmHook(crud, CRUD.HOOK.beforeValidateCU)) {
      return
    }
    crud.findVM('form').$refs[crud.formName].validate(async (valid) => {
      if (!valid) {
        return
      }
      if (!await callVmHook(crud, CRUD.HOOK.afterValidateCU)) {
        return
      }
      if (crud.status.add === CRUD.STATUS.PREPARED) {
        crud.doAdd()
      } else if (crud.status.edit === CRUD.STATUS.PREPARED) {
        crud.doEdit()
      }
    })
  }

  // 执行添加
  const doAdd = async () => {
    if (!await callVmHook(crud, CRUD.HOOK.beforeSubmit)) {
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
  }

  // 执行编辑
  const doEdit = async () => {
    if (!await callVmHook(crud, CRUD.HOOK.beforeSubmit)) {
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
  }

  // 执行删除
  const doDelete = async (data) => {
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
    if (!await callVmHook(crud, CRUD.HOOK.beforeDelete, data)) {
      return
    }
    if (!delAll) {
      dataStatus.delete = CRUD.STATUS.PROCESSING
    }
    // TODO:查看代码逻辑是否有问题
    return crud.crudMethod.del(ids).then(async () => {
      if (delAll) {
        crud.delAllLoading = false
      } else dataStatus.delete = CRUD.STATUS.PREPARED
      crud.dleChangePage(1)
      crud.delSuccessNotify()
      await callVmHook(crud, CRUD.HOOK.afterDelete, data)
      crud.refresh()
    }).catch(() => {
      if (delAll) {
        crud.delAllLoading = false
      } else dataStatus.delete = CRUD.STATUS.PREPARED
    })
  }

  const _toQueryByDebounce = debounce(async () => {
    crud._toQuery()
  }, CRUD.QUERY_DEBOUNCE_TIME, false)

  const _toQuery = async () => {
    if (!await callVmHook(crud, CRUD.HOOK.beforeToQuery)) {
      return
    }
    crud.page.page = 1
    crud.page.hasNextPage = true
    crud.firstLoaded = false
    crud.refresh()
    if (!await callVmHook(crud, CRUD.HOOK.afterToQuery)) {
      return
    }
  }

  Object.assign(crud, {
    submitSuccessNotify, // 表单提交成功通知
    addSuccessNotify, // 添加成功
    editSuccessNotify, // 编辑成功通知
    delSuccessNotify, // 删除成功通知
    toQuery, // 搜索
    refresh, // 刷新
    toAdd, // 启动添加
    toEdit, // 启动编辑
    toDelete, // 启动删除
    cancelDelete, // 取消删除
    cancelCU, // 取消新增/编辑
    submitCU, // 提交新增/编辑
    doAdd, // 执行添加
    doEdit, // 执行编辑
    doDelete // 执行删除
  })
}

// 添加crud功能性方法
function addCrudFeatureMethod(crud, data) {
  // 通用导出
  const doExport = async (data) => {
    try {
      crud.downloadLoading = true
      await fileDownload(crud.crudMethod.download, data)
    } catch (error) {
      console.log(error)
    } finally {
      crud.downloadLoading = false
    }
  }
  // 获取查询参数
  const getQueryParams = () => {
    return {
      page: crud.hasPagination ? crud.page.page : undefined,
      size: crud.hasPagination ? crud.page.size : undefined,
      sort: crud.sort,
      ...crud.query,
      ...crud.params
    }
  }
  // 当前页改变
  const pageChangeHandler = async (e) => {
    crud.page.page = e
    await crud.refresh()
  }
  // 每页条数改变
  const sizeChangeHandler = async (e) => {
    crud.page.size = e
    crud.page.page = 1
    await crud.refresh()
  }
  // 预防删除第二页最后一条数据时，或者多选删除第二页的数据时，页码错误导致请求无数据
  const dleChangePage = (size) => {
    if (crud.data.length === size && crud.page.page !== 1) {
      crud.page.page -= 1
    }
  }

  // 重置查询参数,重置后进行查询操作
  const resetQuery = async (toQuery = true) => {
    if (!await callVmHook(crud, CRUD.HOOK.beforeResetQuery, data)) {
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
  }
  /**
   * 重置表单
   * @param {Array} data 数据
   */
  const resetForm = (data) => {
    // 清除表单信息 TODO:待改待测
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
        // TODO: 待测
        // Vue.set(crudFrom, key, form[key])
      }
    }
  }
  // 重置数据状态
  const resetDataStatus = () => {
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
  }

  // 获取数据状态
  const getDataStatus = (id) => {
    return crud.dataStatus[id]
  }

  // 选择改变
  const selectionChangeHandler = (val) => {
    crud.selections = val
  }

  /**
     * 用于树形表格多选, 选中所有
     * @param selection
     */
  const selectAllChange = (selection) => {
    // 如果选中的数目与请求到的数目相同就选中子节点，否则就清空选中
    if (selection && selection.length === crud.data.length) {
      selection.forEach(val => {
        crud.selectChange(selection, val)
      })
    } else {
      crud.findVM('presenter').$refs['table'].clearSelection()
    }
  }
  /**
   * 用于树形表格多选，单选的封装
   * @param selection
   * @param row
   */
  const selectChange = (selection, row) => {
    // 如果selection中存在row代表是选中，否则是取消选中
    if (selection.find(val => { return val.id === row.id })) {
      if (row.children) {
        row.children.forEach(val => {
          // TODO: 待改待测
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
  }
  // 切换选中状态
  const toggleRowSelection = (selection, data) => {
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
  }
  //  处理排序方式改变
  const handleSortChange = ({ column, prop, order }) => {
    let sort = []
    if (order) {
      const shortOrder = order === 'ascending' ? 'asc' : 'desc'
      sort = [`${prop}.${shortOrder}`]
    } else {
      sort = crud.initSort
    }
    crud.sort = sort
    crud.toQuery()
  }

  // 通知
  const notify = (title, type = CRUD.NOTIFICATION_TYPE.INFO) => {
    ElNotification({
      title,
      type,
      duration: 2500
    })
  }

  Object.assign(crud, {
    doExport, // 通用导出
    getQueryParams, //  获取查询参数
    pageChangeHandler, // 当前页码改变
    sizeChangeHandler, // 每页条数改变
    dleChangePage, // 预防删除第二页最后一条数据时，或者多选删除第二页的数据时，页码错误导致请求无数据
    resetQuery, // 重置查询参数,重置后进行查询操作
    resetForm, // 重置表单
    resetDataStatus, // 重置数据状态
    getDataStatus, // 获取数据状态
    selectionChangeHandler, // 选择改变
    selectAllChange, // 用于树形表格多选, 选中所有
    selectChange, // 用于树形表格多选，单选的封装
    toggleRowSelection, // 切换选中状态
    handleSortChange, // 处理排序方式改变
    notify
  })
}

// 添加crud主要信息
function addCrudMethod(crud, data) {
  // 初始化crud
  const init = () => {
    Object.assign(crud, deepClone(data))
    crud.status.add = CRUD.STATUS.NORMAL
    crud.status.edit = CRUD.STATUS.NORMAL
  }

  // 注册组件
  const registerVM = (type, vm, index = -1) => {
    const vmInfo = {
      id: `${crud.vms.length}_${new Date().getTime()}_${Math.random() * 100000}`,
      type,
      vm,
      CRUD: { HOOK: {}} // 用于hook
    }
    // 默认新加入的vm放在数组最后面
    if (index < 0) {
      crud.vms.push(vmInfo)
      return
    }
    // 将vm插入指定位置
    crud.vms.length = Math.max(crud.vms.length, index)
    crud.vms.splice(index, 1, vmInfo)
  }

  // 注销组件
  const unregisterVM = (vm) => {
    const del = crud.vms.splice(crud.vms.findIndex(e => e && e.id === vm.id), 1)
    return del
  }

  // 查找组件
  const findVM = (val, field = 'type') => {
    return crud.vms.find(vm => vm && vm[field] === val).vm
  }

  Object.assign(crud, {
    init, // 初始化crud
    registerVM, // 注册组件
    unregisterVM, // 注销组件
    findVM // 查找组件
  })
}

// hook回调
async function callVmHook(crud, hook) {
  if (crud.debug) { // 可查看hook调用情况
    console.log('callVmHook: ' + hook)
  }
  let result = true // 回调结果
  const args = [crud]
  // 除去crud、hook。从第三个参数开始推入
  for (let i = 2; i < arguments.length; ++i) {
    args.push(arguments[i])
  }
  // 遍历各组件
  for (const VM of crud.vms) {
    if (VM.CRUD.HOOK[hook]) {
      result = (await VM.CRUD.HOOK[hook](args)) !== false && result
    }
  }
  return result
}

/**
 * 合并选项，只能覆盖默认的选项，不能新增
 * @param {object} source 源数据：默认选项
 * @param {object} rewrite 重写选项
 * @returns 合并结果
 */
function mergeOptions(source, rewrite) {
  const opts = { ...source }
  Object.keys(source).forEach(key => {
    if (Object.prototype.hasOwnProperty.call(rewrite, key)) {
      opts[key] = rewrite[key]
    }
  })
  return opts
}

CRUD.QUERY_DEBOUNCE_TIME = 350 // 查询防抖时间

// CRUD状态
CRUD.STATUS = {
  NORMAL: 0,
  PREPARED: 1,
  PROCESSING: 2
}

// CRUD通知类型
CRUD.NOTIFICATION_TYPE = {
  SUCCESS: 'success',
  WARNING: 'warning',
  INFO: 'info',
  ERROR: 'error'
}

// key 与 value 需要一致
CRUD.HOOK = {
  /** 重新查询 - 之前 */
  beforeToQuery: 'beforeToQuery',
  /** 重新查询 - 之后 */
  afterToQuery: 'afterToQuery',
  /** 刷新 - 之前 */
  beforeRefresh: 'beforeRefresh',
  /** 刷新 - 之后 */
  afterRefresh: 'afterRefresh',
  /** 删除 - 之前 */
  beforeDelete: 'beforeDelete',
  /** 删除 - 之后 */
  afterDelete: 'afterDelete',
  /** 删除取消 - 之前 */
  beforeDeleteCancel: 'beforeDeleteCancel',
  /** 删除取消 - 之后 */
  afterDeleteCancel: 'afterDeleteCancel',
  /** 新建 - 之前 */
  beforeToAdd: 'beforeToAdd',
  /** 新建 - 之后 */
  afterToAdd: 'afterToAdd',
  /** 编辑 - 之前 */
  beforeToEdit: 'beforeToEdit',
  /** 编辑 - 之后 */
  afterToEdit: 'afterToEdit',
  /** 开始 "新建/编辑" - 之前 */
  beforeToCU: 'beforeToCU',
  /** 开始 "新建/编辑" - 之后 */
  afterToCU: 'afterToCU',
  /** "新建/编辑" 验证 - 之前 */
  beforeValidateCU: 'beforeValidateCU',
  /** "新建/编辑" 验证 - 之后 */
  afterValidateCU: 'afterValidateCU',
  /** 添加取消 - 之前 */
  beforeAddCancel: 'beforeAddCancel',
  /** 添加取消 - 之后 */
  afterAddCancel: 'afterAddCancel',
  /** 编辑取消 - 之前 */
  beforeEditCancel: 'beforeEditCancel',
  /** 编辑取消 - 之后 */
  afterEditCancel: 'afterEditCancel',
  /** 提交 - 之前 */
  beforeSubmit: 'beforeSubmit',
  /** 提交 - 之后 */
  afterSubmit: 'afterSubmit',
  /** 添加失败 - 之后 */
  afterAddError: 'afterAddError',
  /** 编辑失败 - 之后 */
  afterEditError: 'afterEditError',
  /** 重置搜索条件 - 之前 */
  beforeResetQuery: 'beforeResetQuery',
  /** TODO:未定义 重置搜索条件 - 之后 */
  afterResetQuery: 'afterResetQuery',
  /** 处理刷新数据 */
  handleRefresh: 'handleRefresh'
}
