import { provide, inject, reactive, ref, watch, getCurrentInstance, onMounted, onBeforeUnmount, onUnmounted, nextTick } from 'vue'
import { mapGetters } from '@/store/lib'
import { isNotBlank, isBlank } from '@data-type/index'
import { debounce } from '@/utils'
import { fileDownload } from '@/utils/file'
import checkPermission from '@/utils/system/check-permission'
import * as lodash from 'lodash'

import useAddFormLocalStorage from '@/composables/form/use-crud-add-form-local-storage'
import useBatchAddFormLocalStorage from '@/composables/form/use-crud-add-batch-form-local-storage'

import { ElMessage, ElNotification } from 'element-plus'

const CRUD = {} // crud公共信息处理
// TODO: 关闭修改或添加窗口，由于状态信息的及时变更，会导致窗口未关闭，标题直接发生变化
/**
 * crud组件
 * @param {*} options 组件自定义选项
 * @param {*} tableRef 非必填 (由于执行顺序问题，无法直接从实例中获取该值，需要传入)
 * @param {*} registerPresenter= true 是否注册主组件，若不注册可之后调用presenter方法注册
 */
export default function useCRUD(options, tableRef, registerPresenter = true) {
  // 获取crud实例
  const crud = getCrud(options)
  // 不能添加新属性，也不能重新配置或者删除任何现有属性（但是可以修改属性的值）
  Object.seal(crud)
  if (registerPresenter) {
    const data = regPresenter(crud, tableRef)
    return data
  }
  return { crud }
}

/**
 * 主页面注册 主列表页的注册
 * @param {*} crud
 * @param {*} tableRef
 * @returns { columns, crud } = { 显示的列, crud }
 */
export function regPresenter(crud, tableRef) {
  const columns = ref({})
  provide('crud', crud)
  provide('permission', crud.permission)
  const internalInstance = getCurrentInstance()
  // 注册组件
  const vmInfo = crud.registerVM(CRUD.VM_TYPE.PRESENTER, internalInstance, 0)
  vmInfo.tableRef = tableRef
  crud.ref.table = tableRef

  // 卸载前，注销组件
  onBeforeUnmount(() => {
    crud.unregisterVM(internalInstance)
    delete crud.ref.table
  })

  // TODO:卸载后初始化crud
  onUnmounted(() => {
    crud.init()
  })

  onMounted(() => {
    if (crud.queryOnPresenterCreated) {
      // TODO:toQuery本来是放在created中查询，因钩子写在组件中，此时触发无法触发钩子的函数，故移入mounted，等待created完成再执行(错误)
      crud.toQuery()
    }
    if (tableRef) {
      watch(
        // TODO:正确情况不需要监听，当table在弹出框（dlg，drawer）中的时候需要监听。是否在监听一次后就取消，看后期业务。
        tableRef,
        () => {
          if (tableRef && tableRef.value) {
            crud.setColumns()
          }
        },
        { immediate: true }
      )
    }
  })
  columns.value = crud.obColumns(columns)
  crud.tableColumns = columns
  return { CRUD: vmInfo.CRUD, crud, columns }
}

/**
 * header组件
 * @param {object} defaultQuery 默认查询项
 * @returns
 */
export function regHeader(defaultQuery) {
  const crud = inject('crud')
  const internalInstance = getCurrentInstance()
  // 注册组件
  const vmInfo = crud.registerVM(CRUD.VM_TYPE.HEADER, internalInstance, 1)

  for (const key in defaultQuery) {
    if (typeof defaultQuery[key] !== 'object') {
      defaultQuery[key] = {
        value: defaultQuery[key], // 默认值
        resetAble: true // 是否可重置
      }
    }
    // TODO: defaultQuery.date是数组
    if (defaultQuery[key] instanceof Array) {
      crud.query[key] = defaultQuery[key]
    } else {
      crud.query[key] = defaultQuery[key].value
    }
  }
  Object.assign(crud.defaultQuery, JSON.parse(JSON.stringify(defaultQuery)))

  onMounted(() => {
    if (crud.queryOnPresenterCreated) {
      // TODO:toQuery本来是放在created中查询，因钩子写在组件中，此时触发无法触发钩子的函数，故移入mounted，等待created完成再执行(错误)
      crud.toQuery()
    }
  })

  // 卸载前，注销组件
  onBeforeUnmount(() => {
    crud.unregisterVM(internalInstance)
  })

  return { CRUD: vmInfo.CRUD, crud, query: crud.query }
}

/**
 * 注册表单组件
 * @param {object} defaultForm 默认表单
 * @param {object} formRef
 */
export function regForm(defaultForm = {}, formRef, customizeFormStoreKey) {
  const crud = inject('crud')
  const internalInstance = getCurrentInstance()
  // 注册组件
  const vmInfo = crud.registerVM(CRUD.VM_TYPE.FORM, internalInstance, 2)
  crud.defaultForm = defaultForm
  vmInfo.formRef = formRef
  crud.ref.form = formRef
  crud.resetForm()

  // 添加表单缓存
  let fmStore = {}
  if (crud.formStore) {
    const formStoreKey = isNotBlank(customizeFormStoreKey) ? customizeFormStoreKey : crud.formStoreKey
    const store = useAddFormLocalStorage(formStoreKey, {
      useDraftCallback: crud.useFormDraftCallback,
      clearDraftCallback: crud.clearFormDraftCallback
    })
    fmStore = store
  }

  onBeforeUnmount(() => {
    crud.unregisterVM(internalInstance)
    delete crud.ref.form
  })

  return { CRUD: vmInfo.CRUD, crud, form: crud.form, ADD_FORM: fmStore }
}

/**
 * 注册表单组件
 * @param {object} defaultForm 默认表单
 * @param {object} formRef
 */
export function regBatchForm(defaultForm, formRef) {
  const crud = inject('crud')
  const internalInstance = getCurrentInstance()
  // 注册组件
  const vmInfo = crud.registerVM(CRUD.VM_TYPE.BATCH_FORM, internalInstance, 3)
  crud.defaultBatchForm = defaultForm
  vmInfo.formRef = formRef
  crud.ref.batchForm = formRef
  crud.resetBatchForm()

  // 添加表单缓存
  let fmStore = {}
  if (crud.formStore) {
    const store = useBatchAddFormLocalStorage(crud.formStoreKey, {
      useDraftCallback: crud.useBatchFormDraftCallback,
      clearDraftCallback: crud.clearBatchFormDraftCallback
    })
    fmStore = store
  }

  onBeforeUnmount(() => {
    crud.unregisterVM(internalInstance)
    delete crud.ref.batchForm
  })

  return { CRUD: vmInfo.CRUD, crud, form: crud.batchForm, ADD_FORM: fmStore }
}

/**
 * 注册分页组件
 */
export function regDetail() {
  const crud = inject('crud')
  const internalInstance = getCurrentInstance()
  // 注册组件
  const vmInfo = crud.registerVM(CRUD.VM_TYPE.DETAIL, internalInstance, 4)
  crud.resetRowDetail()
  // 卸载前，注销组件
  onBeforeUnmount(() => {
    crud.unregisterVM(internalInstance)
  })
  return { CRUD: vmInfo.CRUD, crud, detail: crud.rowDetail }
}

/**
 * 注册分页组件
 */
export function regPagination() {
  const crud = inject('crud')
  const internalInstance = getCurrentInstance()
  // 注册组件
  const vmInfo = crud.registerVM(CRUD.VM_TYPE.PAGINATION, internalInstance, 5)
  // 卸载前，注销组件
  onBeforeUnmount(() => {
    crud.unregisterVM(internalInstance)
  })
  return { CRUD: vmInfo.CRUD, crud, page: crud.page }
}

/**
 * 注册其他组件
 * @param {object} options 选项
 */
export function regExtra(options = {}) {
  const crud = inject('crud')
  const internalInstance = getCurrentInstance()
  // 注册组件
  const vmInfo = crud.registerVM(CRUD.VM_TYPE.OTHER, internalInstance)

  onBeforeUnmount(() => {
    crud.unregisterVM(internalInstance)
  })
  return { CRUD: vmInfo.CRUD, crud }
}

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
  const _data = lodash.cloneDeep(data)
  // 以上为基础数据
  const crud = reactive(Object.assign({}, _data))
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
    // formName: 'form',
    // table,emptyText
    emptyText: '等待加载',
    // 表格数据
    data: [],
    dataPath: 'content',
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
    // 当前行详情
    rowDetail: {},
    // 详情通过接口加载
    detailFormApi: true,
    // Form 表单
    form: {},
    // 批量添加表单
    batchForm: {},
    // 重置表单
    defaultBatchForm: () => {},
    // 重置表单
    defaultForm: () => {},
    // 表单是否缓存key
    formStoreKey: '',
    // 表单缓存
    formStore: false,
    // 表单使用草稿后的回调
    useFormDraftCallback: null,
    // 清除草稿的回调
    clearFormDraftCallback: null,
    // 批量表单使用草稿后的回调
    useBatchFormDraftCallback: null,
    // 清除草稿的回调
    clearBatchFormDraftCallback: null,
    // 默认隐藏列
    invisibleColumns: ['createTime', 'updateTime'],
    // 提交时必填字段
    requiredSubmitField: [],
    // 批量添加表单提交时必填字段
    requiredBatchSubmitField: [],
    // 提交回调结果
    submitResult: null,
    // 排序规则，默认 id 降序， 支持多字段排序 ['id.desc', 'createTime.asc']
    sort: ['id.desc'],
    // 等待时间
    time: 50,
    // 首次查询时间
    firstQueryTime: null,
    // CRUD Method
    crudApi: {
      add: (form) => {},
      batchAdd: (form) => {},
      delete: (id) => {},
      edit: (form) => {},
      get: (id) => {}
    },
    // 主页操作栏显示哪些按钮
    optShow: {
      batchAdd: false,
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
    // 可查询的
    queryable: true,
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
      size: tablePageSize.value,
      // 总数据条数
      total: 0,
      hasNextPage: true
    },
    // 详情显示
    detailVisible: false,
    // 详情加载
    detailLoading: false,
    // 详情加载
    editDetailLoading: false,
    // 首次加载
    firstLoaded: false,
    // 整体loading
    loading: false,
    // 导出的 Loading
    downloadLoading: false,
    // 删除的 Loading
    delAllLoading: false,
    // 是否显示搜索项
    searchToggle: true
  }
  return data
}

// 添加默认信息
function addCrudDefaultInfo(crud, data) {
  Object.assign(crud, {
    // 避免默认sort变更
    defaultSort: data.sort,
    // 记录初始默认的查询参数，后续重置查询时使用
    defaultQuery: JSON.parse(JSON.stringify(data.query)),
    // 预留4位存储：组件 主页、头部、分页、表单，调试查看也方便找
    vms: [],
    // 用于存放tableRef、formRef等
    ref: {},
    // 表格列
    tableColumns: {},
    status: {
      // status 需要放在深拷贝之后加入，因此放在该对象中
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
        return this.add > CRUD.STATUS.NORMAL ? `新增${data.title}` : this.edit > CRUD.STATUS.NORMAL ? `编辑${data.title}` : ''
      }
    },
    bStatus: {
      // bStatus 需要放在深拷贝之后加入，因此放在该对象中
      batchAdd: CRUD.STATUS.NORMAL,
      batchEdit: CRUD.STATUS.NORMAL,
      // 添加或编辑状态
      get cu() {
        if (this.batchAdd === CRUD.STATUS.NORMAL && this.batchEdit === CRUD.STATUS.NORMAL) {
          return CRUD.STATUS.NORMAL
        } else if (this.batchAdd === CRUD.STATUS.PREPARED || this.batchEdit === CRUD.STATUS.PREPARED) {
          return CRUD.STATUS.PREPARED
        } else if (this.batchAdd === CRUD.STATUS.PROCESSING || this.batchEdit === CRUD.STATUS.PROCESSING) {
          return CRUD.STATUS.PROCESSING
        }
        throw new Error('错误的状态')
      },
      // 标题
      get title() {
        return this.batchAdd > CRUD.STATUS.NORMAL ? `新增${data.title}` : this.batchEdit > CRUD.STATUS.NORMAL ? `编辑${data.title}` : ''
      }
    },
    detailTitle: `${data.title}详情`
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
    const result = crud.requiredQuery.some((v) => isBlank(crud.query[v]))
    return !result
  }

  const verifySubmit = () => {
    const result = crud.requiredSubmitField.some((v) => crud.form[v] === null || crud.form[v] === undefined)
    return !result
  }

  // 校验批量提交
  const verifyBatchSubmit = () => {
    const result = crud.requiredBatchSubmitField.some((v) => crud.batchForm[v] === null || crud.batchForm[v] === undefined)
    return !result
  }

  // 搜索
  const toQuery = async () => {
    if (!crud.queryable) return
    // TODO:【考虑删除】若不等待加载完vm完毕后再查询，钩子可能会无法触发(例：首次加载通过watch,immediate:true触发),因此在下方加入settimeout，还需优化
    const vmSet = new Set()
    crud.vms.forEach((vm) => vm && vmSet.add(vm.vm))
    if (!vmSet.size) {
      setTimeout(() => {
        crud.toQuery()
      }, CRUD.QUERY_DEBOUNCE_TIME)
      return
    }
    if (!verifyQuery()) {
      crud.data = []
      crud.page.total = 0
      crud.emptyText = '暂无数据'
      return
    }
    const now = Date.now()
    // TODO: 存在问题，待优化，当queryTime时间呗，第二次进入的参数传的不一样时，会被拦截的问题
    const flag =
      (crud.firstQueryTime && now - crud.firstQueryTime > CRUD.QUERY_DEBOUNCE_TIME) || (!crud.firstQueryTime && crud.firstQueryTime !== 0)
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
    if (!checkPermission(crud.permission.get)) {
      crud.emptyText = '暂无权限'
      return
    }
    crud.emptyText = '加载中'
    if (!(await callVmHook(crud, CRUD.HOOK.beforeRefresh)) && !verifyQuery()) {
      crud.data = []
      crud.page.total = 0
      crud.emptyText = '重新加载'
      return
    }

    let data = []
    // 刷新时清空选中
    crud.selectAllChange()
    try {
      crud.loading = true
      data = await crud.crudApi.get(crud.getQueryParams())
      const res = { data: data }
      crud.emptyText = '暂无数据'
      // data.content = data.content || []
      await callVmHook(crud, CRUD.HOOK.handleRefresh, res)
      crud.page.total = data.totalElements
      crud.page.hasNextPage = data.hasNextPage
      crud.data = isNotBlank(crud.dataPath) ? res.data[crud.dataPath] || [] : res.data || []
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

  // 打开详情
  const toDetail = async (data) => {
    // 避免detailLoading未正确重置为false的情况，因此在头部初始化
    crud.detailLoading = false
    if (crud.detailFormApi && typeof crud.crudApi.detail === 'function') {
      crud.resetRowDetail(data)
      crud.detailLoading = true
      // 后期如果出现查询项不为id，则改造当前方法，例：在crud中传入自定义参数字段
      crud.crudApi
        .detail(data.id)
        .then((val) => {
          crud.resetRowDetail(val)
          callVmHook(crud, CRUD.HOOK.beforeDetailLoaded, crud.rowDetail).then(() => {
            crud.detailLoading = false
          })
        })
        .catch(() => {
          cancelDetail()
          ElMessage.error('加载失败')
          crud.detailLoading = false
        })
    } else {
      crud.resetRowDetail(data)
    }
    // crud.resetRowDetail(data)
    if (!(await callVmHook(crud, CRUD.HOOK.beforeToDetail, crud.rowDetail))) {
      return
    }
    crud.detailVisible = true
    await callVmHook(crud, CRUD.HOOK.afterToDetail, crud.rowDetail)
  }

  // 关闭详情
  const cancelDetail = async (data) => {
    if (!(await callVmHook(crud, CRUD.HOOK.beforeDetailCancel, data))) {
      return
    }
    crud.resetRowDetail() // 关闭详情时，清空detail
    crud.detailVisible = false
    await callVmHook(crud, CRUD.HOOK.afterDetailCancel, data)
  }

  // 打开添加
  const toAdd = async () => {
    if (!((await callVmHook(crud, CRUD.HOOK.beforeToAdd, crud.form)) && (await callVmHook(crud, CRUD.HOOK.beforeToCU, crud.form)))) {
      return
    }
    crud.status.add = CRUD.STATUS.PREPARED
    crud.submitResult = null
    await callVmHook(crud, CRUD.HOOK.afterToAdd, crud.form)
    await callVmHook(crud, CRUD.HOOK.afterToCU, crud.form)
  }

  const toEdit = async (data) => {
    crud.resetForm(JSON.parse(JSON.stringify(data)))
    // 避免editDetailLoading未正确重置为false的情况，因此在头部初始化
    crud.editDetailLoading = false
    if (crud.detailFormApi && typeof crud.crudApi.detail === 'function') {
      crud.editDetailLoading = true
      // 后期如果出现查询项不为id，则改造当前方法，例：在crud中传入自定义参数字段
      crud.crudApi
        .detail(data.id)
        .then((val) => {
          crud.resetForm(JSON.parse(JSON.stringify(val)))
          callVmHook(crud, CRUD.HOOK.beforeEditDetailLoaded, crud.form).then(() => {
            crud.editDetailLoading = false
          })
        })
        .catch(() => {
          cancelCU()
          ElMessage.error('加载失败')
          crud.editDetailLoading = false
        })
    }
    if (!((await callVmHook(crud, CRUD.HOOK.beforeToEdit, crud.form)) && (await callVmHook(crud, CRUD.HOOK.beforeToCU, crud.form)))) {
      return
    }
    crud.status.edit = CRUD.STATUS.PREPARED
    crud.submitResult = null
    crud.getDataStatus(data.id).edit = CRUD.STATUS.PREPARED
    await callVmHook(crud, CRUD.HOOK.afterToEdit, crud.form)
    await callVmHook(crud, CRUD.HOOK.afterToCU, crud.form)
  }

  // 打开批量添加
  const toBatchAdd = async () => {
    if (
      !(
        (await callVmHook(crud, CRUD.HOOK.beforeToBatchAdd, crud.batchForm)) &&
        (await callVmHook(crud, CRUD.HOOK.beforeToBCU, crud.batchForm))
      )
    ) {
      return
    }
    crud.bStatus.batchAdd = CRUD.STATUS.PREPARED
    crud.submitResult = null
    await callVmHook(crud, CRUD.HOOK.afterToBatchAdd, crud.batchForm)
    await callVmHook(crud, CRUD.HOOK.afterToBCU, crud.batchForm)
  }

  // 打开批量修改
  const toBatchEdit = async (data) => {
    crud.resetBatchForm(JSON.parse(JSON.stringify(data)))
    if (
      !(
        (await callVmHook(crud, CRUD.HOOK.beforeToBatchEdit, crud.batchForm)) &&
        (await callVmHook(crud, CRUD.HOOK.beforeToBCU, crud.batchForm))
      )
    ) {
      return
    }
    crud.bStatus.edit = CRUD.STATUS.PREPARED
    crud.submitResult = null
    crud.getDataStatus(data.id).batchEdit = CRUD.STATUS.PREPARED
    await callVmHook(crud, CRUD.HOOK.afterToBatchEdit, crud.batchForm)
    await callVmHook(crud, CRUD.HOOK.afterToBCU, crud.batchForm)
  }

  // 启动删除
  const toDelete = (data) => {
    crud.getDataStatus(data.id).delete = CRUD.STATUS.PREPARED
  }

  // 取消删除
  const cancelDelete = async (data) => {
    if (!(await callVmHook(crud, CRUD.HOOK.beforeDeleteCancel, data))) {
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
    if (crud.ref.form) {
      nextTick(() => {
        crud.ref.form.clearValidate()
      })
    }
  }

  /**
   * 取消批量新增/编辑
   */
  const cancelBCU = async () => {
    const addStatus = crud.bStatus.batchAdd
    const editStatus = crud.bStatus.batchEdit
    if (addStatus === CRUD.STATUS.PREPARED) {
      if (!(await callVmHook(crud, CRUD.HOOK.beforeBatchAddCancel, crud.batchForm))) {
        return
      }
      crud.bStatus.batchAdd = CRUD.STATUS.NORMAL
    }
    if (editStatus === CRUD.STATUS.PREPARED) {
      if (!(await callVmHook(crud, CRUD.HOOK.beforeBatchEditCancel, crud.batchForm))) {
        return
      }
      crud.bStatus.batchEdit = CRUD.STATUS.NORMAL
      crud.getDataStatus(crud.batchForm.id).batchEdit = CRUD.STATUS.NORMAL
    }
    crud.resetBatchForm()
    if (addStatus === CRUD.STATUS.PREPARED) {
      await callVmHook(crud, CRUD.HOOK.afterBatchAddCancel, crud.batchForm)
    }
    if (editStatus === CRUD.STATUS.PREPARED) {
      await callVmHook(crud, CRUD.HOOK.afterBatchEditCancel, crud.batchForm)
    }
    // 清除表单验证
    if (crud.ref.batchForm) {
      nextTick(() => {
        crud.ref.batchForm.clearValidate()
      })
    }
  }

  // 提交新增/编辑
  const submitCU = async () => {
    if (!verifySubmit()) {
      return
    }
    if (!(await callVmHook(crud, CRUD.HOOK.beforeValidateCU))) {
      return
    }
    crud.ref.form.validate(async (valid) => {
      if (!valid) {
        return
      }
      if (!(await callVmHook(crud, CRUD.HOOK.afterValidateCU))) {
        return
      }
      if (crud.status.add === CRUD.STATUS.PREPARED) {
        doAdd()
      } else if (crud.status.edit === CRUD.STATUS.PREPARED) {
        doEdit()
      }
    })
  }

  // 提交批量新增/编辑
  const submitBCU = async () => {
    if (!verifyBatchSubmit()) {
      return
    }
    if (!(await callVmHook(crud, CRUD.HOOK.beforeValidateBCU))) {
      return
    }
    crud.ref.batchForm.validate(async (valid) => {
      if (!valid) {
        return
      }
      if (!(await callVmHook(crud, CRUD.HOOK.afterValidateBCU))) {
        return
      }
      if (crud.bStatus.batchAdd === CRUD.STATUS.PREPARED) {
        doBatchAdd()
      } else if (crud.bStatus.batchEdit === CRUD.STATUS.PREPARED) {
        doBatchEdit()
      }
    })
  }

  // 执行添加
  const doAdd = async () => {
    if (!(await callVmHook(crud, CRUD.HOOK.beforeSubmit))) {
      return
    }
    try {
      crud.status.add = CRUD.STATUS.PROCESSING
      const data = await crud.submitFormFormat(lodash.cloneDeep(crud.form))
      crud.submitResult = await crud.crudApi.add(data)
      await callVmHook(crud, CRUD.HOOK.afterAddSuccess)
      crud.status.add = CRUD.STATUS.NORMAL
      crud.resetForm()
      // 清除表单验证,避免在form重置后，由于值清空触发change
      if (crud.ref.form) {
        nextTick(() => {
          crud.ref.form.clearValidate()
        })
      }
      crud.addSuccessNotify()
      await callVmHook(crud, CRUD.HOOK.afterSubmit)
      crud.toQuery()
    } catch (error) {
      console.log('添加', error)
      crud.status.add = CRUD.STATUS.PREPARED
      await callVmHook(crud, CRUD.HOOK.afterAddError)
    }
  }

  // 执行编辑
  const doEdit = async () => {
    if (!(await callVmHook(crud, CRUD.HOOK.beforeSubmit))) {
      return
    }
    try {
      crud.status.edit = CRUD.STATUS.PROCESSING
      const data = await crud.submitFormFormat(lodash.cloneDeep(crud.form))
      crud.submitResult = await crud.crudApi.edit(data)
      crud.status.edit = CRUD.STATUS.NORMAL
      crud.getDataStatus(crud.form.id).edit = CRUD.STATUS.NORMAL
      crud.editSuccessNotify()
      crud.resetForm()
      // 清除表单验证
      if (crud.ref.form) {
        nextTick(() => {
          crud.ref.form.clearValidate()
        })
      }
      await callVmHook(crud, CRUD.HOOK.afterSubmit)
      crud.refresh()
    } catch (error) {
      console.log('编辑', error)
      crud.status.edit = CRUD.STATUS.PREPARED
      await callVmHook(crud, CRUD.HOOK.afterEditError)
    }
  }

  // 执行批量添加
  const doBatchAdd = async () => {
    if (!(await callVmHook(crud, CRUD.HOOK.beforeBatchSubmit))) {
      return
    }
    try {
      crud.bStatus.batchAdd = CRUD.STATUS.PROCESSING
      // 深拷贝表单后转换，避免表单发生变化
      const data = await crud.submitBatchFormFormat(lodash.cloneDeep(crud.batchForm))
      crud.submitResult = await crud.crudApi.batchAdd(data)
      await callVmHook(crud, CRUD.HOOK.afterBatchAddSuccess)
      crud.bStatus.batchAdd = CRUD.STATUS.NORMAL
      crud.resetBatchForm()
      crud.addSuccessNotify()
      await callVmHook(crud, CRUD.HOOK.afterBatchSubmit)
      crud.toQuery()
    } catch (error) {
      console.log('添加', error)
      crud.bStatus.batchAdd = CRUD.STATUS.PREPARED
      await callVmHook(crud, CRUD.HOOK.afterBatchAddError)
    }
  }

  // 执行编辑
  const doBatchEdit = async () => {
    if (!(await callVmHook(crud, CRUD.HOOK.beforeBatchSubmit))) {
      return
    }
    try {
      crud.bStatus.batchEdit = CRUD.STATUS.PROCESSING
      crud.submitResult = await crud.crudApi.batchEdit(crud.batchForm)
      crud.bStatus.batchEdit = CRUD.STATUS.NORMAL
      // TODO: batchForm 此处会有问题，批量修改暂不存在
      crud.getDataStatus(crud.batchForm.id).edit = CRUD.STATUS.NORMAL
      crud.editSuccessNotify()
      crud.resetBatchForm()
      await callVmHook(crud, CRUD.HOOK.afterBatchSubmit)
      crud.refresh()
    } catch (error) {
      console.log('编辑', error)
      crud.bStatus.batchEdit = CRUD.STATUS.PREPARED
      await callVmHook(crud, CRUD.HOOK.afterBatchEditError)
    }
  }

  // 执行删除
  const doDelete = async (data) => {
    let delAll = false
    let dataStatus
    const ids = []
    if (data instanceof Array) {
      delAll = true
      data.forEach((val) => {
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
    // TODO:查看代码逻辑是否有问题
    return crud.crudApi
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
  }

  const _toQueryByDebounce = debounce(
    async () => {
      _toQuery()
    },
    CRUD.QUERY_DEBOUNCE_TIME,
    false
  )

  const _toQuery = async () => {
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
  }

  Object.assign(crud, {
    submitSuccessNotify, // 表单提交成功通知
    addSuccessNotify, // 添加成功
    editSuccessNotify, // 编辑成功通知
    delSuccessNotify, // 删除成功通知
    toQuery, // 搜索
    refresh, // 刷新v
    toDetail, // 打开详情
    cancelDetail, // 关闭详情
    toAdd, // 启动添加
    toEdit, // 启动编辑
    toDelete, // 启动删除
    cancelDelete, // 取消删除
    cancelCU, // 取消新增/编辑
    submitCU, // 提交新增/编辑
    doDelete, // 执行删除
    cancelBCU, // 取消新增/编辑（批量）
    submitBCU, // 提交新增/编辑（批量）
    toBatchAdd, // 启动添加（批量）
    toBatchEdit, // 启动编辑（批量）
    doBatchAdd, // 执行添加（批量）
    doBatchEdit // 执行编辑（批量）
  })
}

// 添加crud功能性方法
function addCrudFeatureMethod(crud, data) {
  // 通用导出
  const doExport = async (data) => {
    try {
      crud.downloadLoading = true
      await fileDownload(crud.crudApi.download, data)
    } catch (error) {
      console.log(error)
    } finally {
      crud.downloadLoading = false
    }
  }

  // 设置列 TODO:当设置查询默认值，table通过该默认值设置key的情况下，会出现问题，例：甲供材料借出管理
  const setColumns = () => {
    if (!crud.ref.table) return
    nextTick(() => {
      const columns = crud.tableColumns
      // 1：保留上一次的列
      const lastColumns = lodash.cloneDeep(columns)
      // 2：删除所有列表字段（下方重新计算），保留visible方法
      Object.keys(columns).forEach((key) => {
        if (key !== 'visible') delete columns[key]
      })
      // 3:获得table的所有列
      const tableColumns = crud.ref.table.getColumns()
      // 4:获取当前所有列，此时的列在部分情况下会缺失,因此这一步，只设置visible为true
      // 例如：在某些情况下，table.getColumns()读取字段时，dom已经被v-if display：none掉
      tableColumns.forEach((e) => {
        if (!e.property || e.type !== 'default') {
          return
        }
        columns[e.property] = {
          label: e.label,
          visible: true
        }
      })
      // 5.待页面渲染后，重新再获取列。此时获取的列，包含第四步中“某些情况下”的列
      nextTick(() => {
        // ，删除所有列表字段（重新设置，避免展示顺序出现问题，不重新设置会导致原来隐藏的都排在后面），保留visible方法
        Object.keys(columns).forEach((key) => {
          if (key !== 'visible') delete columns[key]
        })
        const tableColumns = crud.ref.table.getColumns()
        // 获得table的所有列
        tableColumns.forEach((e) => {
          if (!e.property || e.type !== 'default') {
            return
          }
          // 设置默认隐藏
          columns[e.property] = {
            label: e.label
          }
          // 设置列的visible状态
          if (lastColumns[e.property]) {
            columns[e.property].visible = lastColumns[e.property].visible // 之前隐藏的列，继续隐藏
          } else {
            columns[e.property].visible = crud.invisibleColumns.indexOf(e.property) === -1 // 默认隐藏
          }
        })
      })
    })
    // // 显示列的方法
    // crud.tableColumns = columns
  }
  // 获取查询参数
  const getQueryParams = () => {
    return {
      pageNumber: crud.hasPagination ? crud.page.page : undefined,
      pageSize: crud.hasPagination ? crud.page.size : undefined,
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
    if (!(await callVmHook(crud, CRUD.HOOK.beforeResetQuery, data))) {
      return
    }
    const defaultQuery = JSON.parse(JSON.stringify(crud.defaultQuery))
    const query = crud.query
    Object.keys(query).forEach((key) => {
      if (defaultQuery[key]) {
        // 字段是否可重置
        if (typeof defaultQuery[key] === 'object' && isNotBlank(defaultQuery[key].resetAble)) {
          if (defaultQuery[key].resetAble) query[key] = defaultQuery[key].value
        } else {
          query[key] = defaultQuery[key]
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
   * 重置详情对象
   * @param {Array} data 数据
   */
  const resetRowDetail = (data) => {
    const detail = data || {}
    const rowDetail = crud.rowDetail
    for (const key in rowDetail) {
      rowDetail[key] = undefined
    }
    for (const key in detail) {
      rowDetail[key] = detail[key]
    }
  }
  /**
   * 重置表单
   * @param {Array} data 数据
   */
  const resetForm = (data) => {
    const ref = crud.ref.form
    // 清除表单信息 TODO:待改待测
    if (ref) {
      // 设置默认值，因此重置放在顶部
      ref.resetFields()
    }
    const form = data || (typeof crud.defaultForm === 'object' ? JSON.parse(JSON.stringify(crud.defaultForm)) : {})
    const crudFrom = crud.form
    for (const key in crudFrom) {
      crudFrom[key] = undefined
    }
    for (const key in form) {
      crudFrom[key] = form[key]
    }
    // 清除表单验证
    if (ref) {
      nextTick(() => {
        ref.clearValidate()
      })
    }
  }
  /**
   * 重置批量表单
   * @param {Array} data 数据
   */
  const resetBatchForm = (data) => {
    const ref = crud.ref.batchForm
    // 清除表单信息 TODO:待改待测
    if (ref) {
      // 设置默认值，因此重置放在顶部
      ref.resetFields()
    }
    const form =
      data || (typeof crud.defaultBatchForm === 'object' ? JSON.parse(JSON.stringify(crud.defaultBatchForm)) : crud.defaultBatchForm())
    const crudBatchForm = crud.batchForm
    for (const key in crudBatchForm) {
      crudBatchForm[key] = undefined
    }
    for (const key in form) {
      crudBatchForm[key] = form[key]
    }
  }

  // 详情重新加载
  const reloadDetail = (id) => {
    const detailId = id || (crud.rowDetail ? crud.rowDetail.id : void 0)
    if (detailId && crud.detailFormApi && typeof crud.crudApi.detail === 'function') {
      // crud.resetRowDetail(data)
      crud.detailLoading = true
      // 后期如果出现查询项不为id，则改造当前方法，例：在crud中传入自定义参数字段
      crud.crudApi
        .detail(detailId)
        .then((val) => {
          crud.resetRowDetail(val)
          callVmHook(crud, CRUD.HOOK.beforeDetailLoaded, crud.rowDetail).then(() => {
            crud.detailLoading = false
          })
        })
        .catch((error) => {
          console.log('详情加载失败', error)
          ElMessage.error('详情加载失败')
          crud.detailLoading = false
        })
    }
  }

  // 表单验证
  const validateField = (field) => {
    crud.ref.form.validateField(field)
  }

  // 批量添加表单验证
  const validateFieldForBatch = (field) => {
    crud.ref.batchForm.validateField(field)
  }

  // 提交表单数据格式化
  const submitFormFormat = (form) => {
    return form
  }

  // 提交批量表单数据格式化
  const submitBatchFormFormat = (form) => {
    return form
  }

  // 重置数据状态
  const resetDataStatus = () => {
    const dataStatus = {}
    function resetStatus(datas = []) {
      datas.forEach((e) => {
        dataStatus[e.id] = {
          delete: 0,
          edit: 0,
          batchEdit: 0
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
      selection.forEach((val) => {
        crud.selectChange(selection, val)
      })
    } else {
      crud.ref.table && crud.ref.table.clearSelection()
    }
  }
  /**
   * 用于树形表格多选，单选的封装
   * @param selection
   * @param row
   */
  const selectChange = (selection, row) => {
    // 如果selection中存在row代表是选中，否则是取消选中
    if (
      selection.find((val) => {
        return val.id === row.id
      })
    ) {
      if (row.children) {
        row.children.forEach((val) => {
          // TODO: 待改待测
          crud.ref.table.toggleRowSelection(val, true)
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
      data.children.forEach((val) => {
        crud.ref.table.toggleRowSelection(val, false)
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
      sort = crud.defaultSort
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

  // 显示的列
  const obColumns = (columns) => {
    return {
      visible(col) {
        return !columns || !columns.value[col] ? true : columns.value[col].visible
      }
    }
  }

  // 设置自定义扩展参数
  const updateProp = (name, value) => {
    crud.props[name] = value
  }

  Object.assign(crud, {
    doExport, // 通用导出
    getQueryParams, //  获取查询参数
    pageChangeHandler, // 当前页码改变
    sizeChangeHandler, // 每页条数改变
    dleChangePage, // 预防删除第二页最后一条数据时，或者多选删除第二页的数据时，页码错误导致请求无数据
    resetQuery, // 重置查询参数,重置后进行查询操作
    resetRowDetail, // 重置详情
    resetForm, // 重置表单
    resetBatchForm, // 重置表单
    reloadDetail, // 重新加载详情
    validateField, // 表单字段校验
    validateFieldForBatch, // 批量表单字段校验
    submitFormFormat, // 提交表单数据格式化
    submitBatchFormFormat, // 提交批量表单数据格式化
    resetDataStatus, // 重置数据状态
    getDataStatus, // 获取数据状态
    selectionChangeHandler, // 选择改变
    selectAllChange, // 用于树形表格多选, 选中所有
    selectChange, // 用于树形表格多选，单选的封装
    toggleRowSelection, // 切换选中状态
    handleSortChange, // 处理排序方式改变
    notify,
    setColumns, // 获取列
    obColumns, // 获取显示的列
    updateProp // 设置自定义扩展参数
  })
}

// 添加crud主要信息
function addCrudMethod(crud, data) {
  // 初始化crud
  const init = () => {
    Object.assign(crud, lodash.cloneDeep(data))
    crud.status.add = CRUD.STATUS.NORMAL
    crud.status.edit = CRUD.STATUS.NORMAL
    crud.bStatus.batchAdd = CRUD.STATUS.NORMAL
    crud.bStatus.batchEdit = CRUD.STATUS.NORMAL
  }

  // 注册组件
  const registerVM = (type, vm, index = -1) => {
    const vmInfo = {
      uid: vm.uid,
      uuid: `${vm.uid}_${new Date().getTime()}_${crud.vms.length}`,
      type,
      vm,
      CRUD: {
        HOOK: {},
        STATUS: CRUD.STATUS,
        NOTIFICATION_TYPE: CRUD.NOTIFICATION_TYPE
      } // 用于hook
    }
    // 默认新加入的vm放在数组最后面
    if (index < 0) {
      crud.vms.push(vmInfo)
    } else {
      // 将vm插入指定位置
      crud.vms.length = Math.max(crud.vms.length, index)
      crud.vms.splice(index, 1, vmInfo)
    }
    return vmInfo
  }

  // 注销组件
  const unregisterVM = (vm) => {
    const del = crud.vms.splice(
      crud.vms.findIndex((e) => e && e.uid === vm.uid),
      1
    )
    return del
  }

  // 查找组件
  const findVM = (val, field = 'type') => {
    return crud.vms.find((vm) => vm && vm[field] === val).vm
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
  if (crud.debug) {
    // 可查看hook调用情况
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
    if (VM && VM.CRUD.HOOK[hook]) {
      const res = await VM.CRUD.HOOK[hook].apply(VM, args)
      result = res !== false && result
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
  Object.keys(source).forEach((key) => {
    if (Object.prototype.hasOwnProperty.call(rewrite, key)) {
      opts[key] = rewrite[key]
    }
  })
  return opts
}

CRUD.QUERY_DEBOUNCE_TIME = 350 // 查询防抖时间

// CRUD 组件类型
CRUD.VM_TYPE = {
  PRESENTER: 'presenter',
  HEADER: 'header',
  PAGINATION: 'pagination',
  FORM: 'form',
  DETAIL: 'detail',
  BATCH_FORM: 'batchForm',
  OTHER: 'other'
}

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
  /** 详情 - 之前 */
  beforeToDetail: 'beforeToDetail',
  /** 详情 - 之后 */
  afterToDetail: 'afterToDetail',
  /** 详情加载后 */
  beforeDetailLoaded: 'beforeDetailLoaded',
  /** 详情关闭 - 之前 */
  beforeDetailCancel: 'beforeDetailCancel',
  /** 详情关闭 - 之后 */
  afterDetailCancel: 'afterDetailCancel',
  /** 新建 - 之前 */
  beforeToAdd: 'beforeToAdd',
  /** 新建 - 之后 */
  afterToAdd: 'afterToAdd',
  /** 编辑时详情加载后 */
  beforeEditDetailLoaded: 'beforeEditDetailLoaded',
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
  /** 添加成功 - 之后 */
  afterAddSuccess: 'afterAddSuccess',
  /** 添加失败 - 之后 */
  afterAddError: 'afterAddError',
  /** 编辑失败 - 之后 */
  afterEditError: 'afterEditError',

  /** 新建 - 之前 */
  beforeToBatchAdd: 'beforeToBatchAdd',
  /** 新建 - 之后 */
  afterToBatchAdd: 'afterToBatchAdd',
  /** 编辑 - 之前 */
  beforeToBatchEdit: 'beforeToBatchEdit',
  /** 编辑 - 之后 */
  afterToBatchEdit: 'afterToBatchEdit',
  /** 开始 "新建/编辑" - 之前 */
  beforeToBCU: 'beforeToBCU',
  /** 开始 "新建/编辑" - 之后 */
  afterToBCU: 'afterToBCU',
  /** "新建/编辑" 验证 - 之前 */
  beforeValidateBCU: 'beforeValidateBCU',
  /** "新建/编辑" 验证 - 之后 */
  afterValidateBCU: 'afterValidateBCU',
  /** 添加取消 - 之前 */
  beforeBatchAddCancel: 'beforeBatchAddCancel',
  /** 添加取消 - 之后 */
  afterBatchAddCancel: 'afterBatchAddCancel',
  /** 编辑取消 - 之前 */
  beforeBatchEditCancel: 'beforeBatchEditCancel',
  /** 编辑取消 - 之后 */
  afterBatchEditCancel: 'afterBatchEditCancel',
  /** 提交 - 之前 */
  beforeBatchSubmit: 'beforeBatchSubmit',
  /** 提交 - 之后 */
  afterBatchSubmit: 'afterBatchSubmit',
  /** 添加成功 - 之后 */
  afterBatchAddSuccess: 'afterBatchAddSuccess',
  /** 添加失败 - 之后 */
  afterBatchAddError: 'afterBatchAddError',
  /** 编辑失败 - 之后 */
  afterBatchEditError: 'afterBatchEditError',

  /** 重置搜索条件 - 之前 */
  beforeResetQuery: 'beforeResetQuery',
  /** TODO:未定义 重置搜索条件 - 之后 */
  afterResetQuery: 'afterResetQuery',
  /** 处理刷新数据 */
  handleRefresh: 'handleRefresh'
}
