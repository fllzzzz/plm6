import { provide, inject, reactive, getCurrentInstance, onBeforeUnmount, onUnmounted, nextTick } from 'vue'
import * as lodash from 'lodash'

import useFormLocalStorage from '@/composables/form/use-form-local-storage'

import { ElNotification } from 'element-plus'

const FORM = {} // crud公共信息处理

/**
 * crud组件
 * @param {*} options 组件自定义选项
 * @param {*} tableRef 非必填 (由于执行顺序问题，无法直接从实例中获取该值，需要传入)
 * @param {*} registerForm= true 是否注册主组件，若不注册可之后调用registerForm方法注册
 */
export default function useForm(options, formRef, registerForm = true) {
  // 获取crud实例
  const crud = getCrud(options)
  // 不能添加新属性，也不能重新配置或者删除任何现有属性（但是可以修改属性的值）
  Object.seal(crud)
  if (registerForm) {
    const data = register(crud, formRef)
    return data
  }
  return { crud }
}

/**
 * 主页面注册
 * @param {*} crud
 * @param {*} tableRef
 * @returns { columns, crud } = { 显示的列, crud }
 */
export function register(crud, formRef) {
  const internalInstance = getCurrentInstance()
  // 注册组件
  const vmInfo = crud.registerVM(FORM.VM_TYPE.FORM, internalInstance, 2)
  vmInfo.formRef = formRef
  crud.ref.form = formRef
  crud.resetForm()

  // 添加表单缓存
  let fmStore = {}
  if (crud.formStore) {
    const store = useFormLocalStorage(crud.formStoreKey, crud, vmInfo.FORM)
    fmStore = store
  }

  onBeforeUnmount(() => {
    crud.unregisterVM(internalInstance)
    delete crud.ref.form
  })

  // TODO:卸载后初始化crud
  onUnmounted(() => {
    crud.init()
  })

  provide('cu', crud)
  provide('permission', crud.permission)
  provide('form', crud.form)
  return { FORM: vmInfo.FORM, cu: crud, form: crud.form, ADD_FORM: fmStore }
}

/**
 * 注册其他组件
 * @param {object} options 选项
 */
export function regExtra(options = {}) {
  const crud = inject('cu')
  const internalInstance = getCurrentInstance()
  // 注册组件
  const vmInfo = crud.registerVM(FORM.VM_TYPE.OTHER, internalInstance)

  onBeforeUnmount(() => {
    crud.unregisterVM(internalInstance)
  })
  return { FORM: vmInfo.FORM, cu: crud, form: crud.form }
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
    // Form 表单
    form: {},
    // 重置表单
    defaultForm: () => {},
    // 表单是否缓存key
    formStoreKey: '',
    // 表单缓存
    formStore: false,
    // 提交时必填字段
    requiredSubmitField: [],
    // 提交回调结果
    submitResult: null,
    // 等待时间
    time: 50,
    // FORM Method
    api: null,
    // 自定义一些扩展属性
    props: {},
    // 权限
    permission: [],
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
  const data = {
    ...options,
    initOptions: options,
    dataStatus: {}, // 记录数据状态
    msg: {
      submit: '提交成功'
    }
  }
  return data
}

// 添加默认信息
function addCrudDefaultInfo(crud, data) {
  Object.assign(crud, {
    // 预留4位存储：组件 主页、头部、分页、表单，调试查看也方便找
    vms: [],
    // 用于存放tableRef、formRef等
    ref: {},
    // 表格列
    tableColumns: {},
    status: {
      edit: FORM.STATUS.NORMAL
    }
  })
}

// 添加crud主要业务使用的方法
function addCrudBusinessMethod(crud) {
  // 操作提示
  const submitSuccessNotify = () => {
    crud.notify(crud.msg.submit, FORM.NOTIFICATION_TYPE.SUCCESS)
  }

  const toEdit = async (data) => {
    crud.resetForm(JSON.parse(JSON.stringify(data)))
    if (!(await callVmHook(crud, FORM.HOOK.beforeToEdit, crud.form) && await callVmHook(crud, FORM.HOOK.beforeToCU, crud.form))) {
      return
    }
    crud.status.edit = FORM.STATUS.PREPARED
    crud.submitResult = null
    await callVmHook(crud, FORM.HOOK.afterToEdit, crud.form)
    await callVmHook(crud, FORM.HOOK.afterToCU, crud.form)
  }

  /**
     * 取消新增/编辑
     */
  const cancelCU = async () => {
    const editStatus = crud.status.edit

    if (editStatus === FORM.STATUS.PREPARED) {
      if (!await callVmHook(crud, FORM.HOOK.beforeEditCancel, crud.form)) {
        return
      }
      crud.status.edit = FORM.STATUS.NORMAL
    }
    crud.resetForm()
    if (editStatus === FORM.STATUS.PREPARED) {
      await callVmHook(crud, FORM.HOOK.afterEditCancel, crud.form)
    }
    // 清除表单验证
    if (crud.ref.form) {
      nextTick(() => {
        crud.ref.form.clearValidate()
      })
    }
  }

  // 提交新增/编辑
  const submitCU = async () => {
    if (!verifySubmit()) {
      return
    }
    if (!await callVmHook(crud, FORM.HOOK.beforeValidateCU)) {
      return
    }
    crud.ref.form.validate(async (valid) => {
      if (!valid) {
        return
      }
      if (!await callVmHook(crud, FORM.HOOK.afterValidateCU)) {
        return
      }
      if (crud.status.edit === FORM.STATUS.PREPARED) {
        doEdit()
      }
    })
  }

  // 执行编辑
  const doEdit = async () => {
    if (!await callVmHook(crud, FORM.HOOK.beforeSubmit)) {
      return
    }
    try {
      crud.status.edit = FORM.STATUS.PROCESSING
      const data = crud.submitFormFormat(lodash.cloneDeep(crud.form))
      crud.submitResult = await crud.api(data)
      await callVmHook(crud, FORM.HOOK.afterAddSuccess)
      crud.status.edit = FORM.STATUS.NORMAL
      crud.submitSuccessNotify()
      crud.resetForm()
      // 清除表单验证
      if (crud.ref.form) {
        nextTick(() => {
          crud.ref.form.clearValidate()
        })
      }
      await callVmHook(crud, FORM.HOOK.afterSubmit)
      crud.refresh()
    } catch (error) {
      console.log('编辑', error)
      crud.status.edit = FORM.STATUS.PREPARED
      await callVmHook(crud, FORM.HOOK.afterEditError)
    }
  }

  const verifySubmit = () => {
    const result = crud.requiredSubmitField.some(v => crud.form[v] === null || crud.form[v] === undefined)
    return !result
  }

  Object.assign(crud, {
    submitSuccessNotify, // 表单提交成功通知
    toEdit, // 启动编辑
    cancelCU, // 取消新增/编辑
    submitCU // 提交新增/编辑
  })
}

// 添加crud功能性方法
function addCrudFeatureMethod(crud, data) {
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
    const form = data || (typeof crud.defaultForm === 'object' ? JSON.parse(JSON.stringify(crud.defaultForm)) : crud.defaultForm())
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
  // 表单验证
  const validateField = (field) => {
    crud.ref.form.validateField(field)
  }

  // 提交表单数据格式化
  const submitFormFormat = (form) => {
    return form
  }
  // 通知
  const notify = (title, type = FORM.NOTIFICATION_TYPE.INFO) => {
    ElNotification({
      title,
      type,
      duration: 2500
    })
  }

  // 设置自定义扩展参数
  const updateProp = (name, value) => {
    crud.props[name] = value
  }

  Object.assign(crud, {
    resetForm, // 重置表单
    validateField, // 表单字段校验
    submitFormFormat, // 提交表单数据格式化
    notify,
    updateProp // 设置自定义扩展参数
  })
}

// 添加crud主要信息
function addCrudMethod(crud, data) {
  // 初始化crud
  const init = () => {
    Object.assign(crud, lodash.cloneDeep(data))
    crud.status.edit = FORM.STATUS.NORMAL
  }

  // 注册组件
  const registerVM = (type, vm, index = -1) => {
    const vmInfo = {
      uid: `${vm.uid}_${new Date().getTime()}_${crud.vms.length}`,
      type,
      vm,
      FORM: {
        HOOK: {},
        STATUS: FORM.STATUS,
        NOTIFICATION_TYPE: FORM.NOTIFICATION_TYPE
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
    const del = crud.vms.splice(crud.vms.findIndex(e => e && e.uid === vm.uid), 1)
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
    if (VM && VM.FORM.HOOK[hook]) {
      const res = await VM.FORM.HOOK[hook].apply(VM, args)
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
  Object.keys(source).forEach(key => {
    if (Object.prototype.hasOwnProperty.call(rewrite, key)) {
      opts[key] = rewrite[key]
    }
  })
  return opts
}

// FORM 组件类型
FORM.VM_TYPE = {
  FORM: 'form',
  OTHER: 'other'
}

// CRUD状态
FORM.STATUS = {
  NORMAL: 0,
  PREPARED: 1,
  PROCESSING: 2
}

// CRUD通知类型
FORM.NOTIFICATION_TYPE = {
  SUCCESS: 'success',
  WARNING: 'warning',
  INFO: 'info',
  ERROR: 'error'
}

// key 与 value 需要一致
FORM.HOOK = {
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
  /** 编辑取消 - 之前 */
  beforeEditCancel: 'beforeEditCancel',
  /** 编辑取消 - 之后 */
  afterEditCancel: 'afterEditCancel',
  /** 提交 - 之前 */
  beforeSubmit: 'beforeSubmit',
  /** 提交 - 之后 */
  afterSubmit: 'afterSubmit',
  /** 编辑失败 - 之后 */
  afterEditError: 'afterEditError'
}
