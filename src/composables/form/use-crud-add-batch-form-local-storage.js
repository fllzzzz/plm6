import { onBeforeUnmount, reactive, provide } from 'vue'
import storage from '@/utils/storage'
import { isBlank } from '@data-type/index'
import { isObjectValueEqual } from '@data-type/object'
import * as lodash from 'lodash'
import { regExtra } from '../use-crud'
// TODO: 版本更新后的表单变更缓存问题
// TODO: 考虑将异常与主动保存为草稿分开，在异常草稿时，给与用户提示

const ADD_FORM = {}

// 缓存key的前缀
ADD_FORM.KEY_PREFIX = 'ADD_BATCH_FORM'
ADD_FORM.TYPE = {
  normal: 1, // 正常保存
  browserClose: 2 // 浏览器关闭保存
}

export default function useCrudAddFormLocalStorage(key, { useDraftCallback, clearDraftCallback } = {}) {
  const { CRUD, crud } = regExtra()
  const ls = reactive({
    key: key,
    expired: 604800000,
    form: crud.batchForm, // 待处理的表单
    initForm: undefined, // 初始的表单
    init: undefined, // 初始化方法
    isRegister: false, // 是否注册的
    saveStoreForm: () => {
      // 保存草稿并退出
      const res = saveFormToStorage(ls)
      crud.cancelBCU()
      return res
    },
    resetForm: () => {
      clearFormStorage(ls)
      // 清除内容
      crud.resetBatchForm()
      ls.init && ls.init()
      if (typeof clearDraftCallback === 'function') clearDraftCallback()
    }
  })

  // crud添加的钩子中，尽量不要写除了ADD_FORM.init 的其他方法初始方法
  // 在打开后开启缓存
  CRUD.HOOK.afterToBatchAdd = () => openStore(ls, useDraftCallback)

  // 在退出后关闭缓存
  CRUD.HOOK.beforeBatchAddCancel = () => closeStore(ls)

  // 在添加成功后清除缓存
  CRUD.HOOK.afterBatchAddSuccess = () => {
    clearFormStorage(ls)
    closeStore(ls)
  }

  // 卸载时判断是否需要记录
  onBeforeUnmount(() => {
    abnormalClose(ls, crud)
    closeStore(ls)
  })

  // 浏览器关闭
  window.onbeforeunload = () => {
    abnormalClose(ls, crud)
  }

  provide('bfmStore', ls)
  return ls
}

// 当前组件初始化
function componentInit(ls) {
  // ls.form = undefined
  ls.initForm = undefined
}

// 打开记录本地缓存
function openStore(ls, useDraftCallback) {
  const storageFormInfo = getFormByStorage(ls.key)
  if (isBlank(storageFormInfo)) {
    // 如果缓存为空，则调用初始化方法
    ls.init && ls.init()
  } else {
    // 如果当前表单缓存不为空，则将原表单覆盖
    const storageForm = storageFormInfo.content
    setFormContent(ls.form, storageForm)
    if (typeof useDraftCallback === 'function') useDraftCallback(ls.form)
  }
  ls.initForm = lodash.cloneDeep(ls.form)
  ls.isRegister = true
}

// 关闭记录本地缓存
function closeStore(ls) {
  ls.isRegister = false
  componentInit(ls)
}

// 为表单赋值
function setFormContent(form, storageForm) {
  for (const key in form) {
    form[key] = undefined
  }
  for (const key in storageForm) {
    form[key] = storageForm[key]
  }
}

// 保存表单
function saveFormToStorage(ls, form, type = ADD_FORM.TYPE.normal) {
  if (!ls.isRegister) return false
  const _form = form || ls.form
  try {
    storage.set(
      `${ADD_FORM.KEY_PREFIX}_${ls.key}`,
      {
        type: type,
        content: _form
      },
      ls.expired
    )
    return true
  } catch (error) {
    console.log('缓存错误信息', error)
    console.log('缓存', _form)
    return false
  }
}

function clearFormStorage(ls) {
  storage.remove(`${ADD_FORM.KEY_PREFIX}_${ls.key}`)
}

// 异常关闭
function abnormalClose(ls, crud) {
  if (!ls.isRegister) return
  const inEdit = crud.bStatus.cu > 0
  // 当在编辑中,并且初始表单内容与当前表单内容不一致时，将信息存为异常保存
  if (inEdit && !isObjectValueEqual(ls.initForm, ls.form)) {
    saveFormToStorage(ls, null, ADD_FORM.TYPE.browserClose)
  }
}

// 获取表单缓存
function getFormByStorage(key) {
  return storage.get(`${ADD_FORM.KEY_PREFIX}_${key}`)
}
