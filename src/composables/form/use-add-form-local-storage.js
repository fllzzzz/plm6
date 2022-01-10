import { onBeforeUnmount, watch, reactive, provide } from 'vue'
import storage from '@/utils/storage'
import { isNotBlank, isBlank } from '@data-type/index'
import { isObjectValueEqual } from '@data-type/object'
import * as lodash from 'lodash'

// TODO: 版本更新后的表单变更缓存问题
// TODO: 考虑将异常与主动保存为草稿分开，在异常草稿时，给与用户提示

const ADD_FORM = {}

// 缓存key的前缀
ADD_FORM.KEY_PREFIX = 'ADD_FORM'
ADD_FORM.TYPE = {
  normal: 1, // 正常保存
  browserClose: 2 // 浏览器关闭保存
}

/**
 * 使用本地缓存
 * @param {string} key 本地缓存key值
 * @param {object} pendingForm 待处理表单
 * @param {*} trigger 触发器,适用于添加在Dialog/Drawer(弹窗)中的情景
 * @returns
 */
export default function useAddFormLocalStorage(key, pendingForm, trigger, { useDraftCallback, clearDraftCallback } = {}) {
  const ls = reactive({
    key: key,
    expired: 604800000,
    form: pendingForm, // 待处理的表单
    initForm: undefined, // 初始的表单
    init: undefined, // 初始化方法
    isRegister: false, // 是否注册的
    saveStoreForm: () => {
      return saveFormToStorage(ls)
    },
    resetForm: () => {
      clearFormStorage(ls)
      // 清除内容
      ls.form.length = 0
      ls.init && ls.init()
      if (typeof clearDraftCallback === 'function') clearDraftCallback()
    }
  })

  // 如果不传入trigger，则通过openStore手动开启
  if (isNotBlank(trigger)) {
    let _trigger
    switch (trigger.constructor.name) {
      case 'Function':
      case 'RefImpl':
      case 'ComputedRefImpl':
        _trigger = trigger
        break
      default:
        _trigger = trigger
    }
    // 传入触发器的情况下监听弹窗打开和关闭的状态
    watch(_trigger, (flag) => {
      if (flag) {
        openStore(ls, useDraftCallback)
      } else {
        closeStore(ls, useDraftCallback)
      }
    })
  } else {
    openStore(ls, useDraftCallback)
  }

  // 卸载时判断是否需要记录
  onBeforeUnmount(() => {
    abnormalClose(ls, trigger)
    closeStore(ls)
  })

  // 浏览器关闭
  window.onbeforeunload = () => {
    abnormalClose(ls, trigger)
  }

  provide('nfmStore', ls)

  return {
    ADD_FORM: ls,
    openStore: (form) => {
      if (form) ls.form = form
      openStore(ls, useDraftCallback)
    },
    closeStore: () => closeStore(ls),
    saveStoreForm: (form) => {
      return saveFormToStorage(ls, form)
    },
    clearFormStorage: () => clearFormStorage(ls)
  }
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
function abnormalClose(ls, trigger) {
  if (!ls.isRegister) return
  let inEdit = true
  if (isNotBlank(trigger)) {
    inEdit = typeof trigger === 'function' ? trigger() : trigger.value
  }
  // 当在编辑中,并且初始表单内容与当前表单内容不一致时，将信息存为异常保存
  if (inEdit && !isObjectValueEqual(ls.initForm, ls.form)) {
    saveFormToStorage(ls, null, ADD_FORM.TYPE.browserClose)
  }
}

// 获取表单缓存
function getFormByStorage(key) {
  return storage.get(`${ADD_FORM.KEY_PREFIX}_${key}`)
}
