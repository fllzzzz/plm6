import { watch, ref } from 'vue'

export default function useVisible({ emit, props, field = 'modelValue', closeHook, showHook }) {
  // dlg显示控制
  const visible = ref(false)

  watch(
    () => props[field],
    (flag) => {
      visible.value = flag
      if (flag) {
        // 关闭重置表单
        if (typeof showHook === 'function') {
          showHook()
        }
      }
    }
  )

  // 关闭dlg
  const handleClose = () => {
    if (typeof closeHook === 'function')closeHook()
    emit(`update:${[field]}`, false)
  }

  return {
    visible,
    handleClose
  }
}
