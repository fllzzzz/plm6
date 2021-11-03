import { watch, ref, nextTick } from 'vue'

export default function useDialogVisible({ emit, props, field = 'modelValue', closeHook }, callback) {
  // dlg显示控制
  const visible = ref(false)

  watch(
    () => props[field],
    (flag) => {
      visible.value = flag
      if (!flag) {
        // 关闭重置表单
        if (typeof callback === 'function') {
          nextTick(() => {
            callback()
          })
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
