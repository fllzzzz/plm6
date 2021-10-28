import { watch, ref, nextTick } from 'vue'

export default function useDialogVisible(emit, props, callback) {
  // dlg显示控制
  const dialogVisible = ref(false)

  watch(
    () => props.modelValue,
    (flag) => {
      dialogVisible.value = flag
      if (!flag) {
        // 关闭重置表单
        nextTick(() => {
          callback()
        })
      }
    }
  )

  // 关闭dlg
  const handleClose = () => {
    emit('update:modelValue', false)
  }

  return {
    dialogVisible,
    handleClose
  }
}
