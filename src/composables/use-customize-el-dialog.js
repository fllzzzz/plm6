import { computed } from 'vue'

export default function useCustomizeElDialog(emit, props) {
  // dlg显示控制
  const dialogVisible = computed(() => props.modelValue)

  // 关闭dlg
  const handleClose = () => {
    emit('update:modelValue', false)
  }

  return {
    dialogVisible,
    handleClose
  }
}
