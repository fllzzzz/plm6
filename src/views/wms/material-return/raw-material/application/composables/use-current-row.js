import { ref, watch } from 'vue'
export default function useCurrentRow({ form, tableRef, delCallback }) {
  // 当前退库源数据
  const currentSource = ref()
  // 当前高亮uid
  const currentUid = ref()

  // 监听list
  watch(
    () => form.list,
    () => {
      setDefaultCurrent()
    },
    { deep: true }
  )

  // 行选中
  function handleRowClick(row, column, event) {
    currentUid.value = row.uid
    currentSource.value = row.source
  }

  // 删除当前行
  function delRow(row, index) {
    form.list.splice(index, 1)
    // 删除后计算是否超出，回调
    if (typeof delCallback === 'function') delCallback(row)
    if (row.uid === currentUid.value || !currentUid.value) {
      setTimeout(() => {
        if (form.list.length > 0) {
          // delRow后会触发handleRowClick,因此延迟触发
          const newCurrent = index < form.list.length ? form.list[index] : form.list[index - 1]
          tableRef.value.setCurrentRow(newCurrent)
          handleRowClick(newCurrent)
        } else {
          // 数据为空时，选中设置为null
          currentUid.value = null
          currentSource.value = null
        }
      }, 0)
    }
  }

  // 设置默认选中行
  function setDefaultCurrent() {
    if (form.list.length > 0 && !currentUid.value) {
      // 选中第一行
      const newCurrent = form.list[0]
      tableRef.value.setCurrentRow(newCurrent)
      handleRowClick(newCurrent)
    }
  }

  return {
    currentSource,
    currentUid,
    delRow,
    setDefaultCurrent,
    handleRowClick
  }
}
