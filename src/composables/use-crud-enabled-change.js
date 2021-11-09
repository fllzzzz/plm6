import { ref } from 'vue'
import { enabledEnum } from '@enum-ms/common'
/**
 * crud中 更新状态改变
 * @param {*} CRUD CRUD
 * @param {*} crud crud
 * @param {*} editEnabled 修改方法
 * @returns
 */
export default function useCrudEnabledChange({ CRUD, crud, editEnabled }) {
  const enabledLoading = ref(false)
  const handleEnabledChange = async (data, labelField = 'name') => {
    try {
      enabledLoading.value = true
      data.enabledLoading = true
      await editEnabled({ id: data.id, enabled: data.enabled })
      let label = ''
      if (labelField) {
        if (Array.isArray(labelField)) {
          const _label = labelField.map(v => data[v])
          label = _label.join(' / ')
        } else {
          label = `“${data[labelField]}”`
        }
      }
      crud.notify(label + enabledEnum.VL[data.enabled] + '成功', data.enabled === enabledEnum.TRUE.V ? CRUD.NOTIFICATION_TYPE.SUCCESS : CRUD.NOTIFICATION_TYPE.WARNING)
    } catch (error) {
      console.log('使用状态', error)
      data.enabled = data.enabled === enabledEnum.TRUE.V ? enabledEnum.FALSE.V : enabledEnum.TRUE.V
    } finally {
      enabledLoading.value = false
      data.enabledLoading = false
    }
  }
  return { enabledLoading, handleEnabledChange }
}
