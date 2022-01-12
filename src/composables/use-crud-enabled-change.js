import { ref } from 'vue'
import { enabledEnum } from '@enum-ms/common'
/**
 * crud中 更新状态改变
 * @param {*} CRUD CRUD
 * @param {*} crud crud
 * @param {*} editEnabled 修改方法
 * @returns
 */
export default function useCrudEnabledChange(
  { CRUD, crud, editEnabled },
  { enabledField = 'enabled', enumObj = enabledEnum, refresh = true, t = 'TRUE', f = 'FALSE' } = {}
) {
  const enabledLoading = ref(false)
  const handleEnabledChange = async (data, labelField = 'name') => {
    try {
      enabledLoading.value = true
      data.enabledLoading = true
      const params = {
        id: data.id
      }
      params[enabledField] = data[enabledField]
      await editEnabled(params)
      let label = ''
      if (labelField) {
        if (Array.isArray(labelField)) {
          const _label = labelField.map((v) => data[v])
          label = _label.join(' / ')
        } else {
          label = `“${data[labelField]}”`
        }
      }
      crud.notify(
        label + enumObj.VL[data[enabledField]],
        data[enabledField] === enumObj[t].V ? CRUD.NOTIFICATION_TYPE.SUCCESS : CRUD.NOTIFICATION_TYPE.WARNING
      )
      if (refresh) {
        crud.refresh()
      }
    } catch (error) {
      console.log('使用状态', error)
      data[enabledField] = data[enabledField] === enumObj[t].V ? enumObj[f].V : enumObj[t].V
    } finally {
      enabledLoading.value = false
      data.enabledLoading = false
    }
  }
  return { enabledLoading, handleEnabledChange }
}
