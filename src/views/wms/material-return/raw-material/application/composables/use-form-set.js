import useTableValidate from '@/composables/form/use-table-validate'
import { uniqueArr } from '@/utils/data-type/array'
import { numFmtByBasicClass } from '@/utils/wms/convert-unit'
import { rawMatClsEnum } from '@/utils/enum/modules/classification'

import { ElMessage } from 'element-plus'
import { ref } from 'vue'

export default function useFormSet({ FORM, form, cu, emit, isEdit, tableRules, init, setFormCallback }) {
  // 同上数据
  const ditto = new Map([
    ['factoryId', -1],
    ['warehouseId', -1]
  ])

  // 初始退库列表id，修改时使用，在退库列表中使用，避免显示
  const sourceReturnIds = ref([])

  // 表格校验
  const { tableValidate, wrongCellMask, cleanUpData } = useTableValidate({
    rules: tableRules,
    ditto,
    errorMsg: '请修正【退库清单】中标红的信息'
  })

  // 表单提交数据清理
  cu.submitFormFormat = async (form) => {
    form.list.map(v => {
      if ((v.basicClass & rawMatClsEnum.STEEL_PLATE.V) && v.list?.length > 0) {
        cleanUpData(v.list)
      }
      return v
    })
    cleanUpData(form.list)
    form.list.map(async (v) => {
      if ((v.basicClass & rawMatClsEnum.STEEL_PLATE.V) && v.list?.length > 0) {
        v.list = await numFmtByBasicClass(v.list, { toSmallest: true, toNum: true })
      }
    })
    form.list = await numFmtByBasicClass(form.list, { toSmallest: true, toNum: true })
    if (isEdit) {
      return form
    } else {
      return form.list
    }
  }

  FORM.HOOK.beforeToEdit = async (crud, form) => {
    if (!isEdit) return
    // 设置监听等
    setFormCallback(form)
    sourceReturnIds.value = uniqueArr(form.list.map(row => row.source.id))
  }

  // 校验
  FORM.HOOK.beforeValidateCU = (crud, form) => {
    return validate()
  }

  // 提交后清除校验结果
  FORM.HOOK.afterSubmit = () => {
    if (isEdit) {
      emit('success')
    }
    init()
  }

  // 校验
  function validate() {
    const { validResult, dealList } = tableValidate(form.list)
    form.list = dealList
    if (validResult) {
      return validateOverSource()
    }
    return false
  }

  // 校验是否超过原材料可退库最大值
  function validateOverSource() {
    const hasOver = form.list.some((v) => v.overTipColor)
    if (hasOver) {
      ElMessage.error('请修正序号前带“三角形标志”的数据，他们的合计量，超过了可退库的总量')
    }
    return !hasOver
  }

  return {
    wrongCellMask,
    sourceReturnIds
  }
}
