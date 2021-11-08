import { batchAdd } from '@/api/config/classification-manage/section-steel-spec-config-detail'
/**
 * 可在模板中增加format来转换格式
 */

import { deepClone } from '@/utils/data-type'
import { ElNotification } from 'element-plus'

const sectionSteelSpecTmpl = {
  title: '型材规格导入',
  dlgWidth: '500px',
  startRow: 2, // 起始行
  fields: [
    { label: '规格', field: 'specification', excelField: '__EMPTY' },
    { label: '单位净量(kg/m)', field: 'unitNet', excelField: '__EMPTY_1' }
  ],
  rules: {
    specification: [{ required: true, max: 20, message: '不能超过50个字符' }],
    unitNet: [{ required: true, min: 1, message: '重量最小为1' }]
  },
  submitFormat: (tableList, standard) => {
    const list = deepClone(tableList)
    list.forEach((v) => {
      v.standard = [{
        id: standard.id,
        unitNet: v.unitNet
      }]
      delete v.unitNet
    })
    return list
  },
  submit: async (tableList, sectionSteel, standard) => {
    const list = sectionSteelSpecTmpl.submitFormat(tableList, standard)
    await batchAdd({ sectionSteelId: sectionSteel.id, list })
    ElNotification({ title: `${sectionSteel.name}-${standard.name}，规格导入成功`, type: 'success' })
  }
}

export default sectionSteelSpecTmpl
