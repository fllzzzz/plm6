import { batchAdd } from '@/api/config/classification-manage/section-steel-spec-config-detail'
/**
 * 可在模板中增加format来转换格式
 * TODO:字段是否设置数据类型，即string/number
 */

import { deepClone } from '@/utils/data-type'
import { ElNotification } from 'element-plus'

const sectionSteelSpecTmpl = {
  title: '型材规格导入', // 表格名称
  dlgWidth: '500px', // 预览窗口宽度
  startRow: 2, // 起始行
  // 解析字段
  fields: [
    { label: '规格', field: 'specification', excelField: '__EMPTY' },
    { label: '单位净量(kg/m)', field: 'unitNet', excelField: '__EMPTY_1' }
  ],
  // 校验规则
  rules: {
    specification: [{ required: true, max: 20, message: '不能超过50个字符' }],
    unitNet: [{ required: true, min: 1, message: '重量最小为1' }]
  },
  // 提交前的数据格式转换
  submitFormat: (tableList, standard) => {
    const list = deepClone(tableList)
    list.forEach((v) => {
      v.standard = [
        {
          id: standard.id,
          unitNet: v.unitNet
        }
      ]
      delete v.unitNet
    })
    return list
  },
  // 将解析结果提交给服务端
  submit: async (tableList, sectionSteel, standard) => {
    const list = sectionSteelSpecTmpl.submitFormat(tableList, standard)
    await batchAdd({ sectionSteelId: sectionSteel.id, list })
    ElNotification({ title: `${sectionSteel.name}-${standard.name}，规格导入成功`, type: 'success' })
  }
}

export default sectionSteelSpecTmpl
