import { getByType } from '@/api/config/system-config/table-print-template'

import printTemplate from '@/utils/print/default-template'

export default async function useDefaultTableTemplate(tableType) {
  let defaultConfig
  try {
    const { content = [] } = await getByType(tableType)
    content.forEach(v => {
      if (v.isDefault && v.config) {
        defaultConfig = JSON.parse(v.config)
      }
    })
  } catch (error) {
    console.log('表格模板', error)
  }

  return defaultConfig || printTemplate[tableType]
}
