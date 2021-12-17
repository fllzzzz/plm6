import { watch } from 'vue'
import { mapGetters } from '@/store/lib'
import { projectWarehouseTypeEnum } from '@/utils/enum/modules/wms'

export default function useProjectChangeByWarehouseType({ crud, field = 'projectId' }) {
  // 全局项目id
  const { globalProjectId } = mapGetters('globalProjectId')
  // 选中项目库时， 根据项目id的变化刷新列表
  watch(
    globalProjectId,
    (val) => {
      if (crud.query.projectWarehouseType === projectWarehouseTypeEnum.PROJECT.V) {
        crud.query[field] = val
        crud.toQuery()
      }
    },
    { immediate: true }
  )
}
