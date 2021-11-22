import { watch } from 'vue'
import { mapGetters } from '@/store/lib'

export default function useGlobalProjectIdChangeToQuery(crud, field = 'projectId') {
  const { globalProjectId } = mapGetters('globalProjectId')
  watch(
    globalProjectId,
    (val) => {
      crud.query[field] = val
      crud.toQuery()
    },
    { immediate: true }
  )
}
