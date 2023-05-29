<template>
  <div>
    <div v-show="crud.searchToggle">
    </div>
    <crudOperation>
       <template #optRight>
        <upload-btn
          v-if="crud.query.projectId && checkPermission(crud.permission.import)"
          :data="uploadQuery"
          :upload-fun="enclosureStandardPartUpload"
          btn-name="清单导入"
          btn-type="primary"
          btn-size="mini"
          class="filter-item"
          @success="crud.toQuery()"
        />
        <export-button :fn="enclosureDownloadStandardPart" class="filter-item" v-permission="crud.permission.templateDownload">
          清单模板
        </export-button>
      </template>
    </crudOperation>
  </div>
</template>

<script setup>
import { enclosureStandardPartUpload, enclosureDownloadStandardPart } from '@/api/plan/technical-manage/standard-part'
import { defineProps, watch, computed } from 'vue'
import { regHeader } from '@compos/use-crud'
import checkPermission from '@/utils/system/check-permission'

import crudOperation from '@crud/CRUD.operation'
// import monomerSelect from '@/components-system/plan/monomer-select'
// import rrOperation from '@crud/RR.operation'
import uploadBtn from '@comp/file-upload/ExcelUploadBtn'
import ExportButton from '@comp-common/export-button/index.vue'

const defaultQuery = {
  projectId: { value: undefined, resetAble: false }
}

// const monomerSelectRef = ref()
// const emit = defineEmits(['currentChange', 'currentAreaChange'])
// const areaInfo = ref([])
const { crud, query } = regHeader(defaultQuery)
const props = defineProps({
  projectId: {
    type: [Number, String],
    default: undefined
  }
})
const uploadQuery = computed(() => {
  const obj = {}
  for (const i in query) {
    if (query[i]) {
      obj[i] = query[i]
    }
  }
  return obj
})

watch(
  () => props.projectId,
  (val) => {
    if (val) {
      crud.query.projectId = props.projectId
      crud.toQuery()
    }
  },
  { immediate: true, deep: true }
)

// function handleCurrent(val) {
//   emit('currentChange', val)
// }

// function areaChange(val) {
//   const findVal = areaInfo.value.find(v => v.id === val) || {}
//   emit('currentAreaChange', findVal)
//   crud.toQuery()
// }

// function getAreaInfo(val) {
//   areaInfo.value = val || []
// }
</script>
