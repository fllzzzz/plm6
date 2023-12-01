<template>
  <div>
    <div v-show="crud.searchToggle">
      <monomer-select
        ref="monomerSelectRef"
        v-model="query.monomerId"
        :project-id="props.projectId"
        class="filter-item"
        :default="false"
        clearable
        @change="crud.toQuery"
        @getCurrentInfo="handleCurrent"
        @getAreaInfo="getAreaInfo"
      />
      <template v-if="query.monomerId">
        <template v-if="areaInfo && areaInfo.length>0">
          <common-select
            v-model="query.areaId"
            :options="areaInfo"
            type="other"
            clearable
            :dataStructure="{ key: 'id', label: 'name', value: 'id' }"
            size="small"
            placeholder="请选择区域"
            class="filter-item"
            style="width:200px;"
            @change="areaChange"
          />
        </template>
        <template v-else>
          <span style="font-size:12px;color:red;margin-left:10px;">*当前单体下未创建区域</span>
        </template>
      </template>
      <div>
        <common-select
          type="enum"
          v-model="query.boolReturn"
          :options="whetherEnum.ENUM"
          clearable
          placeholder="是否退量"
          style="width: 200px"
          class="filter-item"
          @change="crud.toQuery"
        />
        <el-input
          v-model="query.name"
          size="small"
          placeholder="名称"
          style="width: 170px; margin-left: 0"
          class="filter-item"
          clearable
        />
        <el-input
          v-model="query.specification"
          size="small"
          placeholder="规格"
          style="width: 170px"
          class="filter-item"
          clearable
        />
        <el-input
          v-model="query.remark"
          size="small"
          placeholder="备注"
          style="width: 170px"
          class="filter-item"
          clearable
        />
        <rrOperation />
      </div>
    </div>
    <crudOperation :disabled="!query.areaId">
       <template #optRight>
        <upload-btn
          v-if="crud.query.projectId && checkPermission(crud.permission.import)"
          :data="uploadQuery"
          :upload-fun="standardPartUpload"
          btn-name="清单导入"
          btn-type="primary"
          btn-size="mini"
          class="filter-item"
          :disabled="!query.areaId"
          @success="crud.toQuery()"
        />
        <export-button :fn="downloadStandardPart" class="filter-item" v-permission="crud.permission.templateDownload">
          清单模板
        </export-button>
      </template>
    </crudOperation>
  </div>
</template>

<script setup>
import { standardPartUpload, downloadStandardPart } from '@/api/plan/technical-manage/standard-part'
import { defineProps, ref, watch, defineEmits, computed } from 'vue'
import { regHeader } from '@compos/use-crud'
import checkPermission from '@/utils/system/check-permission'

import { whetherEnum } from '@enum-ms/common'

import crudOperation from '@crud/CRUD.operation'
import monomerSelect from '@/components-system/plan/monomer-select'
import rrOperation from '@crud/RR.operation'
import uploadBtn from '@comp/file-upload/ExcelUploadBtn'
import ExportButton from '@comp-common/export-button/index.vue'

const defaultQuery = {
  // classifyName: '',
  // serialNumber: '',
  monomerId: { value: undefined, resetAble: false },
  areaId: { value: undefined, resetAble: false },
  projectId: { value: undefined, resetAble: false },
  name: undefined,
  specification: undefined,
  remark: undefined,
  boolReturn: undefined
}

const monomerSelectRef = ref()
const emit = defineEmits(['currentChange', 'currentAreaChange'])
const areaInfo = ref([])
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
      query.monomerId = undefined
      query.areaId = undefined
      areaInfo.value = []
      crud.toQuery()
    }
  },
  { immediate: true, deep: true }
)

function handleCurrent(val) {
  emit('currentChange', val)
  crud.toQuery()
}

function areaChange(val) {
  const findVal = areaInfo.value.find(v => v.id === val) || {}
  emit('currentAreaChange', findVal)
  crud.toQuery()
}

function getAreaInfo(val) {
  areaInfo.value = val || []
  // query.areaId = areaInfo.value[0]?.id
  areaChange(query.areaId)
}
</script>
