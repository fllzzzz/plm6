<template>
  <div>
    <div v-show="crud.searchToggle">
      <monomer-select
        ref="monomerSelectRef"
        v-model="query.monomerId"
        :project-id="props.projectId"
        class="filter-item"
        @getAreaInfo="getAreaInfo"
      />
      <area-tabs
        class="filter-item"
        style="width: calc(100% - 230px);"
        v-model="query.areaId"
        :area-info="areaInfo"
        :default-tab="defaultTab"
        @tab-click="tabClick"
      />
      <el-input
        v-model="query.artifactName"
        size="small"
        placeholder="输入构件名称搜索"
        style="width: 170px;margin-left:0;"
        class="filter-item"
        clearable
        @blur="crud.toQuery"
      />
      <el-input
        v-model="query.artifactSerialNumber"
        size="small"
        placeholder="输入构件编号搜索"
        style="width: 170px;"
        class="filter-item"
        clearable
        @blur="crud.toQuery"
      />
      <el-input
        v-model="query.machinePartSerialNumber"
        size="small"
        placeholder="输入零件编号搜索"
        style="width: 170px;"
        class="filter-item"
        clearable
        @blur="crud.toQuery"
      />
    </div>
    <crudOperation>
      <template #optLeft>
        <upload-btn
          v-if="currentArea && currentArea.id"
          v-permission="crud.permission.importList"
          :data="{ areaId: crud.query.areaId }"
          :upload-fun="listUpload"
          btn-name="零构件清单导入"
          btn-type="primary"
          btn-size="mini"
          class="filter-item"
          @success="crud.toQuery"
        />
        <export-button
          v-if="currentArea && currentArea.id"
          :fn="downloadArtifactTree"
          :params="carryParam"
          show-btn-text
          btn-text="零构件清单（按构件条件查询）"
          class="filter-item"
        />
        <export-button
          :fn="downloadArtifactTreeTemplate"
          show-btn-text
          btn-text="零构件清单模板"
          class="filter-item"
        />
      </template>
    </crudOperation>
  </div>
</template>

<script setup>
import { defineProps, ref, computed, watch } from 'vue'
import { useRouter } from 'vue-router'
import { regHeader } from '@compos/use-crud'
import crudOperation from '@crud/CRUD.operation'
import monomerSelect from '@/components-system/plan/monomer-select'
import areaTabs from '@/components-system/plan/area-tabs'
import uploadBtn from '@/components/file-upload/ExcelUploadBtn'
import { monomerDetail } from '@/api/plan/monomer'
import { listUpload } from '@/api/plan/technical-manage/artifact-tree'
import ExportButton from '@comp-common/export-button/index.vue'
import { downloadArtifactTree, downloadArtifactTreeTemplate } from '@/api/plan/technical-manage/artifact-tree'

const router = useRouter()

const defaultQuery = {
  artifactName: '', 
  artifactSerialNumber: '', 
  machinePartSerialNumber: '',
  monomerId: { value: undefined, resetAble: false },
  areaId: { value: undefined, resetAble: false },
  projectId: { value: undefined, resetAble: false }
}

const monomerSelectRef = ref()
const currentArea = ref({})
const areaInfo = ref([])
const defaultTab = ref({})
const { crud, query } = regHeader(defaultQuery)
const typeProp = { key: 'no', label: 'name', value: 'no' }
const typeOption = ref([])
const props = defineProps({
  projectId: {
    type: [Number, String],
    default: undefined
  }
})

watch(
  () => props.projectId,
  (val) => {
    if (val) {
      crud.query.projectId = props.projectId
      crud.toQuery()
    }
  },
  { immediate: true }
)
const carryParam = computed(()=>{
  const param = { ...crud.query }
  delete param.machinePartSerialNumber
  return param
})

function tabClick(val) {
  const { name, label } = val
  currentArea.value = {
    id: name,
    name: label
  }
  crud.toQuery()
}
function getAreaInfo(val) {
  areaInfo.value = val || []
  if (areaInfo.value.length > 0) {
    defaultTab.value = {
      id: areaInfo.value[0].id + '',
      name: areaInfo.value[0].name
    }
  } else {
    defaultTab.value = {}
  }
}

</script>
